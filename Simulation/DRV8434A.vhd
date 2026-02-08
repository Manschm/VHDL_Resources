-- drv8434a_emu.vhd
-- Non-synthesizable DRV8434A emulator (VHDL-2008)
--
-- Notes:
--  * M0/M1 support 'Z' (Hi-Z) and 'L' (weak-0) to represent "330k to GND" options from the datasheet table.
--  * nFAULT and STL_REP are modeled as open-drain outputs but can include internal pullups
--    (INTERNAL_PULLUPS generic) so a plain std_logic input sees '1' instead of 'Z'.
--  * Motor is simplified to a single position number:
--        motor_pos_steps = full-step units (fractional when microstepping)
--        motor_pos_q256  = fixed-point position in steps*256 (integer)
--
--  * By default, motor position only advances when outputs are enabled (ENABLE /= 0 and ready and not faulted).
--    Set COUNT_WHEN_DISABLED => true to count STEP edges even when ENABLE=0.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity drv8434a_emu is
  generic (
    -- From datasheet (typical / limits); tweak for your testbench needs:
    T_SLEEP      : time := 120 us;  -- nSLEEP low -> sleep after this
    T_WAKE       : time := 1 ms;    -- nSLEEP high -> ready after this (typ ~0.8..1.2ms)
    T_RESET_MIN  : time := 20 us;   -- nSLEEP low pulse width to clear faults (min)
    T_RESET_MAX  : time := 40 us;   -- nSLEEP low pulse width to clear faults (max)

    COUNT_WHEN_DISABLED : boolean := false;

    -- If true, emulate external pullups so outputs read as '1' when released (instead of 'Z').
    INTERNAL_PULLUPS    : boolean := true;

    -- If true, a stall (when enabled) also asserts nFAULT low (matches typical "stall fault" reporting usage).
    STALL_ASSERTS_NFAULT : boolean := true
  );
  port (
    -- Control interface (FPGA typically connects here)
    STEP     : in  std_logic;
    DIR      : in  std_logic;
    ENABLE   : in  std_logic;
    nSLEEP   : in  std_logic;

    -- Microstep mode pins
    M0       : in  std_logic;
    M1       : in  std_logic;

    -- Stall detection mode pin (optional for many FPGA designs)
    --  '0' = torque count mode, '1' = stall threshold mode, 'Z' = learning mode, 'L' = 330k to GND (disabled)
    STL_MODE : in  std_logic := '0';

    -- Simple runtime fault injection hooks (testbench convenience)
    INJ_FAULT : in std_logic := '0'; -- rising to '1' latches a generic fault until cleared by nSLEEP reset pulse
    INJ_STALL : in std_logic := '0'; -- when '1', motor is considered stalled (position won't change)

    -- Open-drain outputs (modeled)
    nFAULT  : out std_logic;
    STL_REP : out std_logic;

    -- Simplified motor output
    motor_pos_steps : out real;     -- full-step units (fractional in microstep modes)
    motor_pos_q256  : out integer;  -- fixed-point: steps * 256

    -- Debug/visibility
    step_mode_div   : out integer;  -- 1,2,4,...,256
    ready_o         : out std_logic -- '1' when device is awake/ready
  );
end entity;

architecture sim of drv8434a_emu is

  type t_pin is (PIN0, PIN1, PINZ, PINL, PINH, PINX);

  function classify(s : std_logic) return t_pin is
  begin
    case s is
      when '0' => return PIN0;
      when '1' => return PIN1;
      when 'Z' => return PINZ;
      when 'L' => return PINL;
      when 'H' => return PINH;
      when others => return PINX;
    end case;
  end function;

  -- Inputs with internal pulldown (DIR, STEP, nSLEEP per datasheet)
  function eff_pulldown(s : std_logic) return std_logic is
  begin
    case s is
      when '1' | 'H' => return '1';
      when others    => return '0'; -- includes '0','L','Z','X','U',...
    end case;
  end function;

  -- ENABLE: treat '0'/'L' as disabled, anything else as enabled (incl. Hi-Z).
  function eff_enable(s : std_logic) return std_logic is
  begin
    case s is
      when '0' | 'L' => return '0';
      when others    => return '1';
    end case;
  end function;

  -- Microstep divisor from datasheet Table 7-2 using M0/M1 tristate / weak-0.
  function microstep_div(m0, m1 : std_logic) return integer is
    variable a : t_pin := classify(m0);
    variable b : t_pin := classify(m1);
  begin
    if    (a = PIN0 and b = PIN0) then return 1;    -- Full step 100%
    elsif (a = PIN0 and b = PINL) then return 1;    -- Full step 71% (330k to GND)
    elsif (a = PIN1 and b = PIN0) then return 2;    -- Non-circular 1/2 step
    elsif (a = PINZ and b = PIN0) then return 2;    -- 1/2 step
    elsif (a = PIN0 and b = PIN1) then return 4;    -- 1/4
    elsif (a = PIN1 and b = PIN1) then return 8;    -- 1/8
    elsif (a = PINZ and b = PIN1) then return 16;   -- 1/16
    elsif (a = PIN0 and b = PINZ) then return 32;   -- 1/32
    elsif (a = PINZ and b = PINL) then return 64;   -- 1/64 (M1 = 330k to GND)
    elsif (a = PINZ and b = PINZ) then return 128;  -- 1/128
    elsif (a = PIN1 and b = PINZ) then return 256;  -- 1/256
    else
      return 1; -- default fallback for illegal/unknown combos
    end if;
  end function;

  -- Internal state
  signal sleeping      : boolean := true;
  signal ready         : boolean := false;

  signal fault_latched : boolean := false;
  signal stall_active  : boolean := false;

  signal pos_q256_s    : integer := 0;

  signal div_s         : integer := 1;

  signal nfault_od     : std_logic := 'Z';
  signal stlrep_od     : std_logic := 'Z';

begin

  -- Decode mode continuously
  div_s <= microstep_div(M0, M1);

  -- Export simplified motor position
  motor_pos_q256  <= pos_q256_s;
  motor_pos_steps <= real(pos_q256_s) / 256.0;

  step_mode_div <= div_s;
  ready_o       <= '1' when ready else '0';

  -- Stall input (not latched here; you can make it latched externally if desired)
  stall_active <= (eff_pulldown(INJ_STALL) = '1');

  ---------------------------------------------------------------------------
  -- nSLEEP behavior: sleep entry delay, wake delay, and reset pulse fault clear
  ---------------------------------------------------------------------------
  p_nsleep : process
    variable t_low_start : time;
    variable t_low       : time;
    variable ns_eff       : std_logic;
  begin
    -- Initial state based on nSLEEP level
    ns_eff := eff_pulldown(nSLEEP);
    if ns_eff = '0' then
      sleeping <= true;
      ready    <= false;
      -- Wait until device is brought out of sleep
      wait until (nSLEEP'event and eff_pulldown(nSLEEP) = '1');
      sleeping <= false;
      ready    <= false;
      ready    <= true after T_WAKE;
    else
      sleeping <= false;
      ready    <= false;
      ready    <= true after T_WAKE;
    end if;

    loop
      -- Wait for nSLEEP going low
      wait until (nSLEEP'event and eff_pulldown(nSLEEP) = '0');
      t_low_start := now;
      ready       <= false;

      -- Wait for rise OR timeout at T_SLEEP
      wait until (nSLEEP'event and eff_pulldown(nSLEEP) = '1') for T_SLEEP;

      if eff_pulldown(nSLEEP) = '0' then
        -- Still low after T_SLEEP -> in sleep mode
        sleeping <= true;
        wait until (nSLEEP'event and eff_pulldown(nSLEEP) = '1');
      end if;

      t_low := now - t_low_start;

      -- Clear faults if reset pulse width is in window
      if (t_low >= T_RESET_MIN) and (t_low <= T_RESET_MAX) then
        fault_latched <= false;
      end if;

      -- Exit sleep / resume operation
      sleeping <= false;

      if t_low >= T_SLEEP then
        -- If it actually entered sleep, enforce wake delay
        ready <= false;
        ready <= true after T_WAKE;
      else
        -- Reset pulse (short) should not require wake delay
        ready <= true;
      end if;
    end loop;
  end process;

  ---------------------------------------------------------------------------
  -- Fault injection latch (cleared by nSLEEP reset pulse above)
  ---------------------------------------------------------------------------
  p_fault_inj : process
  begin
    if eff_pulldown(INJ_FAULT) = '1' then
      fault_latched <= true;
    end if;

    loop
      wait until INJ_FAULT'event;
      if eff_pulldown(INJ_FAULT) = '1' then
        fault_latched <= true;
      end if;
    end loop;
  end process;

  ---------------------------------------------------------------------------
  -- STEP/DIR -> motor position accumulator
  ---------------------------------------------------------------------------
  p_step : process
    variable prev_step : std_logic := '0';
    variable cur_step  : std_logic;

    variable inc_q256  : integer;
    variable div_now   : integer;

    variable dir_eff   : std_logic;
    variable en_eff    : std_logic;

    variable do_count  : boolean;
  begin
    prev_step := eff_pulldown(STEP);

    loop
      wait until STEP'event;
      cur_step := eff_pulldown(STEP);

      -- Rising edge detect on effective STEP level
      if (prev_step = '0') and (cur_step = '1') then
        div_now  := microstep_div(M0, M1);
        inc_q256 := 256 / div_now; -- exact for allowed divisors

        dir_eff := eff_pulldown(DIR);
        en_eff  := eff_enable(ENABLE);

        -- "Motor moves" only when awake/ready, not sleeping, not faulted.
        -- If stalled, motor position is frozen (driver may still step internally).
        do_count := ready and (not sleeping) and (not fault_latched) and (not stall_active);

        if (not COUNT_WHEN_DISABLED) then
          do_count := do_count and (en_eff = '1');
        end if;

        if do_count then
          if dir_eff = '1' then
            pos_q256_s <= pos_q256_s + inc_q256;
          else
            pos_q256_s <= pos_q256_s - inc_q256;
          end if;
        end if;
      end if;

      prev_step := cur_step;
    end loop;
  end process;

  ---------------------------------------------------------------------------
  -- Open-drain outputs: nFAULT and STL_REP
  --   - nFAULT pulls low on fault (and optionally on stall)
  --   - STL_REP pulls low when "no stall" in torque/stall-threshold modes,
  --     releases (goes high via pullup) when stall is active.
  ---------------------------------------------------------------------------
  p_outputs : process(all)
    variable stlm : t_pin;
    variable stall_enabled : boolean;
  begin
    stlm := classify(STL_MODE);

    -- Stall detection disabled if STL_MODE is "330k to GND" (use 'L' in sim)
    stall_enabled := (stlm /= PINL);

    -- nFAULT open drain
    if sleeping then
      nfault_od <= 'Z';
    else
      if fault_latched then
        nfault_od <= '0';
      elsif STALL_ASSERTS_NFAULT and stall_enabled and stall_active then
        nfault_od <= '0';
      else
        nfault_od <= 'Z';
      end if;
    end if;

    -- STL_REP open drain behavior
    if sleeping then
      stlrep_od <= 'Z';
    else
      if (stlm = PIN0 or stlm = PIN1) and stall_enabled then
        -- In torque-count or stall-threshold mode: low = no stall, released = stall
        if stall_active then
          stlrep_od <= 'Z';
        else
          stlrep_od <= '0';
        end if;
      elsif (stlm = PINZ) and stall_enabled then
        -- Learning mode: high until "learning successful" (not modeled -> keep released)
        stlrep_od <= 'Z';
      else
        stlrep_od <= 'Z';
      end if;
    end if;
  end process;

  -- Drive open-drain nets + optional internal pullups (2 drivers per signal; resolved by std_logic)
  nFAULT  <= nfault_od;
  STL_REP <= stlrep_od;

  nFAULT  <= '1' when INTERNAL_PULLUPS else 'Z';
  STL_REP <= '1' when INTERNAL_PULLUPS else 'Z';

end architecture;
