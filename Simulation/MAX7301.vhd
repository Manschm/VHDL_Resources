-- !!! This code was generated using an LLM. !!!
-- max7301_model.vhd
-- Behavioral (non-synthesizable) MAX7301 emulation, VHDL-2008
--
-- SPI: Mode 0 style (sample DIN on SCLK rising, update DOUT on SCLK falling)
-- Protocol: 16-bit word latched on CS rising:
--   D15..D8 = command byte, with D15 = R/W, D14..D8 = 7-bit register address
--   D7..D0  = data byte
--
-- Read behavior: after a READ command is latched, the model loads the addressed
-- register value into the *low byte* of the internal 16-bit shift register.
-- During the *next* 16 clocks, the register value appears in the last 8 bits shifted out.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity max7301_model is
  generic (
    -- You can shrink this for a "20-port" style hookup (e.g. G_LAST_PORT => 23),
    -- but the SPI register map still exists; "missing pins" read as G_FLOAT_INPUT_LEVEL.
    G_FIRST_PORT         : integer := 4;
    G_LAST_PORT          : integer := 31;

    -- Optional delay for DOUT updates after SCLK falling edge
    G_TDO                : time    := 0 ns;

    -- What the model reads if an input pin is floating ('Z') without pullup
    G_FLOAT_INPUT_LEVEL  : std_logic := '0'
  );
  port (
    CS   : in  std_logic;
    SCLK : in  std_logic;
    DIN  : in  std_logic;
    DOUT : out std_logic;

    -- GPIO pins P4..P31 (or subset via generics). Resolved inout to allow TB drivers.
    P    : inout std_logic_vector(G_LAST_PORT downto G_FIRST_PORT)
  );
end entity;

architecture behav of max7301_model is
  type t_cfg_regs is array (0 to 6) of std_logic_vector(7 downto 0); -- 0x09..0x0F

  signal p_drv    : std_logic_vector(G_LAST_PORT downto G_FIRST_PORT) := (others => 'Z');
  signal dout_reg : std_logic := '0';
begin
  -- Basic sanity
  assert (G_FIRST_PORT >= 4 and G_LAST_PORT <= 31 and G_FIRST_PORT <= G_LAST_PORT)
    report "max7301_model: port range must be within P4..P31 and FIRST<=LAST"
    severity failure;

  -- Drive resolved pins and DOUT
  P    <= p_drv;
  DOUT <= dout_reg;

  main : process
    --------------------------------------------------------------------------
    -- Internal device state (variables => single-driver semantics, sim-only)
    --------------------------------------------------------------------------
    variable cfg_regs_v   : t_cfg_regs := (others => x"AA"); -- power-up: inputs w/o pullup ("10" pairs)
    variable config_m_v   : std_logic  := '0';               -- M bit (transition detect enable/switch)
    variable config_s_v   : std_logic  := '0';               -- S bit (1=normal, 0=shutdown)
    variable mask_v       : std_logic_vector(6 downto 0) := (others => '0'); -- P24..P30 masks (bit0=P24)
    variable port_lat_v   : std_logic_vector(31 downto 0) := (others => '0'); -- output latch
    variable int_lat_v    : std_logic := '0';               -- INT latch (drives P31 when M=1 and P31 output)
    variable snapshot_v   : std_logic_vector(6 downto 0) := (others => '0'); -- P24..P30 snapshot
    variable armed_v      : boolean := false;               -- one-shot arming flag
    variable sh_v         : std_logic_vector(15 downto 0) := (others => '0'); -- 16-bit shift register

    --------------------------------------------------------------------------
    -- Helpers
    --------------------------------------------------------------------------
    function has_pin(pnum : integer) return boolean is
    begin
      return (pnum >= G_FIRST_PORT) and (pnum <= G_LAST_PORT);
    end function;

    impure function sample_pin(pnum : integer) return std_logic is
      variable v : std_logic;
    begin
      if not has_pin(pnum) then
        return G_FLOAT_INPUT_LEVEL;
      end if;

      v := P(pnum);
      case v is
        when '0' | 'L' => return '0';
        when '1' | 'H' => return '1';
        when 'Z'       => return G_FLOAT_INPUT_LEVEL;
        when others    => return ieee.std_logic_1164.to_X01(v); -- may return 'X'
      end case;
    end function;

    impure function cfg_pair(pnum : integer) return std_logic_vector(1 downto 0) is
      variable reg_i : integer;
      variable idx   : integer;
      variable b     : std_logic_vector(7 downto 0);
    begin
      -- Only meaningful for P4..P31
      if (pnum < 4) or (pnum > 31) then
        return "10"; -- treat as input no pullup
      end if;

      reg_i := (pnum - 4) / 4;     -- 0..6 for 0x09..0x0F
      idx   := (pnum - 4) mod 4;   -- 0..3 within register
      b     := cfg_regs_v(reg_i);

      -- Pair mapping: P4 uses D1:D0, P5 uses D3:D2, P6 uses D5:D4, P7 uses D7:D6, etc.
      return b(2*idx+1) & b(2*idx);
    end function;

    impure function port_read_bit(pnum : integer) return std_logic is
      variable pair : std_logic_vector(1 downto 0);
    begin
      if (pnum < 0) or (pnum > 31) then
        return '0';
      end if;

      -- Shutdown forces all pins to behave as inputs without pullups
      if config_s_v = '0' then
        return sample_pin(pnum);
      end if;

      pair := cfg_pair(pnum);

      if pair = "01" then
        -- output mode
        if (pnum = 31) and (config_m_v = '1') then
          return int_lat_v; -- P31 becomes INT output latch when M=1
        else
          return port_lat_v(pnum);
        end if;
      else
        -- input modes (10, 11) and invalid (00 treated as input)
        return sample_pin(pnum);
      end if;
    end function;

    procedure decode_group(addr : integer; start_p : out integer; count : out integer) is
    begin
      -- valid for 0x40..0x5F only
      if addr >= 16#40# and addr <= 16#43# then
        start_p := 4;
        count   := addr - 16#3C#; -- 0x40->4 .. 0x43->7
      elsif addr >= 16#44# and addr <= 16#58# then
        start_p := addr - 16#40#; -- 0x44->4 .. 0x58->24
        count   := 8;
      elsif addr >= 16#59# and addr <= 16#5F# then
        start_p := addr - 16#40#; -- 0x59->25 .. 0x5F->31
        count   := 16#60# - addr; -- 0x59->7 .. 0x5F->1
      else
        start_p := 0;
        count   := 0;
      end if;
    end procedure;

    impure function read_register(addr : integer) return std_logic_vector(7 downto 0) is
      variable r : std_logic_vector(7 downto 0) := (others => '0');
      variable pnum, start_p, count : integer;
    begin
      case addr is
        when 16#00# =>
          r := (others => '0'); -- No-op
        when 16#04# =>
          -- Configuration register: only M (D7) and S (D0) are modeled; other bits read 0
          r(7) := config_m_v;
          r(6) := '0';
          r(0) := config_s_v;
        when 16#06# =>
          -- Mask register: D7 reads 0; D6..D0 are masks for P30..P24 (bit0=P24)
          r(7) := '0';
          r(6 downto 0) := mask_v;
        when 16#09# to 16#0F# =>
          r := cfg_regs_v(addr - 16#09#);
        when 16#20# to 16#3F# =>
          pnum := addr - 16#20#;
          r(0) := port_read_bit(pnum);
        when 16#40# to 16#5F# =>
          decode_group(addr, start_p, count);
          for i in 0 to count-1 loop
            r(i) := port_read_bit(start_p + i);
          end loop;
        when others =>
          r := (others => '0');
      end case;
      return r;
    end function;

    impure function sample_p24_p30 return std_logic_vector is
      variable s : std_logic_vector(6 downto 0);
    begin
      for i in 0 to 6 loop
        s(i) := port_read_bit(24 + i);
      end loop;
      return s;
    end function;

    procedure update_pin_drivers is
      variable pair : std_logic_vector(1 downto 0);
    begin
      for pnum in G_FIRST_PORT to G_LAST_PORT loop
        if config_s_v = '0' then
          -- shutdown: inputs without pullups
          p_drv(pnum) <= 'Z';
        else
          pair := cfg_pair(pnum);
          case pair is
            when "01" =>
              -- output
              if (pnum = 31) and (config_m_v = '1') then
                p_drv(pnum) <= int_lat_v;
              else
                p_drv(pnum) <= port_lat_v(pnum);
              end if;

            when "11" =>
              -- input with pullup: model as weak high
              p_drv(pnum) <= 'H';

            when others =>
              -- input without pullup (or invalid)
              p_drv(pnum) <= 'Z';
          end case;
        end if;
      end loop;
    end procedure;

    --------------------------------------------------------------------------
    -- Event locals
    --------------------------------------------------------------------------
    variable cmd   : std_logic_vector(7 downto 0);
    variable data  : std_logic_vector(7 downto 0);
    variable rw    : std_logic;
    variable addr  : integer;
    variable pnum, start_p, count : integer;
    variable cur   : std_logic_vector(6 downto 0);
  begin
    -- Initial outputs
    dout_reg <= '0';
    update_pin_drivers;

    -- Main event loop: react to SPI edges, CS latching, and GPIO changes for transition detect
    while true loop
      wait on CS, SCLK, P;

      ------------------------------------------------------------------------
      -- SPI shifting (only while CS=0)
      ------------------------------------------------------------------------
      if (CS = '0') and SCLK'event then
        if SCLK = '1' then
          -- rising edge: shift in DIN
          sh_v := sh_v(14 downto 0) & DIN;
        else
          -- falling edge: update DOUT from MSB (stable before next rising edge)
          dout_reg <= sh_v(15) after G_TDO;
        end if;
      end if;

      ------------------------------------------------------------------------
      -- Latch and execute on CS rising
      ------------------------------------------------------------------------
      if CS'event and (CS = '1') then
        cmd  := sh_v(15 downto 8);
        data := sh_v(7 downto 0);
        rw   := cmd(7); -- D15
        addr := to_integer(unsigned(cmd(6 downto 0))); -- D14..D8 (7-bit address)

        if rw = '0' then
          -- WRITE
          case addr is
            when 16#00# =>
              null; -- no-op

            when 16#04# =>
              -- Config: M (D7), S (D0)
              config_m_v := data(7);
              config_s_v := data(0);

              if data(7) = '1' then
                -- Writing config with M=1: take snapshot, arm one-shot, force INT low
                snapshot_v := sample_p24_p30;
                armed_v    := true;
                int_lat_v  := '0';
              else
                -- M cleared disables detection
                armed_v := false;
              end if;

            when 16#06# =>
              -- Mask register write updates masks; any access clears INT and disarms
              mask_v    := data(6 downto 0);
              int_lat_v := '0';
              armed_v   := false;

            when 16#07# =>
              null; -- reserved

            when 16#09# to 16#0F# =>
              cfg_regs_v(addr - 16#09#) := data;

            when 16#20# to 16#3F# =>
              pnum := addr - 16#20#;
              if (pnum >= 0) and (pnum <= 31) then
                port_lat_v(pnum) := data(0);
              end if;

            when 16#40# to 16#5F# =>
              decode_group(addr, start_p, count);
              for i in 0 to count-1 loop
                pnum := start_p + i;
                if (pnum >= 0) and (pnum <= 31) then
                  port_lat_v(pnum) := data(i);
                end if;
              end loop;

            when others =>
              null;
          end case;

        else
          -- READ: load addressed register value into sh_v(7..0) so it appears
          -- in the last 8 bits of the *next* 16 clocks.
          if addr = 16#06# then
            -- accessing mask register clears INT and disarms one-shot
            int_lat_v := '0';
            armed_v   := false;
          end if;

          sh_v(7 downto 0) := read_register(addr);
        end if;

        -- After command execution, update pin drivers
        update_pin_drivers;
      end if;

      ------------------------------------------------------------------------
      -- Transition detection (one-shot) on P24..P30 (mask bits) with INT on P31
      -- Enabled/armed by writing configuration register with M=1.
      ------------------------------------------------------------------------
      if (config_s_v = '1') and (config_m_v = '1') and armed_v and (int_lat_v = '0') then
        cur := sample_p24_p30;
        for i in 0 to 6 loop
          if (mask_v(i) = '1') and (cur(i) /= snapshot_v(i)) then
            int_lat_v := '1';   -- latch interrupt
            armed_v   := false; -- one-shot: disarm on first detected transition
            exit;
          end if;
        end loop;

        -- Pin drivers may need update if P31 is in INT mode
        update_pin_drivers;
      end if;
    end loop;
  end process;

end architecture;
