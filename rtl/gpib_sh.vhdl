-- IEEE 488.1 Source Handshake (SH) interface function
-- implements SH1 - complete capability
-- Copyright 2016 Eric Smith <spacewar@gmail.com>

-- In the following licensing information, the terms "program" includes but
-- is not limited to the provided VHDL source files and testbench stimulus
-- files, and any works derived therefrom, even if translated or compiled
-- into a different form and/or embedded in hardware.

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License
-- (GPL) as published by the Free Software Foundation, but not any
-- earlier or later version of the GPL.

-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

use work.gpib_defs.all;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity gpib_sh is
  generic (clock_cycle_time: time := 25 ns);
  port (clock: in  std_logic;

        pon:     in  std_logic;  -- power on
        nba:     in  std_logic;  -- new byte available

        dev_change_multiline: out std_logic; -- device requested to change
                                             -- multiline message

        ATN:     in  std_logic;  -- attention
        RFD:     in  std_logic;  -- ready for data
        DAC:     in  std_logic;  -- data accepted

	DAV_out: out std_logic;  -- data available
	DAV_en:  out std_logic;  -- data available driven actively

        source_handshake_state: buffer source_handshake_state_t;

        talker_state: in talker_state_t;
        controller_state: in controller_state_t);

end entity gpib_sh;

architecture rtl of gpib_sh is

  constant T1_time: time := 2 us;

  constant T1_clock_cycles: integer := T1_time / clock_cycle_time;

  constant source_handshake_state_timer_max: integer := T1_clock_cycles;

  signal next_source_handshake_state: source_handshake_state_t;

  signal transition_to_idle: std_logic;
  signal transition_from_idle: std_logic;
  signal transition_to_STRS: std_logic;

  signal source_handshake_state_timer: unsigned (log2ceil(source_handshake_state_timer_max) downto 0);

begin
  dev_change_multiline <= '1' when (source_handshake_state = SIDS or
                                    source_handshake_state = SGNS or
                                    source_handshake_state = SWNS or
                                    source_handshake_state = SIWS)
                     else '0';

  -- DAV is driven passively false  in SIDS, SIWS
  --               actively  false  in SGNS, SDYS
  --               actively  true   in STRS
  --               actively  true or false in SWNS
  DAV_out <= '1' when source_handshake_state = STRS
    else '0';

  DAV_en <= '1' when (source_handshake_state = SGNS)
                  or (source_handshake_state = SDYS)
		  or (source_handshake_state = STRS)
		  or (source_handshake_state = SWNS)
       else '0';

  transition_to_idle <= to_std_logic((ATN = '1' and not (controller_state = CACS or
                                            controller_state = CTRS))
                        or (ATN = '0' and not (talker_state = TACS or
                                               talker_state = SPAS)));

  transition_from_idle <= to_std_logic(talker_state = TACS or
                                       talker_state = SPAS or
                                       controller_state = CACS);

  source_handshake_state_reg_process: process (clock)
  begin
    if rising_edge(clock) then
      if pon = '1' or source_handshake_state /= next_source_handshake_state then
        source_handshake_state_timer <= (others => '0');
      elsif source_handshake_state_timer /= unsigned'(source_handshake_state_timer'range => '1') then
        source_handshake_state_timer <= source_handshake_state_timer + 1;
      else
        source_handshake_state_timer <= source_handshake_state_timer;
      end if;
      
      source_handshake_state <= next_source_handshake_state;
    end if;
  end process source_handshake_state_reg_process;

  -- Some transitions are required to occur in t2, response to interface
  -- messages or state transitions, which is less than or equal to 200 ns.
  -- Some transitions are required to occur after T1, the settling time
  -- for multiline messages, which is >= 2 us.
  -- For Time values see table 48 of IEEE 488.1-2003(E).
  next_source_handshake_state_process: process (source_handshake_state,
                                                source_handshake_state_timer,
                                                pon,
                                                nba,
                                                RFD,
                                                DAC,
                                                transition_to_idle,
                                                transition_from_idle)
  begin
    next_source_handshake_state <= source_handshake_state;
    if pon = '1' then
      next_source_handshake_state <= SIDS;
    else
      case source_handshake_state is
        when SIDS =>
          if transition_from_idle = '1' then
            next_source_handshake_state <= SGNS;
          end if;
        when SGNS =>
          if transition_to_idle = '1' then
            next_source_handshake_state <= SIDS;  -- must occur within t2
          elsif nba = '1' then
            next_source_handshake_state <= SDYS;
          end if;
        when SDYS =>
          if transition_to_idle = '1' then
            next_source_handshake_state <= SIDS;  -- must occur within t2
          elsif RFD = '1' and DAC = '0' and source_handshake_state_timer >= T1_clock_cycles then
            next_source_handshake_state <= STRS;  -- must not transition until T1
          end if;
        when STRS =>
          if transition_to_idle = '1' then
            next_source_handshake_state <= SIWS;  -- must occur within t2
          elsif DAC = '1' then
            next_source_handshake_state <= SWNS;
          end if;
        when SWNS =>
          if transition_to_idle = '1' then
            next_source_handshake_state <= SIWS;  -- must occur within t2
          elsif nba = '0' then
            next_source_handshake_state <= SGNS;
          end if;
        when SIWS =>
          if transition_from_idle = '1' then
            next_source_handshake_state <= SWNS;
          elsif nba = '0' then
            next_source_handshake_state <= SIDS;
          end if;
      end case;
    end if;
  end process next_source_handshake_state_process;

end architecture rtl;
