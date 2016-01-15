-- IEEE 488.1 Listener (L) interface function
--            Listener with address Extension (LE) interface function
-- implements L5  - basic listener, listen only mode, unaddress if MTA
-- implements LE5 - basic listener, listen only mode, unaddress if MSA ^ TPAS
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

entity gpib_l is
  generic (clock_cycle_time: time := 25 ns);
  port (clock:  in  std_logic;

        addr:   in  std_logic_vector(4 downto 0);
        addr_ext: in  std_logic;  -- address extention (LE)
                                  -- NOTE: change addr_ext only while pon is true
        sec_addr_v: in  std_logic_vector (31 downto 0);

        pon:    in  std_logic;  -- power on
	ltn:    in  std_logic;  -- listen
	lun:    in  std_logic;  -- local unlisten
        lon:    in  std_logic;  -- listen only

        IFC:    in  std_logic;  -- interface clear
        ATN:    in  std_logic;  -- attention

        multiline: in std_logic_vector(7 downto 0);

        listener_state:                 buffer listener_state_t;
        listener_primary_address_state: buffer listener_primary_address_state_t;

        acceptor_handshake_state:       in     acceptor_handshake_state_t;
        talker_primary_address_state:   in     talker_primary_address_state_t;
        controller_state:               in     controller_state_t);
end entity gpib_l;

architecture rtl of gpib_l is

  signal next_listener_state: listener_state_t;
  signal next_listener_primary_address_state: listener_primary_address_state_t;

begin
  listener_state_reg_process: process (clock)
  begin
    if rising_edge(clock) then
      listener_state <= next_listener_state;
      listener_primary_address_state <= next_listener_primary_address_state;
    end if;
  end process listener_state_reg_process;

  -- Some transitions are required to occur in t2, response to interface
  -- messages or state transitions, which is less than or equal to 200 ns.
  -- Some transitions are required to occur in t4, XXX
  -- For Time values see table 48 of IEEE 488.1-2003(E).
  next_listener_state_process: process (listener_state,
                                        listener_primary_address_state,
                                        acceptor_handshake_state,
                                        talker_primary_address_state,
                                        controller_state,
                                        pon,
			                lon,
					ltn,
					lun,
                                        addr,
                                        addr_ext,
                                        sec_addr_v,
				        IFC,
				        ATN,
                                        multiline)
  begin
    if pon = '1' or IFC = '1' then
      next_listener_state <= LIDS;
    else
      case listener_state is
        when LIDS =>
          if (lon = '1' or
              (ltn = '1' and controller_state = CACS) or
              (acceptor_handshake_state = ACDS and
               ((addr_ext = '0' and MLA(ATN, multiline, addr)) or
                (addr_ext = '1' and MSA(ATN, multiline, sec_addr_v) and listener_primary_address_state = LPAS)))) then
            next_listener_state <= LADS;
          else
            next_listener_state <= listener_state;
          end if;
        when LADS =>
          if ((lun = '1' and controller_state = CACS) or
              (acceptor_handshake_state = ACDS and
               (UNL(ATN, multiline) or
                (addr_ext = '0' and MTA(ATN, multiline, addr)) or
                (addr_ext = '1' and MSA(ATN, multiline, sec_addr_v) and talker_primary_address_state = TPAS)))) then
            next_listener_state <= LIDS;
          elsif ATN = '0' then
            next_listener_state <= LACS;  -- must occur within t2
          else
            next_listener_state <= listener_state;
          end if;
        when LACS =>
          if ATN = '1' then
            next_listener_state <= LADS;  -- must occur within t2
          else
            next_listener_state <= listener_state;
          end if;
      end case;
    end if;
  end process next_listener_state_process;

  next_listener_primary_address_state_process: process (listener_primary_address_state,
                                                        acceptor_handshake_state,
                                                        pon,
                                                        addr,
                                                        addr_ext,
                                                        IFC,
                                                        ATN,
                                                        multiline)
  begin
    if pon = '1' then
      next_listener_primary_address_state <= LPIS;
    else
      case listener_primary_address_state is
        when LPIS =>
          if addr_ext = '1' and MLA(ATN, multiline, addr) and acceptor_handshake_state = ACDS then
            next_listener_primary_address_state <= LPAS;
          else
            next_listener_primary_address_state <= listener_primary_address_state;
          end if;
        when LPAS =>
          if PCG(ATN, multiline) and not MLA(ATN, multiline, addr) and acceptor_handshake_state = ACDS then
            next_listener_primary_address_state <= LPIS;
          else
            next_listener_primary_address_state <= listener_primary_address_state;
          end if;
      end case;
    end if;
  end process next_listener_primary_address_state_process;

end architecture rtl;
