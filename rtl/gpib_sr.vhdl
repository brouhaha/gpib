-- IEEE 488.1 Service Request (SR) interface function
-- implements SR1 - complete capability
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

entity gpib_sr is
  generic (clock_cycle_time: time := 25 ns);
  port (clock:   in  std_logic;

        pon:     in  std_logic;  -- power on
        rsv:     in  std_logic;  -- request service

    	SRQ_out: out std_logic;
    	SRQ_en:  out std_logic;

        service_request_state: buffer service_request_state_t;
        talker_state:          in     talker_state_t);
end entity gpib_sr;

architecture rtl of gpib_sr is

  signal next_service_request_state: service_request_state_t;

begin
  -- SRQ is driven actively  true  in SRQS
  -- SRQ is driven passively false in NPRS, APRS
  SRQ_out <= '1' when service_request_state = SRQS
        else '0';
  SRQ_en  <= '1' when service_request_state = SRQS
        else '0';

  service_request_state_reg_process: process (clock)
  begin
    if rising_edge(clock) then
      service_request_state <= next_service_request_state;
    end if;
  end process service_request_state_reg_process;

  next_service_request_state_process: process (service_request_state,
                                               talker_state,
                                               pon,
                                               rsv)
  begin
    if pon = '1' then
      next_service_request_state <= NPRS;
    else
      case service_request_state is
        when NPRS =>
          if rsv = '1' and talker_state /= SPAS then
            next_service_request_state <= SRQS;
          else
            next_service_request_state <= service_request_state;
          end if;
        when SRQS =>
          if talker_state = SPAS then
            next_service_request_state <= APRS;
          else
            next_service_request_state <= service_request_state;
          end if;
        when APRS =>
          if rsv = '0' and talker_state /= SPAS then
            next_service_request_state <= NPRS;
          else
            next_service_request_state <= service_request_state;
          end if;
      end case;
    end if;
  end process next_service_request_state_process;

end architecture rtl;
