-- IEEE 488.1 Device Trigger (DT) interface function
-- implements DT1 - complete capability
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

entity gpib_dt is
  generic (clock_cycle_time: time := 25 ns);
  port (clock:     in  std_logic;

        ATN:       in  std_logic;
        multiline: in  std_logic_vector(7 downto 0);

        device_trigger_state:     out device_trigger_state_t;
        acceptor_handshake_state: in  acceptor_handshake_state_t;
        listener_state:           in  listener_state_t);
end entity gpib_dt;

architecture rtl of gpib_dt is
begin
  device_trigger_state <= DTAS when (GET(ATN, multiline) and
                                     listener_state = LADS and
                                     acceptor_handshake_state = ACDS)
                     else DTIS;
end architecture rtl;
