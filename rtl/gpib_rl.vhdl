-- IEEE 488.1 Remote Local (RL) interface function
-- implements RL1 - complete capability
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

entity gpib_rl is
  generic (clock_cycle_time: time := 25 ns);
  port (clock:     in  std_logic;
        addr:      in  std_logic_vector(4 downto 0);

        pon:       in  std_logic;  -- power on
        rtl:       in  std_logic;  -- return to local

        ATN:       in  std_logic;
        REN:       in  std_logic;
        multiline: in  std_logic_vector(7 downto 0);

        rem_ctrl:  out std_logic;  -- remote control mode

        remote_local_state:       buffer remote_local_state_t;
        acceptor_handshake_state: in     acceptor_handshake_state_t;
        listener_state:           in     listener_state_t);
end entity gpib_rl;

architecture rtl of gpib_rl is

  signal next_remote_local_state: remote_local_state_t;

begin
  rem_ctrl <= '1' when (remote_local_state = REMS or
                        remote_local_state = RWLS)
         else '0';

  remote_local_state_reg_process: process (clock)
  begin
    if rising_edge(clock) then
      remote_local_state <= next_remote_local_state;
    end if;
  end process remote_local_state_reg_process;

  next_remote_local_state_process: process (remote_local_state,
                                            acceptor_handshake_state,
                                            listener_state,
                                            addr,
                                            pon,
                                            rtl,
                                            ATN,
                                            REN,
                                            multiline)
  begin
    if (pon = '1' or
        REN = '0') -- within t4, < 100us
    then
      next_remote_local_state <= LOCS;
    else
      case remote_local_state is
        when LOCS =>
          if (REN = '1' and
              rtl = '0' and
              MLA(ATN, multiline, addr) and
              acceptor_handshake_state = ACDS) then
            next_remote_local_state <= REMS;
          elsif (REN = '1' and
                 LLO(ATN, multiline) and
                 acceptor_handshake_state = ACDS) then
            next_remote_local_state <= LWLS;
          else
            next_remote_local_state <= remote_local_state;
          end if;
        when LWLS =>
          if (MLA(ATN, multiline, addr) and
              acceptor_handshake_state = ACDS) then
            next_remote_local_state <= RWLS;
          else
            next_remote_local_state <= remote_local_state;
          end if;
        when REMS =>
          if ((GTL(ATN, multiline) and
               rtl = '0' and
               MLA(ATN, multiline, addr) and
               acceptor_handshake_state = ACDS) or
              (rtl = '1' and
               ((not LLO(ATN, multiline)) or
                acceptor_handshake_state /= ACDS))) then
            next_remote_local_state <= LOCS;
          elsif (LLO(ATN, multiline) and
                 acceptor_handshake_state = ACDS) then
            next_remote_local_state <= RWLS;
          else
            next_remote_local_state <= remote_local_state;
          end if;
        when RWLS =>
          if (GTL(ATN, multiline) and
              listener_state = LADS and
              acceptor_handshake_state = ACDS) then
            next_remote_local_state <= LWLS;
          else
            next_remote_local_state <= remote_local_state;
          end if;
      end case;
    end if;
  end process next_remote_local_state_process;

end architecture rtl;
