-- IEEE 488.1 Acceptor Handshake (AH) interface function
-- implements AH1 - complete capability
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

entity gpib_ah is
  generic (clock_cycle_time: time := 25 ns);
  port (clock:   in  std_logic;

        pon:     in  std_logic;  -- power on
        rdy:     in  std_logic;  -- ready for next message
	tcs:     in  std_logic;  -- take control synchronously

        ATN:     in  std_logic;  -- attention
        DAV:     in  std_logic;  -- data valid

    	RFD_out: out std_logic;  -- ready for data
    	RFD_en:  out std_logic;  -- ready for data driven actively
	DAC_out: out std_logic;  -- data accepted
	DAC_en:  out std_logic;  -- data accepted driven actively

        acceptor_handshake_state: buffer acceptor_handshake_state_t;

        listener_state: in listener_state_t);
end entity gpib_ah;

architecture rtl of gpib_ah is

  constant T3_time: time := 1 * clock_cycle_time;

  constant T3_clock_cycles: integer := T3_time / clock_cycle_time;

  constant acceptor_handshake_state_timer_max: integer := T3_clock_cycles;

  signal acceptor_handshake_state_timer: unsigned(log2ceil(acceptor_handshake_state_timer_max) downto 0);

  signal next_acceptor_handshake_state: acceptor_handshake_state_t;

begin
  -- RFD is driven passively true   in AIDS, ACRS
  --               actively  false  in ANRS, ACDS, AWNS
  RFD_out <= '1' when (acceptor_handshake_state = AIDS)
                   or (acceptor_handshake_state = ACRS)
    else '0';

  RFD_en <= '0' when (acceptor_handshake_state = AIDS)
                  or (acceptor_handshake_state = ACRS)
       else '1';

  -- DAC is driven passively true  in AIDS, AWNS
  --               actively false  in ANRS, ACRS, ACDS
  DAC_out <= '1' when (acceptor_handshake_state = AIDS)
                   or (acceptor_handshake_state = AWNS)
    else '0';

  DAC_en <= '0' when (acceptor_handshake_state = AIDS)
                  or (acceptor_handshake_state = AWNS)
       else '1';

  acceptor_handshake_state_reg_process: process (clock)
  begin
    if rising_edge(clock) then
      if pon = '1' or acceptor_handshake_state /= next_acceptor_handshake_state then
        acceptor_handshake_state_timer <= (others => '0');
      elsif acceptor_handshake_state_timer /= unsigned'(acceptor_handshake_state_timer'range => '1') then
        acceptor_handshake_state_timer <= acceptor_handshake_state_timer + 1;
      else
        acceptor_handshake_state_timer <= acceptor_handshake_state_timer;
      end if;
      
      acceptor_handshake_state <= next_acceptor_handshake_state;
    end if;
  end process acceptor_handshake_state_reg_process;

  -- Some transitions are required to occur in t2, response to interface
  -- messages or state transitions, which is less than or equal to 200 ns.
  -- Some transitions are required to occur after T3, the time required
  -- for interface function to accept, not necessarily respond to,
  -- interface messages. Value is implementation-dependent.
  -- For Time values see table 48 of IEEE 488.1-2003(E).
  next_acceptor_handshake_state_process: process (acceptor_handshake_state,
                                                  acceptor_handshake_state_timer,
                                                  pon,
                                                  rdy,
                                                  tcs,
                                                  ATN,
                                                  DAV,
                                                  listener_state)
  begin
    next_acceptor_handshake_state <= acceptor_handshake_state;
    if pon = '1' or ((ATN = '0') and
                     not (listener_state = LACS or listener_state = LADS)) then
      next_acceptor_handshake_state <= AIDS;
    else
      case acceptor_handshake_state is
        when AIDS =>
          if ATN = '1' or listener_state = LACS or listener_state = LADS then
            next_acceptor_handshake_state <= ANRS;
          end if;
        when ANRS =>
          -- In following, note that DAV = '0' is "optional". See standard.
          if ((ATN = '1' and DAV = '0') or rdy = '1') and tcs = '0' then
            next_acceptor_handshake_state <= ACRS; -- must occur within t2
          elsif DAV = '1' then -- should never occur under normal operation
            next_acceptor_handshake_state <= AWNS;
          end if;
        when ACRS =>
          if ATN = '0' and rdy = '0' then
            next_acceptor_handshake_state <= ANRS;  -- must occur within t2
          elsif DAV = '1' then
            next_acceptor_handshake_state <= ACDS;
          end if;
        when ACDS =>
          if ((rdy = '0' and ATN = '0') or
              (ATN = '1' and acceptor_handshake_state_timer >= T3_clock_cycles)) then
            next_acceptor_handshake_state <= AWNS;
          elsif DAV = '0' then
            next_acceptor_handshake_state <= ACRS;
          end if;
        when AWNS =>
          if DAV = '0' then
            next_acceptor_handshake_state <= ANRS;
          end if;
      end case;
    end if;
  end process next_acceptor_handshake_state_process;

end architecture rtl;
