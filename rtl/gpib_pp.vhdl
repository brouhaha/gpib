-- IEEE 488.1 Parallel Poll (PP) interface function
-- implements PP1 - remote configuration
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

entity gpib_pp is
  generic (clock_cycle_time: time := 25 ns);
  port (clock: in  std_logic;

        pon:    in  std_logic;  -- power on
        ist:    in  std_logic;  -- individual status
	lpe:    in  std_logic;  -- local poll enabled

        ATN:    in  std_logic;  -- attention
        IDY:    in  std_logic;  -- identify

	multiline: in std_logic_vector(7 downto 0);

    	PPRn_out:  out std_logic_vector(7 downto 0);  -- parallel poll response
    	PPRn_en:   out std_logic;  -- parallel poll response enable

        parallel_poll_state: buffer parallel_poll_state_t;
        parallel_poll_addressed_state: buffer parallel_poll_addressed_state_t;

        acceptor_handshake_state: in acceptor_handshake_state_t;
        listener_state: in listener_state_t);
end entity gpib_pp;

architecture rtl of gpib_pp is

  signal next_parallel_poll_state: parallel_poll_state_t;
  signal next_parallel_poll_addressed_state: parallel_poll_addressed_state_t;

  signal parallel_poll_response: std_logic;

  signal S, next_S: std_logic;
  signal P, next_P: unsigned(2 downto 0);

begin
  -- PPRn is driven actively true in PPAS
  parallel_poll_response_process: process(parallel_poll_state,
   				          ist,
					  S,
					  P)
  begin
    for i in 0 to 7 loop
      PPRn_out(i) <= to_std_logic(parallel_poll_state = PPAS
                                  and ist = S
                                  and P = i);
    end loop;
  end process parallel_poll_response_process;

  PPRn_en <= '1' when parallel_poll_state = PPAS
        else '0';

  parallel_poll_state_reg_process: process (clock)
  begin
    if rising_edge(clock) then
      parallel_poll_state <= next_parallel_poll_state;
      parallel_poll_addressed_state <= next_parallel_poll_addressed_state;
      P <= next_P;
      S <= next_S;
    end if;
  end process parallel_poll_state_reg_process;

  -- Some transitions are required to occur in t5, response to ATN and EOI,
  -- which is less than or equal to 200 ns.
  -- For Time values see table 48 of IEEE 488.1-2003(E).
  next_parallel_poll_state_process: process (parallel_poll_state,
  				    	     parallel_poll_addressed_state,
                                             acceptor_handshake_state,
                                             pon,
                                             lpe,
					     ATN,
					     IDY,
					     multiline,
                                             S,
                                             P)
  begin
    next_parallel_poll_state <= parallel_poll_state;
    next_S <= S;
    next_P <= P;
    if pon = '1' then
      next_parallel_poll_state <= PPIS;
    else
      case parallel_poll_state is
        when PPIS =>
          if (PPE (ATN, multiline)
	      and parallel_poll_addressed_state = PACS
	      and acceptor_handshake_state = ACDS)
	    or lpe = '1' then
            next_parallel_poll_state <= PPSS;
            next_S <= multiline (3);
            next_P <= unsigned (multiline(2 downto 0));
          end if;
        when PPSS =>
          if (((PPD(ATN, multiline)
                and parallel_poll_addressed_state = PACS)
               or PPU(ATN, multiline))
              and acceptor_handshake_state = ACDS)
            or lpe = '0' then
            next_parallel_poll_state <= PPIS;
          elsif IDY = '1' and ATN = '1' then
            next_parallel_poll_state <= PPAS; -- must occur within t5
          end if;
        when PPAS =>
          if ATN = '0' or IDY = '0' then
            next_parallel_poll_state <= PPSS; -- must occur within t5
          end if;
      end case;
    end if;
  end process next_parallel_poll_state_process;

  next_parallel_poll_addressed_state_process: process (parallel_poll_addressed_state,
                                                       acceptor_handshake_state,
                                                       listener_state,
                                                       pon,
                                                       ATN,
                                                       multiline)
  begin
    next_parallel_poll_addressed_state <= parallel_poll_addressed_state;
    if pon = '1' then
      next_parallel_poll_addressed_state <= PUCS;
    else
      case parallel_poll_addressed_state is
        when PUCS =>
          if PPC(ATN, multiline) and listener_state = LADS and acceptor_handshake_state = ACDS then
            next_parallel_poll_addressed_state <= PACS;
          end if;
        when PACS =>
          if PCG(ATN, multiline) and (not PPC(ATN, multiline)) and acceptor_handshake_state = ACDS then
            next_parallel_poll_addressed_state <= PUCS;
          end if;
      end case;
    end if;
  end process next_parallel_poll_addressed_state_process;

end architecture rtl;
