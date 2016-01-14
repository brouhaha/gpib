-- IEEE 488.1 Talker (T) interface function,
--            Talker with address Extension (TE) interface function
-- implements T5  - basic talker,
--                  serial poll,
--                  talk only mode,
--                  unaddress if MLA
-- implements TE5 - basic talker,
--                  serial poll,
--                  talk only mode,
--                  unaddress if MSA ^ LPAS
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

entity gpib_t is
  generic (clock_cycle_time: time := 25 ns);
  port (clock:    in  std_logic;

        addr:     in  std_logic_vector (4 downto 0);
        addr_ext: in  std_logic;  -- address extention (TE)
                                  -- NOTE: change addr_ext only while pon is true
        sec_addr: in  std_logic_vector (4 downto 0);

        pon:      in  std_logic;  -- power on
        ton:      in  std_logic;  -- talk only
        data_out: in  std_logic_vector(7 downto 0); -- data to transmit

        dev_send_data: out std_logic; -- device can send one DAB, EOS, END
        dev_send_stb:  out std_logic; -- device can send one STB message

        IFC:      in  std_logic;  -- interface clear
        ATN:      in  std_logic;  -- attention

        multiline:     in  std_logic_vector (7 downto 0);
        multiline_out: out std_logic_vector (7 downto 0);
        multiline_en:  out std_logic; -- multiline  driven actively

	END_out: out std_logic;  -- end of data
	END_en:  out std_logic;  -- end of data driven actively

        talker_state:                   buffer talker_state_t;
        talker_primary_address_state:   buffer talker_primary_address_state_t;
        talker_serial_poll_state:       buffer talker_serial_poll_state_t;

        acceptor_handshake_state:       in     acceptor_handshake_state_t;
        listener_primary_address_state: in     listener_primary_address_state_t);
end entity gpib_t;

architecture rtl of gpib_t is

  signal next_talker_state:                 talker_state_t;
  signal next_talker_primary_address_state: talker_primary_address_state_t;
  signal next_talker_serial_poll_state:     talker_serial_poll_state_t;

begin
  dev_send_data <= '1' when talker_state = TACS
              else '0';

  dev_send_stb  <= '1' when talker_state = SPAS
              else '0';

  -- END is driven passively false  in TIDS, TADS
  --                 actively  true or false in TACS, SPAS
  END_out <= '0' when talker_state = TIDS or talker_state = TADS
        else '1'; -- XXX need to determine what to send
  END_en <= '0' when talker_state = TIDS or talker_state = TADS
       else '1';

  multiline_out <= data_out;
  multiline_en  <= '1' when (talker_state = TACS or
                             talker_state = SPAS)
       else '0';

  talker_state_reg_process: process (clock)
  begin
    if rising_edge(clock) then
      talker_state <= next_talker_state;
      talker_primary_address_state <= next_talker_primary_address_state;
      talker_serial_poll_state <= next_talker_serial_poll_state;
    end if;
  end process talker_state_reg_process;

  -- Some transitions are required to occur in t2, response to interface
  -- messages or state transitions, which is less than or equal to 200 ns.
  -- Some transitions are required to occur in t4, XXX
  -- For Time values see table 48 of IEEE 488.1-2003(E).
  next_talker_state_process: process (talker_state,
                                      talker_primary_address_state,
  			              talker_serial_poll_state,
                                      listener_primary_address_state,
                                      acceptor_handshake_state,
                                      pon,
			              ton,
                                      addr_ext,
                                      addr,
                                      sec_addr,
				      IFC,
				      ATN,
                                      multiline)
  begin
    if pon = '1' or IFC = '1' then
      next_talker_state <= TIDS;
    else
      case talker_state is
        when TIDS =>
          if (ton = '1' or
              (acceptor_handshake_state = ACDS and
               ((addr_ext = '0' and MTA(ATN, multiline, addr)) or
                (addr_ext = '1' and MSA(ATN, multiline, sec_addr) and talker_primary_address_state = TPAS))))
          then
            next_talker_state <= TADS;
          else
            next_talker_state <= talker_state;
          end if;
        when TADS =>
          if (acceptor_handshake_state = ACDS and
              (OTA(ATN, multiline, addr) or
               (addr_ext = '0' and MLA(ATN, multiline, addr)) or
               (addr_ext = '1' and OSA(ATN, multiline, sec_addr) and talker_primary_address_state = TPAS) or
               (addr_ext = '1' and MSA(ATN, multiline, sec_addr) and listener_primary_address_state = LPAS)))
          then
            next_talker_state <= TIDS;
          elsif ATN = '0' and talker_serial_poll_state /= SPMS then
            next_talker_state <= TACS;
          elsif ATN = '0' and talker_serial_poll_state = SPMS then
            next_talker_state <= SPAS;
          else
            next_talker_state <= talker_state;
          end if;
        when TACS =>
          if ATN = '1' then
            next_talker_state <= TADS;  -- must occur within t2
          else
            next_talker_state <= talker_state;
          end if;
        when SPAS =>
          if ATN = '1' then
            next_talker_state <= TADS;  -- must occur within t2
          else
            next_talker_state <= talker_state;
          end if;
      end case;
    end if;
  end process next_talker_state_process;

  next_talker_primary_address_state_process: process (talker_primary_address_state,
                                                      acceptor_handshake_state,
                                                      pon,
                                                      addr_ext,
                                                      addr,
                                                      IFC,
                                                      ATN,
                                                      multiline)
  begin
    if pon = '1' or IFC = '1' or addr_ext = '0' then
      next_talker_primary_address_state <= TPIS;
    else
      case talker_primary_address_state is
        when TPIS =>
          if addr_ext = '1' and MTA(ATN, multiline, addr) and acceptor_handshake_state = ACDS then
            next_talker_primary_address_state <= TPAS;
          else
            next_talker_primary_address_state <= talker_primary_address_state;
          end if;
        when TPAS =>
          if PCG(ATN, multiline) and not MTA(ATN, multiline, addr) and acceptor_handshake_state = ACDS then
            next_talker_primary_address_state <= TPIS;
          else
            next_talker_primary_address_state <= talker_primary_address_state;
          end if;
      end case;
    end if;
  end process next_talker_primary_address_state_process;

  -- Some transitions are required to occur in t4, XXX
  -- For Time values see table 48 of IEEE 488.1-2003(E).
  next_talker_serial_poll_state_process: process (talker_serial_poll_state,
                                                  acceptor_handshake_state,
                                                  pon,
                                                  IFC,
                                                  ATN,
                                                  multiline)
  begin
    if pon = '1' or IFC = '1' then
      next_talker_serial_poll_state <= SPIS;
    else
      case talker_serial_poll_state is
        when SPIS =>
          if IFC = '0' and SPE(ATN, multiline) and acceptor_handshake_state = ACDS then
            next_talker_serial_poll_state <= SPMS;
          else
            next_talker_serial_poll_state <= talker_serial_poll_state;
          end if;
        when SPMS =>
          if SPD(ATN, multiline) and acceptor_handshake_state = ACDS then
            next_talker_serial_poll_state <= SPIS;
          else
            next_talker_serial_poll_state <= talker_serial_poll_state;
          end if;
      end case;
    end if;
  end process next_talker_serial_poll_state_process;

end architecture rtl;
