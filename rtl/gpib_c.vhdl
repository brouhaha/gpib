-- IEEE 488.1 Controller (C) interface function
-- implements C1: system controller
--            C2: send IFC and take charge,
--            C3: send REN
--            C4: respond to SRQ
--            C5: send I.F. messages, receive control, pass control,
--                pass control to self, parallel poll,
--                take control synchronously
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

entity gpib_c is
  generic (clock_cycle_time: time := 25 ns);
  port (clock:   in  std_logic;

        pon:     in  std_logic;  -- power on
        rsc:     in  std_logic;  -- request system control
        rpp:     in  std_logic;  -- request parallel poll
        gts:     in  std_logic;  -- go to standby
        tca:     in  std_logic;  -- take control asynchronously
        tcs:     in  std_logic;  -- take control synchronously
        sic:     in  std_logic;  -- send interface clear
        sre:     in  std_logic;  -- send remote enable;
        multiline_msg_out: in std_logic_vector(7 downto 0);

        ATN:     in  std_logic;  -- attention
        ATN_out: out std_logic;
        ATN_en:  out std_logic;

        IDY_out: out std_logic;
        IDY_en:  out std_logic;

        IFC:     in  std_logic;  -- interface clear
        IFC_out: out std_logic;
        IFC_en:  out std_logic;

        REN_out: out std_logic;
        REN_en:  out std_logic;
        
        SRQ:     in  std_logic;  -- service request

        multiline:     in  std_logic_vector (7 downto 0);
        multiline_out: out std_logic_vector (7 downto 0);
        multiline_en:  out std_logic;

        controller_state:                   buffer controller_state_t;
        controller_service_requested_state: buffer controller_service_requested_state_t;
        controller_system_control_state:    buffer controller_system_control_state_t;
        controller_remote_enable_state:     buffer controller_remote_enable_state_t;
        controller_interface_clear_state:   buffer controller_interface_clear_state_t;

        acceptor_handshake_state: in acceptor_handshake_state_t;
        source_handshake_state:   in source_handshake_state_t;
        talker_state:             in talker_state_t);
end entity gpib_c;

architecture rtl of gpib_c is

  signal next_controller_state: controller_state_t;
  signal next_controller_remote_enable_state: controller_remote_enable_state_t;
  signal next_controller_interface_clear_state: controller_interface_clear_state_t;

  -- For Time values see table 48 of IEEE 488.1-2003(E).
  constant T6_time:  time :=    2 us; -- parallel poll execution time
  constant T7_time:  time :=  500 ns; -- controller delay to allow current
                                      -- talker to see ATN message
  constant T8_time:  time :=  100 us; -- length of IFC or REN false
  constant T9_time:  time := 1500 ns; -- delay for EOI
  constant T10_time: time := 1500 ns; -- delay for NDAV

  -- XXX following should be changed to round upward
  constant T6_clock_cycles: integer  := T6_time  / clock_cycle_time;
  constant T7_clock_cycles: integer  := T7_time  / clock_cycle_time;
  constant T8_clock_cycles: integer  := T8_time  / clock_cycle_time;
  constant T9_clock_cycles: integer  := T9_time  / clock_cycle_time;
  constant T10_clock_cycles: integer := T10_time / clock_cycle_time;

  constant controller_state_timer_max: integer := max(max(T6_clock_cycles, T7_clock_cycles), max(T9_clock_cycles, T10_clock_cycles));
  constant controller_remote_enable_state_timer_max: integer := T8_clock_cycles;
  constant controller_interface_clear_state_timer_max: integer := T8_clock_cycles;

  signal controller_state_timer: unsigned(log2ceil(controller_state_timer_max) downto 0);
  signal controller_remote_enable_state_timer: unsigned(log2ceil(T8_clock_cycles) downto 0);
  signal controller_interface_clear_state_timer: unsigned(log2ceil(T8_clock_cycles) downto 0);
  
begin
  -- ATN is driven actively  true  in CACS, CPWS, CPPS, CSWS, CAWS, CTRS
  -- ATN is driven actively  false in CSBS, CSHS
  -- ATN is driven passively false in CIDS, CADS
  ATN_out <= '1' when (controller_state = CACS or
                       controller_state = CPWS or
                       controller_state = CPPS or
                       controller_state = CSWS or
                       controller_state = CAWS or
                       controller_state = CTRS)
        else '0';
  ATN_en  <= '0' when (controller_state = CIDS or
                       controller_state = CADS)
        else '1';

  -- IDY is driven actively  true  in CPWS, CPPS
  -- IDY is driven actively  false in CACS, CSWS, CAWS, CTRS
  -- IDY is driven passively false in CIDS, CADS, CSBS, CSHS
  IDY_out <= '1' when (controller_state = CPWS or
                       controller_state = CPPS)
        else '0';
  IDY_en  <= '0' when (controller_state = CIDS or
                       controller_state = CADS or
                       controller_state = CSBS or
                       controller_state = CSHS)
        else '1';

  -- IFC is driven actively  true  in SIAS
  -- IFC is driven actively  false in SINS
  -- IFC is driven passively false in SIIS
  IFC_out <= '1' when controller_interface_clear_state = SIAS
        else '0';
  IFC_en  <= '0' when controller_interface_clear_state = SIIS
        else '1';

  -- REN is driven actively  true  in SRAS
  -- REN is driven actively  false in SRNS
  -- REN is driven passively false in SRIS
  REN_out <= '1' when controller_remote_enable_state = SRAS
        else '0';
  REN_en  <= '0' when controller_remote_enable_state = SRIS
        else '1';

  multiline_out <= "00011001"        when controller_state = CTRS
              else multiline_msg_out when controller_state = CACS
              else "00000000";
  multiline_en  <= '1' when (controller_state = CTRS or
                             controller_state = CACS)
             else '0';

  controller_state_reg_process: process (clock)
  begin
    if rising_edge (clock) then
      if pon = '1' or controller_state /= next_controller_state then
        controller_state_timer <= (others => '0');
      elsif controller_state_timer /= unsigned'(controller_state_timer'range => '1') then
        controller_state_timer <= controller_state_timer + 1;
      else
        controller_state_timer <= controller_state_timer;
      end if;

      controller_state <= next_controller_state;
    end if;
  end process controller_state_reg_process;

  controller_remote_enable_state_reg_process: process (clock)
  begin
    if rising_edge (clock) then
      if pon = '1' or controller_remote_enable_state /= next_controller_remote_enable_state then
        controller_remote_enable_state_timer <= (others => '0');
      elsif controller_remote_enable_state_timer /= unsigned'(controller_remote_enable_state_timer'range => '1') then
        controller_remote_enable_state_timer <= controller_remote_enable_state_timer + 1;
      else
        controller_remote_enable_state_timer <= controller_remote_enable_state_timer;
      end if;

      controller_remote_enable_state <= next_controller_remote_enable_state;
    end if;
  end process controller_remote_enable_state_reg_process;

  controller_interface_clear_state_reg_process: process (clock)
  begin
    if rising_edge (clock) then
      if pon = '1' or controller_interface_clear_state /= next_controller_interface_clear_state then
        controller_interface_clear_state_timer <= (others => '0');
      elsif controller_interface_clear_state_timer /= unsigned'(controller_interface_clear_state_timer'range => '1') then
        controller_interface_clear_state_timer <= controller_interface_clear_state_timer + 1;
      else
        controller_interface_clear_state_timer <= controller_interface_clear_state_timer;
      end if;

      controller_interface_clear_state <= next_controller_interface_clear_state;
    end if;
  end process controller_interface_clear_state_reg_process;

  -- Some transitions are required to occur in t4.
  -- Some states are required to hold for a minimum of T6, T7, T9, or T10.
  -- For Time values see table 48 of IEEE 488.1-2003(E).
  next_controller_state_process: process (controller_state,
                                          controller_state_timer,
                                          controller_system_control_state,
                                          controller_interface_clear_state,
                                          acceptor_handshake_state,
                                          source_handshake_state,
                                          talker_state,
                                          pon,
                                          rpp,
                                          gts,
                                          tca,
                                          tcs,
				          IFC,
				          ATN,
                                          multiline)
  begin
    if (pon = '1' or
        (IFC = '1' and controller_system_control_state = SACS)) -- within t4
    then
      next_controller_state <= CIDS;
    else
      case controller_state is
        when CIDS =>
          if ((IFC = '0' and
               acceptor_handshake_state = ACDS and
               TCT(ATN, multiline) and
               talker_state = TADS)
              or controller_interface_clear_state = SIAS) then
            next_controller_state <= CADS;
          else
            next_controller_state <= controller_state;
          end if;
        when CADS =>
          if ATN = '0' then
            next_controller_state <= CACS;
          else
            next_controller_state <= controller_state;
          end if;
        when CTRS =>
          if source_handshake_state /= STRS then
            next_controller_state <= CIDS;
          else
            next_controller_state <= controller_state;
          end if;
        when CACS =>
          if (TCT(ATN, multiline) and
              acceptor_handshake_state = ACDS and
              talker_state /= TADS) then
            next_controller_state <= CTRS;
          elsif (rpp = '1' and
                 source_handshake_state /= STRS and
                 source_handshake_state /= SDYS) then
            next_controller_state <= CPWS;
          elsif (gts = '1' and
                 source_handshake_state /= STRS and
                 source_handshake_state /= SDYS) then
            next_controller_state <= CSBS;
          else
            next_controller_state <= controller_state;
          end if;
        when CPWS =>
          if controller_state_timer >= T6_clock_cycles then
            next_controller_state <= CPPS;
          elsif rpp = '0' then
            next_controller_state <= CAWS;
          else
            next_controller_state <= controller_state;
          end if;
        when CPPS =>
          if rpp = '0' then
            next_controller_state <= CAWS;
          else
            next_controller_state <= controller_state;
          end if;
        when CSBS =>
          if tca = '1' then
            next_controller_state <= CSWS;
          elsif tcs = '1' and acceptor_handshake_state = ANRS then
            next_controller_state <= CSHS;
          else
            next_controller_state <= controller_state;
          end if;
        when CSHS =>
          if tcs = '0' then
            next_controller_state <= CSBS;
          elsif controller_state_timer >= T10_clock_cycles then
            next_controller_state <= CSWS;
          else
            next_controller_state <= controller_state;
          end if;
        when CAWS =>
          if rpp = '1' then
            next_controller_state <= CPWS;
          elsif controller_state_timer >= T9_clock_cycles and rpp = '0' then
            next_controller_state <= CACS;
          else
            next_controller_state <= controller_state;
          end if;
        when CSWS =>
          if controller_state_timer >= T7_clock_cycles and
            talker_state = TADS
          then
            next_controller_state <= CAWS;
          else
            next_controller_state <= controller_state;
          end if;
      end case;
    end if;
  end process next_controller_state_process;

  controller_service_requested_state <= CSRS when SRQ = '1'
                                   else CSNS;

  controller_system_control_state <= SACS when rsc = '1'
                                else SNAS;

  next_controller_remote_enable_state_process: process (controller_remote_enable_state,
                                                        controller_remote_enable_state_timer,
                                                        controller_system_control_state,
                                                        pon,
                                                        sre)
  begin
    if (pon = '1' or controller_system_control_state /= SACS) then
      next_controller_remote_enable_state <= SRIS;
    else
      case controller_remote_enable_state is
        when SRIS =>
          if (controller_system_control_state = SACS and
              sre = '1' and
              controller_remote_enable_state_timer >= T8_clock_cycles) then
            next_controller_remote_enable_state <= SRAS;
          elsif (controller_system_control_state = SACS and
                 sre = '0') then
            next_controller_remote_enable_state <= SRNS;
          else
            next_controller_remote_enable_state <= controller_remote_enable_state;
          end if;
        when SRNS =>
          if (sre = '1' and
              controller_remote_enable_state_timer >= T8_clock_cycles) then
            next_controller_remote_enable_state <= SRAS;
          else
            next_controller_remote_enable_state <= controller_remote_enable_state;
          end if;
        when SRAS =>
          if sre = '0' then
            next_controller_remote_enable_state <= SRNS;
          else
            next_controller_remote_enable_state <= controller_remote_enable_state;
          end if;
      end case;
    end if;
  end process next_controller_remote_enable_state_process;

  next_controller_interface_clear_state_process: process (controller_interface_clear_state,
                                                          controller_interface_clear_state_timer,
                                                          controller_system_control_state,
                                                          pon,
                                                          sic)
  begin
    if (pon = '1' or controller_system_control_state /= SACS) then
      next_controller_interface_clear_state <= SIIS;
    else
      case controller_interface_clear_state is
        when SIIS =>
          if (controller_system_control_state = SACS and
              sic = '1') then
            next_controller_interface_clear_state <= SIAS;
          elsif (controller_system_control_state = SACS and
                 sic = '0') then
            next_controller_interface_clear_state <= SINS;
          else
            next_controller_interface_clear_state <= controller_interface_clear_state;
          end if;
        when SINS =>
          if sic = '1' then
            next_controller_interface_clear_state <= SIAS;
          else
            next_controller_interface_clear_state <= controller_interface_clear_state;
          end if;
        when SIAS =>
          if (sic = '0' and
              controller_interface_clear_state_timer >= T8_clock_cycles) then
            next_controller_interface_clear_state <= SINS;
          else
            next_controller_interface_clear_state <= controller_interface_clear_state;
          end if;
      end case;
    end if;
  end process next_controller_interface_clear_state_process;

end architecture rtl;
