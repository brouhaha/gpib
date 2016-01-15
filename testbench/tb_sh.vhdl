-- Testbench for IEEE 488.1 Source Handshake (SH) interface function
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
use std.textio.all;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.tb_util.all;

entity tb_sh is
  generic (stimulus_filename: string := "tb_sh.stim");
end tb_sh;

architecture behavioral of tb_sh is

  -- SH inputs
  signal clock: std_logic;

  signal pon: std_logic;
  signal nba: std_logic;

  signal ATN: std_logic;
  signal RFD: std_logic;
  signal DAC: std_logic;

  signal talker_state: talker_state_t;
  signal controller_state: controller_state_t;

  -- SH outputs
  signal source_handshake_state: source_handshake_state_t;

  signal dev_change_multiline: std_logic;

  signal DAV_out: std_logic;
  signal DAV_en:  std_logic;


  type vector_t is record
    wait_for_state_change: boolean;

    -- SH inputs
    pon: std_logic;
    nba: std_logic;

    ATN: std_logic;
    RFD: std_logic;
    DAC: std_logic;

    talker_state: talker_state_t;
    controller_state: controller_state_t;

  -- SH expected outputs
    exp_sh_state: source_handshake_state_t;

    exp_dev_change_multiline: std_logic;

    exp_DAV_out: std_logic;
    exp_DAV_en:  std_logic;
  end record;

  type vector_access_t is access vector_t;

  procedure parse_vector (l: inout line; v: out vector_access_t) is
    variable vector: vector_access_t;
    variable token: line;
  begin
    vector := new vector_t;

    get_boolean   (l, vector.wait_for_state_change);

    -- SH inputs
    get_std_logic (l, vector.pon);
    get_std_logic (l, vector.nba);
    get_std_logic (l, vector.ATN);
    get_std_logic (l, vector.RFD);
    get_std_logic (l, vector.DAC);

    get_token (l, token);
    vector.talker_state := talker_state_t'value(token.all);

    get_token (l, token);
    vector.controller_state := controller_state_t'value(token.all);

    -- SH expected outputs
    get_token (l, token);
    vector.exp_sh_state := source_handshake_state_t'value(token.all);

    get_std_logic (l, vector.exp_dev_change_multiline);
    get_std_logic (l, vector.exp_DAV_out);
    get_std_logic (l, vector.exp_DAV_en);

    assert l'length = 0 report "extranous fields in vector" severity error;

    v := vector;
  end parse_vector;


begin
  source_handshake: entity work.gpib_sh (rtl)
    generic map (clock_cycle_time => 20 ns)
    port map (clock   => clock,
              pon     => pon,
              nba     => nba,
              dev_change_multiline => dev_change_multiline,
              ATN     => ATN,
              RFD     => RFD,
              DAC     => DAC,
              DAV_out => DAV_out,
              DAV_en  => DAV_en,
              source_handshake_state => source_handshake_state,
              talker_state           => talker_state,
              controller_state       => controller_state);

  process
    variable line_number:  integer := 0;

    variable vector_count: integer := 0;
    variable pass_count:   integer := 0;
    variable fail_count:   integer := 0;

    variable cycle: integer := 0;
    variable prev_source_handshake_state: source_handshake_state_t;

    file stimulus: text open read_mode is stimulus_filename;
    variable l: line;

    variable test_name: line;
    variable vector: vector_access_t;

  begin
    clock <= '0';

    while not endfile (stimulus) loop

      readline (stimulus, l);
      line_number := line_number + 1;
      strip_leading_whitespace (l);
      strip_trailing_comment (l);
      strip_trailing_whitespace (l);

      if l'length = 0 then
        next;
      end if;

      if l (l'low) = '"' then
        assert l (l'high) = '"';
        test_name := new string'(l (l'low + 1 to l'high - 1));
        next;
      end if;

      parse_vector (l, vector);
      vector_count := vector_count + 1;

      pon <= vector.pon;
      nba <= vector.nba;
      ATN <= vector.ATN;
      RFD <= vector.RFD;
      DAC <= vector.DAC;
      talker_state <= vector.talker_state;
      controller_state <= vector.controller_state;

      prev_source_handshake_state := source_handshake_state;

      for j in 1 to 1000 loop
        wait for 5 ns;
        clock <= '0';
        wait for 10 ns;
        clock <= '1';
        wait for 5 ns;

        cycle := cycle + 1;

        exit when not vector.wait_for_state_change;

        if source_handshake_state = vector.exp_sh_state then
          report "state matched after " & integer'image(j) & " cycles" severity note;
          exit;
        elsif source_handshake_state /= prev_source_handshake_state then
          exit;
        end if;
      end loop;

      if (source_handshake_state = vector.exp_sh_state and
          dev_change_multiline = vector.exp_dev_change_multiline and
          DAV_out = vector.exp_DAV_out and
          DAV_en = vector.exp_DAV_en) then
        pass_count := pass_count + 1;
      else
        fail_count := fail_count + 1;
        report "line number: " & integer'image (line_number) & " test name: " & test_name.all;
        report "cycle: " & integer'image (cycle) severity note;

        if source_handshake_state /= vector.exp_sh_state then
          report ("unexpected source_handshake_state, expected " &
                  source_handshake_state_t'image (vector.exp_sh_state) &
                  " actual " &
                  source_handshake_state_t'image (source_handshake_state))
            severity error;
        end if;
        if dev_change_multiline /= vector.exp_dev_change_multiline then
          report ("unexpected dev_change_multiline, expected " &
                  std_logic'image (vector.exp_dev_change_multiline) &
                  " actual " &
                  std_logic'image (dev_change_multiline))
            severity error;
        end if;
        if DAV_out /= vector.exp_DAV_out then
          report ("unexpected DAV_out, expected " &
                  std_logic'image (vector.exp_DAV_out) &
                  " actual " &
                  std_logic'image (DAV_out))
            severity error;
        end if;
        if DAV_en  /= vector.exp_DAV_en  then
          report ("unexpected DAV_en,  expected " &
                  std_logic'image (vector.exp_DAV_en) &
                  " actual " &
                  std_logic'image (DAV_en))
            severity error;
        end if;
      end if;
    end loop;

    assert pass_count + fail_count = vector_count;
    report (integer'image (vector_count) & " vectors, " &
            integer'image (pass_count)   & " passed, " &
            integer'image (fail_count)   & " failed");
    wait; -- ends simulation
  end process;
end behavioral;
