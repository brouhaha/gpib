-- Testbench for IEEE 488.1 Acceptor Handshake (AH) interface function
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

entity tb_ah is
end tb_ah;

architecture behavioral of tb_ah is

  -- AH inputs
  signal clock: std_logic;

  signal pon: std_logic;
  signal rdy: std_logic;
  signal tcs: std_logic;

  signal ATN: std_logic;
  signal DAV: std_logic;

  signal listener_state: listener_state_t;

  -- AH outputs
  signal acceptor_handshake_state: acceptor_handshake_state_t;

  signal RFD_out: std_logic;
  signal RFD_en:  std_logic;
  signal DAC_out: std_logic;
  signal DAC_en:  std_logic;


  type vector_t is record
    wait_for_state_change: boolean;

    -- AH inputs
    pon:            std_logic;
    rdy:            std_logic;
    tcs:            std_logic;
    ATN:            std_logic;
    DAV:            std_logic;
    listener_state: listener_state_t;

    -- AH expected outputs
    exp_ah_state:   acceptor_handshake_state_t;
    exp_RFD_out:    std_logic;
    exp_RFD_en:     std_logic;
    exp_DAC_out:    std_logic;
    exp_DAC_en:     std_logic;
  end record;

  type vector_access_t is access vector_t;

  procedure parse_vector (l: inout line; v: out vector_access_t) is
    variable vector: vector_access_t;
    variable token: line;
  begin
    vector := new vector_t;
    
    get_boolean   (l, vector.wait_for_state_change);

    -- AH inputs
    get_std_logic (l, vector.pon);
    get_std_logic (l, vector.rdy);
    get_std_logic (l, vector.tcs);
    get_std_logic (l, vector.ATN);
    get_std_logic (l, vector.DAV);

    get_token (l, token);
    vector.listener_state := listener_state_t'value(token.all);

    -- AH expected outputs
    get_token (l, token);
    vector.exp_ah_state := acceptor_handshake_state_t'value(token.all);

    get_std_logic (l, vector.exp_RFD_out);
    get_std_logic (l, vector.exp_RFD_en);
    get_std_logic (l, vector.exp_DAC_out);
    get_std_logic (l, vector.exp_DAC_en);

    assert l'length = 0 report "extraneous fields in vector" severity error;

    v := vector;
  end parse_vector;

begin
  acceptor_handshake: entity work.gpib_ah (rtl)
    generic map (clock_cycle_time => 20 ns)
    port map (clock   => clock,
              pon     => pon,
              rdy     => rdy,
              tcs     => tcs,
              ATN     => ATN,
              DAV     => DAV,
              RFD_out => RFD_out,
              RFD_en  => RFD_en,
              DAC_out => DAC_out,
              DAC_en  => DAC_en,
              acceptor_handshake_state => acceptor_handshake_state,
              listener_state           => listener_state);

  process
    variable line_number:  integer := 0;

    variable vector_count: integer := 0;
    variable pass_count:   integer := 0;
    variable fail_count:   integer := 0;
                                      
    variable cycle: integer := 0;
    variable prev_acceptor_handshake_state: acceptor_handshake_state_t;

    file stimulus: text open read_mode is "tb_ah.stim";
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
      rdy <= vector.rdy;
      tcs <= vector.tcs;
      ATN <= vector.ATN;
      DAV <= vector.DAV;
      listener_state <= vector.listener_state;

      prev_acceptor_handshake_state := acceptor_handshake_state;

      for j in 1 to 1000 loop
        wait for 5 ns;
        clock <= '0';
        wait for 10 ns;
        clock <= '1';
        wait for 5 ns;

        cycle := cycle + 1;

        exit when not vector.wait_for_state_change;

        if acceptor_handshake_state = vector.exp_ah_state then
          report "state matched after " & integer'image(j) & " cycles" severity note;
          exit;
        elsif acceptor_handshake_state /= prev_acceptor_handshake_state then
          exit;
        end if;
      end loop;

      if (acceptor_handshake_state = vector.exp_ah_state and
          RFD_out = vector.exp_RFD_out and
          RFD_en  = vector.exp_RFD_en and
          DAC_out = vector.exp_DAC_out and
          DAC_en  = vector.exp_DAC_en) then
        pass_count := pass_count + 1;
      else
        fail_count := fail_count + 1;
        report "line number: " & integer'image (line_number) & " test name: " & test_name.all;
        report "cycle: " & integer'image (cycle) severity note;

        if acceptor_handshake_state /= vector.exp_ah_state then
          report ("unexpected acceptor_handshake_state, expected " &
                  acceptor_handshake_state_t'image (vector.exp_ah_state) &
                  " actual " &
                  acceptor_handshake_state_t'image (acceptor_handshake_state))
            severity error;
        end if;
        if RFD_out /= vector.exp_RFD_out then
          report ("unexpected RFD_out, expected " &
                  std_logic'image (vector.exp_RFD_out) &
                  " actual " &
                  std_logic'image (RFD_out))
            severity error;
        end if;
        if RFD_en  /= vector.exp_RFD_en  then
          report ("unexpected RFD_en,  expected " &
                  std_logic'image (vector.exp_RFD_en) &
                  " actual " &
                  std_logic'image (RFD_en))
            severity error;
        end if;
        if DAC_out /= vector.exp_DAC_out then
          report ("unexpected DAC_out, expected " &
                  std_logic'image (vector.exp_DAC_out) &
                  " actual " &
                  std_logic'image (DAC_out))
            severity error;
        end if;
        if DAC_en  /= vector.exp_DAC_en  then
          report ("unexpected DAC_en,  expected " &
                  std_logic'image (vector.exp_DAC_en) &
                  " actual " &
                  std_logic'image (DAC_en))
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
