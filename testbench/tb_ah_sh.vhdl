-- Testbench for IEEE 488.1 Acceptor Handshake (AH) and
-- Source Handshake (SH) interface functions
-- Copyright 2016 Eric Smith <spacewar@gmail.com>

use work.gpib_defs.all;

library ieee;
use std.textio.all;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.tb_util.all;

entity tb_ah_sh is
end tb_ah_sh;

architecture behavioral of tb_ah_sh is

  -- common AH and SH inputs
  signal clock: std_logic;

  signal pon: std_logic;

  signal ATN: std_logic;

  -- AH inputs
  signal rdy: std_logic;
  signal tcs: std_logic;

  signal DAV: std_logic;  -- output by SH

  signal listener_state: listener_state_t;

  -- AH outputs
  signal acceptor_handshake_state: acceptor_handshake_state_t;

  signal RFD_out: std_logic;
  signal RFD_en:  std_logic;
  signal DAC_out: std_logic;
  signal DAC_en:  std_logic;

  -- SH inputs 
  signal nba: std_logic;
 
  signal RFD: std_logic;  -- output by AH
  signal DAC: std_logic;  -- output by AH

  signal talker_state:     talker_state_t;
  signal controller_state: controller_state_t;

  -- SH outputs
  signal source_handshake_state: source_handshake_state_t;

  signal dev_change_multiline: std_logic;

  signal DAV_out: std_logic;
  signal DAV_en:  std_logic;
  
  type vector_t is record
    wait_for_state_change: boolean;

    -- inputs
    clock:          std_logic;

    pon:            std_logic;
    rdy:            std_logic;
    tcs:            std_logic;
    nba:            std_logic;

    ATN:            std_logic;

    listener_state: listener_state_t;
    talker_state:     talker_state_t;
    controller_state: controller_state_t;

    -- expected outputs
    exp_sh_state:             source_handshake_state_t;
    exp_ah_state:             acceptor_handshake_state_t; 
    exp_dev_change_multiline: std_logic;
   
    exp_RFD: std_logic;
    exp_DAV: std_logic;
    exp_DAC: std_logic;
  end record;

  type vector_access_t is access vector_t;

  procedure parse_vector (l: inout line; v: out vector_access_t) is
    variable vector: vector_access_t;
    variable token: line;
  begin
    vector := new vector_t;
    
    get_boolean   (l, vector.wait_for_state_change);

    -- inputs
    get_std_logic (l, vector.pon);
    get_std_logic (l, vector.rdy);
    get_std_logic (l, vector.tcs);
    get_std_logic (l, vector.nba);
    get_std_logic (l, vector.ATN);

    get_token (l, token);
    vector.listener_state := listener_state_t'value(token.all);

    get_token (l, token);
    vector.talker_state := talker_state_t'value(token.all);

    get_token (l, token);
    vector.controller_state := controller_state_t'value(token.all);

    -- expected outputs
    get_token (l, token);
    vector.exp_sh_state := source_handshake_state_t'value(token.all);

    get_token (l, token);
    vector.exp_ah_state := acceptor_handshake_state_t'value(token.all);

    get_std_logic (l, vector.exp_dev_change_multiline);
    get_std_logic (l, vector.exp_RFD);
    get_std_logic (l, vector.exp_DAV);
    get_std_logic (l, vector.exp_DAC);

    assert l'length = 0 report "extraneous fields in vector" severity error;

    v := vector;
  end parse_vector;

begin
  RFD <= RFD_out when RFD_en = '1'
    else '1';

  DAV <= DAV_out when DAV_en = '1'
    else '0';

  DAC <= DAC_out when DAC_en = '1'
    else '1';

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
    variable prev_acceptor_handshake_state: acceptor_handshake_state_t;
    variable prev_source_handshake_state:   source_handshake_state_t;

    file stimulus: text open read_mode is "tb_ah_sh.stim";
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
      nba <= vector.nba;
      ATN <= vector.ATN;
      listener_state   <= vector.listener_state;
      talker_state     <= vector.talker_state;
      controller_state <= vector.controller_state;

      prev_acceptor_handshake_state := acceptor_handshake_state;
      prev_source_handshake_state   := source_handshake_state;

      for j in 1 to 1000 loop
        wait for 5 ns;
        clock <= '0';
        wait for 10 ns;
        clock <= '1';
        wait for 5 ns;

        cycle := cycle + 1;

        exit when not vector.wait_for_state_change;

        if (acceptor_handshake_state = vector.exp_ah_state and
            source_handshake_state   = vector.exp_sh_state) then
          report "states matched after " & integer'image(j) & " cycles" severity note;
          exit;
        elsif (acceptor_handshake_state /= prev_acceptor_handshake_state or
               source_handshake_state   /= prev_source_handshake_state) then
          exit;
        end if;
      end loop;

      if (acceptor_handshake_state = vector.exp_ah_state and
          source_handshake_state  = vector.exp_sh_state and 
          dev_change_multiline = vector.exp_dev_change_multiline and
          RFD = vector.exp_RFD and
          DAV = vector.exp_DAV and
          DAC = vector.exp_DAC) then
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
        if RFD /= vector.exp_RFD then
          report ("unexpected RFD, expected " &
                  std_logic'image (vector.exp_RFD) &
                  " actual " &
                  std_logic'image (RFD))
            severity error;
        end if;
        if DAV /= vector.exp_DAV then
          report ("unexpected DAV, expected " &
                  std_logic'image (vector.exp_DAV) &
                  " actual " &
                  std_logic'image (DAV))
            severity error;
        end if;
        if DAC /= vector.exp_DAC then
          report ("unexpected DAC, expected " &
                  std_logic'image (vector.exp_DAC) &
                  " actual " &
                  std_logic'image (DAC))
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
