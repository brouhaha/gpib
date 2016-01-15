-- IEEE 488.1 interface
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

entity gpib is
  generic (clock_cycle_time: time := 25 ns);
  port (clock:     in  std_logic;

        -- local messages from device functions to interface functions
        gts:       in  std_logic;  -- go to standby (C)
        ist:       in  std_logic;  -- individual status qualifier (PP)
        lon:       in  std_logic;  -- listen only (L, LE)
        lpe:       in  std_logic;  -- local poll enable (PP)
        ltn:       in  std_logic;  -- listen (L, LE)
        lun:       in  std_logic;  -- local unlisten (L, LE)
        nba:       in  std_logic;  -- new byte available (SH)
        pon:       in  std_logic;  -- power on (most functions)
        rdy:       in  std_logic;  -- ready (AH)
        rpp:       in  std_logic;  -- request parallel poll (C)
        rsc:       in  std_logic;  -- request system control (C)
        rsv:       in  std_logic;  -- request service (SR)
        rtl:       in  std_logic;  -- return to local (RL)
        sic:       in  std_logic;  -- send interface clear (C)
        sre:       in  std_logic;  -- send remote enable (C)
        tca:       in  std_logic;  -- take control asynchronously (C)
        tcs:       in  std_logic;  -- take control synchronously (AH, C)
        ton:       in  std_logic;  -- talk only (T, TE)

        -- other data from device functions to interface functions
        addr:      in  std_logic_vector (4 downto 0);
        addr_ext:  in  std_logic;   -- address extention (TE, LE)
                                    -- NOTE: change addr_ext only while
                                    --       pon is true
        sec_addr_v: in  std_logic_vector (31 downto 0);

        data_out:  in  std_logic_vector(7 downto 0); -- data to transmit

        -- local messages from interface functions to device functions
        dev_change_multiline: out std_logic;  -- device can change multiline
                                              -- message (SH)
        dev_send_data: out std_logic; -- device can send one DAB, EOS, END (T)
        dev_send_stb:  out std_logic; -- device can send one STB message (T)
        rem_ctrl:             out std_logic;  -- remote control mode (RL)

        -- GPIB bus (remote messages)
        -- negative logic - true is low voltage, false is higher voltage
        DAV_n:       inout  std_logic;
        NRFD_n:      inout  std_logic;
        NDAC_n:      inout  std_logic;
        IFC_n:       inout  std_logic;
        ATN_n:       inout  std_logic;
        SRQ_n:       inout  std_logic;
        REN_n:       inout  std_logic;
        EOI_n:       inout  std_logic;
        multiline_n: inout  std_logic_vector (7 downto 0);

        -- SN75160/SN75162 buffer control
        buf_TE:    buffer std_logic;  -- talk enable
        buf_PE:    out    std_logic;  -- parallel poll enable (OC drive)
        buf_DC:    buffer std_logic;  -- direction control
        buf_SC:    in     std_logic   -- system controller  
        );
end entity gpib;

architecture rtl of gpib is
  signal source_handshake_state: source_handshake_state_t;
  signal acceptor_handshake_state: acceptor_handshake_state_t;
  signal talker_state: talker_state_t;
  signal talker_primary_address_state: talker_primary_address_state_t;
  signal talker_serial_poll_state: talker_serial_poll_state_t;
  signal listener_state: listener_state_t;
  signal listener_primary_address_state: listener_primary_address_state_t;
  signal service_request_state: service_request_state_t;
  signal remote_local_state: remote_local_state_t;
  signal parallel_poll_state: parallel_poll_state_t;
  signal parallel_poll_addressed_state: parallel_poll_addressed_state_t;
  signal device_clear_state: device_clear_state_t;
  signal device_trigger_state: device_trigger_state_t;
  signal controller_state: controller_state_t;
  signal controller_service_requested_state: controller_service_requested_state_t;
  signal controller_system_control_state: controller_system_control_state_t;
  signal controller_remote_enable_state: controller_remote_enable_state_t;
  signal controller_interface_clear_state: controller_interface_clear_state_t;

  -- unsynchronized bus inputs, positive logic
  signal DAV_in_unsync: std_logic;
  signal RFD_in_unsync: std_logic;
  signal DAC_in_unsync: std_logic;
  signal IFC_in_unsync: std_logic;
  signal ATN_in_unsync: std_logic;
  signal SRQ_in_unsync: std_logic;
  signal REN_in_unsync: std_logic;
  signal EOI_in_unsync: std_logic;
  signal multiline_in_unsync: std_logic_vector(7 downto 0);

  -- synchronized bus inputs, positive logic
  signal DAV_in:       std_logic;
  signal RFD_in:       std_logic;
  signal DAC_in:       std_logic;
  signal IFC_in:       std_logic;
  signal ATN_in:       std_logic;
  signal SRQ_in:       std_logic;
  signal REN_in:       std_logic;
  signal EOI_in:       std_logic;
  signal multiline_in: std_logic_vector(7 downto 0);

  -- bus outputs and enables
  signal DAV_out, DAV_en: std_logic; -- from SH
  signal RFD_out, RFD_en: std_logic; -- from AH
  signal DAC_out, DAC_en: std_logic; -- from AH

  signal ATN_out, ATN_en: std_logic; -- from C

  signal EOI_out, EOI_en: std_logic; -- formed from combination of IDY and END
  signal END_out, END_en: std_logic; -- from T
  signal IDY_out, IDY_en: std_logic; -- from C

  signal SRQ_out, SRQ_en: std_logic; -- from SR
  signal IFC_out, IFC_en: std_logic; -- from C
  signal REN_out, REN_en: std_logic; -- from C

  signal T_multiline_out, C_multiline_out: std_logic_vector(7 downto 0);
  signal T_multiline_en,  C_multiline_en:  std_logic;

  signal PPRn_out:      std_logic_vector(7 downto 0);
  signal PPRn_en:       std_logic;
  
begin
  -- bus input synchronizers

  DAV_in_unsync <= not DAV_n;
  RFD_in_unsync <= NRFD_n;
  DAC_in_unsync <= NDAC_n;
  IFC_in_unsync <= not IFC_n;
  ATN_in_unsync <= not ATN_n;
  SRQ_in_unsync <= not SRQ_n;
  REN_in_unsync <= not REN_n;
  EOI_in_unsync <= not EOI_n;
  multiline_in_unsync <= not multiline_n;
    
  DAV_synchronizer: entity work.synchronizer (rtl)
    port map (clock => clock, input => DAV_in_unsync, output => DAV_in);

  RFD_synchronizer: entity work.synchronizer (rtl)
    port map (clock => clock, input => RFD_in_unsync, output => RFD_in);

  DAC_synchronizer: entity work.synchronizer (rtl)
    port map (clock => clock, input => DAC_in_unsync, output => DAC_in);

  IFC_synchronizer: entity work.synchronizer (rtl)
    port map (clock => clock, input => IFC_in_unsync, output => IFC_in);

  ATN_synchronizer: entity work.synchronizer (rtl)
    port map (clock => clock, input => ATN_in_unsync, output => ATN_in);

  SRQ_synchronizer: entity work.synchronizer (rtl)
    port map (clock => clock, input => SRQ_in_unsync, output => SRQ_in);

  REN_synchronizer: entity work.synchronizer (rtl)
    port map (clock => clock, input => REN_in_unsync, output => REN_in);

  EOI_synchronizer: entity work.synchronizer (rtl)
    port map (clock => clock, input => EOI_in_unsync, output => EOI_in);

  multiline_synchronizer: for i in 0 to 7 generate
    m_synchronizer: entity work.synchronizer (rtl)
      port map (clock => clock, input => multiline_in_unsync (i), output => multiline_in (i));
  end generate multiline_synchronizer;

  -- SN75160/SN75162 buffer control signals
  buf_TE <= (T_multiline_en or -- talker TACS or SPAS
             PPRn_en or        -- parallel poll PPAS
             C_multiline_en);  -- controller CACS or CTRS
         
  buf_PE <= PPRn_en; -- 1 when PP in PPAS

  buf_DC <= ATN_en; -- 0 when C in CIDS or CADS

  -- bus output drivers
  DAV_n <= not DAV_out when buf_TE = '1'  -- for full compliance, use DAV_en = '1'
    else 'Z';                             --   (not compatible with SN75162)

  NRFD_n <=     RFD_out when buf_TE = '0' -- for full compliance, use RFD_en = '1'
     else 'Z';                            --   (not compatible with SN75162)

  NDAC_n <=     DAC_out when buf_TE = '0' -- for full compliance, use DAC_en = '1'
     else 'Z';                            --   (not compatible with SN75162)

  ATN_n <= not ATN_out when ATN_en = '1'  -- equiv to buf_DC, OK for SN75162
    else 'Z';

  EOI_out <= IDY_out when IDY_en = '1'
        else END_out when END_en = '1'
        else '0';

  -- this EIO output enable logic is for use with SN75162
  EOI_en <= to_std_logic((buf_DC = '1' and buf_TE = '1' and ATN_in_unsync = '1')
                      or (buf_DC = '0' and buf_TE = '0' and ATN_in_unsync = '0')
                      or (buf_DC = '0' and buf_TE = '1'));
                                  
  -- for full compliance, use the follwoing (not compatible with SN75162)
  -- EOI_en <= IDY_en or END_en;

  EOI_n <= not EOI_out when EOI_en = '1'
    else 'Z';
  
  SRQ_n <= not SRQ_out when ATN_en = '0' -- for full compliance, use SRQ_en = '1'
    else 'Z';                            --   (not compatible with SN75162)

  IFC_n <= not IFC_out when buf_SC = '1' -- for full compliance, use IFC_en = '1'
    else 'Z';                            --   (not compatible with SN75162)
  
  REN_n <= not REN_out when buf_SC = '1' -- for full compliance, use REN_en = '1'
    else 'Z';                            --   (not compatible with SN75162)
  
  -- If not using SN75160, the bus should be driven open-drain for parallel
  -- poll response, but push-pull for control or talker data output.
  multiline_n <= not PPRn_out when PPRn_en = '1'
          else not C_multiline_out when C_multiline_en = '1'
          else not T_multiline_out when T_multiline_en = '1'
          else (others => 'Z');

  source_handshake: entity work.gpib_sh (rtl)
    generic map (clock_cycle_time => clock_cycle_time)
    port map (clock   => clock,
              pon     => pon,
              nba     => nba,
              dev_change_multiline => dev_change_multiline,
              ATN     => ATN_in,
              RFD     => RFD_in,
              DAC     => DAC_in,
              DAV_out => DAV_out,
              DAV_en  => DAV_en,
              source_handshake_state => source_handshake_state,
              talker_state           => talker_state,
              controller_state       => controller_state);

  acceptor_handshake: entity work.gpib_ah (rtl)
    generic map (clock_cycle_time => clock_cycle_time)
    port map (clock   => clock,
              pon     => pon,
              rdy     => rdy,
              tcs     => tcs,
              ATN     => ATN_in,
              DAV     => DAV_in,
              RFD_out => RFD_out,
              RFD_en  => RFD_en,
              DAC_out => DAC_out,
              DAC_en  => DAC_en,
              acceptor_handshake_state => acceptor_handshake_state,
              listener_state           => listener_state);
    
  talker: entity work.gpib_t (rtl)
    generic map (clock_cycle_time => clock_cycle_time)
    port map (clock    => clock,

              addr       => addr,
              addr_ext   => addr_ext,
              sec_addr_v => sec_addr_v,

              pon      => pon,
              ton      => ton,
              data_out => data_out,

              dev_send_data => dev_send_data,
              dev_send_stb  => dev_send_stb,

              IFC      => IFC_in,
              ATN      => ATN_in,
              multiline     => multiline_in,
              multiline_out => T_multiline_out,
              multiline_en  => T_multiline_en,
              END_out => END_out,
              END_en  => END_en,

              talker_state                   => talker_state,
              talker_primary_address_state   => talker_primary_address_state,
              talker_serial_poll_state       => talker_serial_poll_state,
              acceptor_handshake_state       => acceptor_handshake_state,
              listener_primary_address_state => listener_primary_address_state);
    
  listener: entity work.gpib_l (rtl)
    generic map (clock_cycle_time => clock_cycle_time)
    port map (clock     => clock,

              addr        => addr,
              addr_ext    => addr_ext,
              sec_addr_v  => sec_addr_v,

              pon       => pon,
              ltn       => ltn,
              lun       => lun,
              lon       => lon,
              IFC       => IFC_in,
              ATN       => ATN_in,
              multiline => multiline_in,
              
              listener_state                 => listener_state,
              listener_primary_address_state => listener_primary_address_state,
              acceptor_handshake_state       => acceptor_handshake_state,
              talker_primary_address_state   => talker_primary_address_state,
              controller_state               => controller_state);

  service_request: entity work.gpib_sr (rtl)
    generic map (clock_cycle_time => clock_cycle_time)
    port map (clock     => clock,
              pon       => pon,
              rsv       => rsv,
              SRQ_out   => SRQ_out,
              SRQ_en    => SRQ_en,
              service_request_state => service_request_state,
              talker_state          => talker_state);
    
  remote_local: entity work.gpib_rl (rtl)
    generic map (clock_cycle_time => clock_cycle_time)
    port map (clock     => clock,
              addr      => addr,
              pon       => pon,
              rtl       => rtl,
              ATN       => ATN_in,
              REN       => REN_in,
              multiline => multiline_in,
              rem_ctrl  => rem_ctrl,
              remote_local_state            => remote_local_state,
              acceptor_handshake_state      => acceptor_handshake_state,
              listener_state                => listener_state);

  parallel_poll: entity work.gpib_pp (rtl)
    generic map (clock_cycle_time => clock_cycle_time)
    port map (clock     => clock,
              pon       => pon,
              ist       => ist,
              lpe       => lpe,
              ATN       => ATN_in,
              IDY       => EOI_in,
              multiline => multiline_in,
              PPRn_out  => PPRn_out,
              PPRn_en   => PPRn_en,
              parallel_poll_state           => parallel_poll_state,
              parallel_poll_addressed_state => parallel_poll_addressed_state,
              acceptor_handshake_state      => acceptor_handshake_state,
              listener_state                => listener_state);
    
  device_clear: entity work.gpib_dc (rtl)
    generic map (clock_cycle_time => clock_cycle_time)
    port map (clock     => clock,
              ATN       => ATN_in,
              multiline => multiline_in,
              device_clear_state       => device_clear_state,
              acceptor_handshake_state => acceptor_handshake_state,
              listener_state           => listener_state);

  device_trigger: entity work.gpib_dt (rtl)
    generic map (clock_cycle_time => clock_cycle_time)
    port map (clock     => clock,
              ATN       => ATN_in,
              multiline => multiline_in,
              device_trigger_state     => device_trigger_state,
              acceptor_handshake_state => acceptor_handshake_state,
              listener_state           => listener_state);

  controller: entity work.gpib_c (rtl)
    generic map (clock_cycle_time => clock_cycle_time)
    port map (clock     => clock,

              pon       => pon,
              rsc       => rsc,
              rpp       => rpp,
              gts       => gts,
              tca       => tca,
              tcs       => tcs,
              sic       => sic,
              sre       => sre,
              multiline_msg_out => data_out,

              ATN       => ATN_in,
              ATN_out   => ATN_out,
              ATN_en    => ATN_en,

              IDY_out   => IDY_out,
              IDY_en    => IDY_en,

              IFC       => IFC_in,
              IFC_out   => IFC_out,
              IFC_en    => IFC_en,

              REN_out   => REN_out,
              REN_en    => REN_en,

              SRQ       => SRQ_in,
              
              multiline     => multiline_in,
              multiline_out => C_multiline_out,
              multiline_en  => C_multiline_en,
              
              controller_state                   => controller_state,
              controller_service_requested_state => controller_service_requested_state,
              controller_system_control_state    => controller_system_control_state,
              controller_remote_enable_state     => controller_remote_enable_state,
              controller_interface_clear_state   => controller_interface_clear_state,

              acceptor_handshake_state      => acceptor_handshake_state,
              source_handshake_state      => source_handshake_state,
              talker_state                => talker_state);

end architecture rtl;
