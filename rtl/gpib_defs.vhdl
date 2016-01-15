-- IEEE 488.1 definitions
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

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package gpib_defs is

  function max(left, right: integer) return integer;

  function to_std_logic(v: boolean) return std_logic;

  -- srlInteger and log2ceil functions are from Karsten Becker on StackExchange
  function srlInteger(arg: integer; s:natural) return integer;
  function log2ceil (L: POSITIVE) return NATURAL;

  -- function states
  type source_handshake_state_t is (SIDS,
                                    SGNS,
                                    SDYS,
                                    STRS,
                                    SWNS,
                                    SIWS);

  type acceptor_handshake_state_t is (AIDS,
                                      ANRS,
                                      ACRS,
                                      ACDS,
                                      AWNS);

  type talker_state_t is (TIDS,
                          TADS,
                          TACS,
                          SPAS);

  -- talker_primary_address_state_t is used by TE only, not by T
  type talker_primary_address_state_t is (TPIS,
                                          TPAS);

  type talker_serial_poll_state_t is (SPIS,
                                      SPMS);

  type listener_state_t is (LIDS,
                            LADS,
                            LACS);

  -- talker_primary_address_state_t is used by LE only, not by L
  type listener_primary_address_state_t is (LPIS,
                                            LPAS);

  type service_request_state_t is (NPRS,
                                   SRQS,
                                   APRS);

  type remote_local_state_t is (LOCS,
                                LWLS,
                                REMS,
                                RWLS);

  type parallel_poll_state_t is (PPIS,
                                 PPSS,
                                 PPAS);

  type parallel_poll_addressed_state_t is (PUCS,
                                           PACS);

  type device_clear_state_t is (DCIS,
                                DCAS);

  type device_trigger_state_t is (DTIS,
                                  DTAS);

  type controller_state_t is (CIDS,
                              CADS,
                              CTRS,
                              CACS,
                              CPWS,
                              CPPS,
                              CSBS,
                              CSHS,
                              CAWS,
                              CSWS);

  type controller_service_requested_state_t is (CSRS,
                                                CSNS);

  type controller_system_control_state_t is (SNAS,
                                             SACS);

  type controller_remote_enable_state_t is (SRIS,
                                            SRNS,
                                            SRAS);

  type controller_interface_clear_state_t is (SIIS,
                                              SINS,
                                              SIAS);

  function DCL(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0))
    return boolean;
  
  function GET(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0))
    return boolean;
  
  function GTL(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0))
    return boolean;
  
  function LLO(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0))
    return boolean;
  
  function MLA(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0);
               signal addr: std_logic_vector(4 downto 0))
    return boolean;
  
  function MSA(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0);
               signal sec_addr_v: std_logic_vector(31 downto 0))
    return boolean;
  
  function MTA(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0);
               signal addr: std_logic_vector(4 downto 0))
    return boolean;
  
  function OSA(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0);
               signal sec_addr_v: std_logic_vector(31 downto 0))
    return boolean;
  
  function OTA(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0);
               signal addr: std_logic_vector(4 downto 0))
    return boolean;
  
  function PCG(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0))
    return boolean;
  
  function PPC(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0))
    return boolean;
  
  function PPD(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0))
    return boolean;
  
  function PPE(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0))
    return boolean;
  
  function PPU(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0))
    return boolean;
  
  function SCG(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0))
    return boolean;
  
  function SDC(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0))
    return boolean;
  
  function SPD(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0))
    return boolean;
  
  function SPE(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0))
    return boolean;
  
  function TAG(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0))
    return boolean;
  
  function TCT(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0))
    return boolean;
  
  function UNL(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0))
    return boolean;
  
end gpib_defs;

package body gpib_defs is

  function max(left, right: integer) return integer is
  begin
    if left > right then
      return left;
    else
      return right;
    end if;
  end function max;

  function to_std_logic(v: boolean) return std_logic is
  begin
    if v then
      return '1';
    else
      return '0';
    end if;
  end function to_std_logic;

  -- srlInteger and log2ceil functions are from Karsten Becker on StackExchange
  function srlInteger(arg: integer; s:natural) return integer is
  begin
    return to_integer(SHIFT_RIGHT(to_UNSIGNED(ARG,32), s));
  end srlInteger;

  function log2ceil (L: POSITIVE) return NATURAL is
    variable i, bitCount : natural;
  begin
    i := L-1;
    bitCount:=0;
    while (i > 0) loop
        bitCount := bitCount + 1;
        i:=srlInteger(i,1);
    end loop;
    return bitCount;
  end log2ceil;

  function DCL(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0))
    return boolean is
  begin
    return ATN = '1' and std_match(multiline, "X0010100");
  end function DCL;

  function GET(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0))
    return boolean is
  begin
    return ATN = '1' and std_match(multiline, "X0001000");
  end function GET;

  function GTL(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0))
    return boolean is
  begin
    return ATN = '1' and std_match(multiline, "X0000001");
  end function GTL;

  function LLO(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0))
    return boolean is
  begin
    return ATN = '1' and std_match(multiline, "X0010001");
  end function LLO;

  function MLA(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0);
               signal addr: std_logic_vector(4 downto 0))
    return boolean is
  begin
    -- Xilinx tools can't synthesize std_match where both sides aren't static
    -- return ATN = '1' and std_match(multiline, "X01" & addr);
    return (ATN = '1' and
            std_match(multiline(7 downto 5), "X01") and
            multiline(4 downto 0) = addr);
  end function MLA;
  
  function MSA(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0);
               signal sec_addr_v: std_logic_vector(31 downto 0))
    return boolean is
    variable i: integer range 0 to 31;
  begin
    i := to_integer(unsigned(multiline (4 downto 0)));
    return (ATN = '1' and
            std_match(multiline(7 downto 5), "X11") and
            sec_addr_v (i) = '1');
  end function MSA;
  
  function MTA(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0);
               signal addr: std_logic_vector(4 downto 0))
    return boolean is
  begin
    return (ATN = '1' and
            std_match(multiline(7 downto 5), "X10") and
            multiline (4 downto 0) = addr);
  end function MTA;
  
  function OSA(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0);
               signal sec_addr_v: std_logic_vector(31 downto 0))
    return boolean is
    variable i: integer range 0 to 31;
  begin
    i := to_integer(unsigned(multiline (4 downto 0)));
    return SCG(ATN, multiline) and sec_addr_v (i) = '0';
  end function OSA;
  
  function OTA(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0);
               signal addr: std_logic_vector(4 downto 0))
    return boolean is
  begin
    return TAG(ATN, multiline) and multiline(4 downto 0) /= addr;
  end function OTA;
  
  function PCG(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0))
    return boolean is
  begin
    return ATN = '1' and (std_match(multiline, "X0XXXXXX") or
                          std_match(multiline, "X10XXXXX"));
  end function PCG;

  function PPC(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0))
    return boolean is
  begin
    return ATN = '1' and std_match(multiline, "X0000101");
  end function PPC;

  function PPD(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0))
    return boolean is
  begin
    return ATN = '1' and std_match(multiline, "X111XXXX");
  end function PPD;

  function PPE(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0))
    return boolean is
  begin
    return ATN = '1' and std_match(multiline, "X110XXXX");
  end function PPE;

  function PPU(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0))
    return boolean is
  begin
    return ATN = '1' and std_match(multiline, "X0010101");
  end function PPU;

  function SCG(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0))
    return boolean is
  begin
    return ATN = '1' and std_match(multiline, "X11XXXXX");
  end function SCG;

  function SDC(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0))
    return boolean is
  begin
    return ATN = '1' and std_match(multiline, "X0000100");
  end function SDC;

  function SPD(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0))
    return boolean is
  begin
    return ATN = '1' and std_match(multiline, "X0001001");
  end function SPD;

  function SPE(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0))
    return boolean is
  begin
    return ATN = '1' and std_match(multiline, "X0011000");
  end function SPE;

  function TAG(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0))
    return boolean is
  begin
    return ATN = '1' and std_match(multiline, "X10XXXXX");
  end function TAG;

  function TCT(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0))
    return boolean is
  begin
    return ATN = '1' and std_match(multiline, "X0011001");
  end function TCT;

  function UNL(signal ATN: std_logic;
               signal multiline: std_logic_vector(7 downto 0))
    return boolean is
  begin
    return ATN = '1' and std_match(multiline, "X0111111");
  end function UNL;

end package body gpib_defs;
