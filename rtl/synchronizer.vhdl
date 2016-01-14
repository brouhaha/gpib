-- dual-rank synchronizer
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

-- This should have a generic parameter for the type of signal to be
-- synchronized, but generic types require VHDL 2008. For now, just
-- use std_logic;
entity synchronizer is
  port (clock:     in  std_logic;
        input:     in  std_logic;
        output:    out std_logic);
end entity synchronizer;

architecture rtl of synchronizer is
  signal r1, r2: std_logic;
begin
  s: process (clock)
  begin
    if rising_edge(clock) then
      r1 <= input;
      r2 <= r1;
    end if;
  end process s;
  output <= r2;
end architecture rtl;
