-- Tests for test bench utility functions and procedures
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
use std.textio.all;
use ieee.std_logic_1164.all;

use work.tb_util.all;

entity tb_util_test is
end tb_util_test;

architecture behavioral of tb_util_test is
begin
  process
    variable l: line;
    variable token: line;
    variable b: boolean;
    variable sl: std_logic;
  begin
    l := new string'("   abc");
    strip_leading_whitespace (l);
    assert l.all = "abc" report "strip_leading_whitespace failed " & """" & l.all & """" severity error;

    l := new string'("abc");
    strip_leading_whitespace (l);
    assert l.all = "abc" report "strip_leading_whitespace failed " & """" & l.all & """" severity error;

    l := new string'("   abc   ");
    strip_trailing_whitespace (l);
    assert l.all = "   abc" report "strip_trailing_whitespace failed " & """" & l.all & """" severity error;

    l := new string'("   abc");
    strip_trailing_whitespace (l);
    assert l.all = "   abc" report "strip_trailing_whitespace failed " & """" & l.all & """" severity error;

    l := new string'("   abc   ");
    strip_trailing_comment (l);
    assert l.all = "   abc   " report "strip_trailing_comment failed " & """" & l.all & """" severity error;

    l := new string'("   abc # 345  ");
    strip_trailing_comment (l);
    assert l.all = "   abc " report "strip_trailing_comment failed " & """" & l.all & """" severity error;

    l := new string'("# 345  ");
    strip_trailing_comment (l);
    assert l.all = "" report "strip_trailing_comment failed " & """" & l.all & """" severity error;

    l := new string'(" abc def ");
    get_token (l, token);
    assert token.all = "abc" report "get_token failed " & """" & token.all & """" severity error;
    assert l.all = " def " report "get_token failed " & """" & l.all & """" severity error;

    l := new string'("  true 37 ");
    get_boolean (l, b);
    assert b = true report "get_boolean failed " & """" & boolean'image(b) & """" severity error;
    assert l.all = " 37 " report "get_boolean failed " & """" & l.all & """" severity error;

    l := new string'("false");
    get_boolean (l, b);
    assert b = false report "get_boolean failed " & """" & boolean'image(b) & """" severity error;
    assert l.all = "" report "get_boolean failed " & """" & l.all & """" severity error;

    l := new string'(" Z false 12 # foo");
    get_std_logic (l, sl);
    assert sl = 'Z' report "get_std_logic failed " & """" & std_logic'image(sl) & """" severity error;
    assert l.all = " false 12 # foo" report "get_std_logic failed " & """" & l.all & """" severity error;

    wait; -- exit simulation

  end process;
  
end behavioral;
