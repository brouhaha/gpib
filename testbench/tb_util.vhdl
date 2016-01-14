-- Test bench utility functions and procedures
-- Copyright 2016 Eric Smith <spacewar@gmail.com>

-- inspired in part by:
--   http://www.stefanvhdl.com/vhdl/vhdl/txt_util.vhd
--   http://www.mrc.uidaho.edu/mrc/people/jff/vhdl_info/txt_util.vhd

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

package tb_util is

  function to_std_logic (c: character) return std_logic;

  function to_character (sl: std_logic) return character;

  function to_string (slv: std_logic_vector) return string;

  procedure strip_leading_whitespace (l: inout line);

  procedure strip_trailing_whitespace (l: inout line);

  procedure strip_trailing_comment (l: inout line);

  procedure get_token (l: inout line; token: out line);

  procedure get_boolean (l: inout line; value: out boolean);

  procedure get_std_logic (l: inout line; value: out std_logic);
  
end tb_util;


package body tb_util is

  function to_std_logic (c: character) return std_logic is
    variable sl: std_logic;
  begin
    case c is
      when 'U' => sl := 'U';
      when 'X' => sl := 'X';
      when '0' => sl := '0';
      when '1' => sl := '1';
      when 'Z' => sl := 'Z';
      when 'W' => sl := 'W';
      when 'L' => sl := 'L';
      when 'H' => sl := 'H';
      when '-' => sl := '-';
      when others => assert false report "character is not valid std_logic" severity error;
    end case;
    return sl;
  end to_std_logic;

  function to_character (sl: std_logic) return character is
    variable c: character;
  begin
    case sl is
      when 'U' => c := 'U';
      when 'X' => c := 'X';
      when '0' => c := '0';
      when '1' => c := '1';
      when 'Z' => c := 'Z';
      when 'W' => c := 'W';
      when 'L' => c := 'L';
      when 'H' => c := 'H';
      when '-' => c := '-';
    end case;
    return c;
  end to_character;

  function to_string (slv: std_logic_vector) return string is
    variable s: string (slv'range);
  begin
    for i in slv'range loop
      s (i) := to_character (slv (i));
    end loop;
    return s;
  end to_string;

  function is_whitespace (c : character) return boolean is
  begin
    return c = ' ' or c = HT;
  end is_whitespace;

  procedure strip_leading_whitespace (l: inout line) is
    variable j: integer := l'high + 1;
  begin
    for i in l'range loop
      if not is_whitespace (l (i)) then
        j := i;
        exit;
      end if;
    end loop;
    l := new string'(l (j to l'high));
  end strip_leading_whitespace;

  procedure strip_trailing_whitespace (l: inout line) is
    variable j: integer := l'low - 1;
  begin
    for i in l'reverse_range loop
      if not is_whitespace (l (i)) then
        j := i;
        exit;
      end if;
    end loop;
    l := new string'(l (l'low to j));
  end strip_trailing_whitespace;

  procedure strip_trailing_comment (l: inout line) is
    variable j: integer := 0;
  begin
    for i in l'reverse_range loop
      if l (i) = '#' then
        j := i;
        exit;
      end if;
    end loop;
    if j /= 0 then
      l := new string'(l (l'low to j - 1));
    end if;
  end strip_trailing_comment;

  procedure get_token (l: inout line; token: out line) is
    variable j: integer;
  begin
    strip_leading_whitespace (l);
    j := l'high;
    for i in l'range loop
      if is_whitespace (l (i)) then
        j := i - 1;
        exit;
      end if;
    end loop;
    token := new string'(l (l'left to j));
    l := new string'(l (j + 1 to l'high));
  end get_token;

  procedure get_boolean (l: inout line; value: out boolean) is
    variable token: line;
  begin
    get_token (l, token);
    value := boolean'value (token.all);
  end get_boolean;

  procedure get_std_logic (l: inout line; value: out std_logic) is
    variable token: line;
  begin
    get_token (l, token);
    assert token'length = 1;
    value := to_std_logic (token(token'low));
  end get_std_logic;

end package body tb_util;
