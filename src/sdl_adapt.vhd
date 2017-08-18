-- Adaptation for SDL
-- (C) Copyright 2017 Christopher D. Kilgour
-- 
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
-- of the License, or (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
-- 

package sdl_ghdl is

  function init_sdl(dummy: integer) return integer;
  attribute foreign of init_sdl : function is "VHPIDIRECT init_sdl";

  function put_pixel(x: integer; y: integer;
                     r: integer; g: integer; b: integer) return integer;
  attribute foreign of put_pixel : function is "VHPIDIRECT put_pixel";

end sdl_ghdl;

package body sdl_ghdl is

  -- dummy implementations to satisfy build, actual invocations will go to the
  -- C implementation
  function init_sdl(dummy : integer) return integer is
  begin
    assert false report "VHPI" severity failure;
  end init_sdl;

  function put_pixel(x: integer; y: integer;
                     r: integer; g: integer; b: integer) return integer is
  begin
    assert false report "VHPI" severity failure;
  end put_pixel;

end sdl_ghdl;
