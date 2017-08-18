-- Pixel RAM for Konami Arcade Emulator
-- (C) Copyright 2011, 2017 Christopher D. Kilgour
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
--
-- Adapted from Xilinx "XST User's Guide" 10.1, "Dual-Port RAM with
-- Synchronous Read"
--         ________
--        /        \
-- DPRA --|        |
--   WE --|        |
--   DI --|        |-- DPO
--    A --|        |
--  CLK --|>       |
--        \________/ 
--
-- DPRA = Dual Port Read Address
--   WE = Write Enable
--   DI = Data Input
--    A = Write Address
--  CLK = Positive-Edge (write) Clock
--  DPO = Dual Port Output
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity pixel_ram is
  port (                                
    clk  : in  std_logic;
    we   : in  std_logic;
    a    : in  std_logic_vector(8 downto 0);
    dpra : in  std_logic_vector(8 downto 0);
    di   : in  std_logic_vector(4 downto 0);
    dpo  : out std_logic_vector(4 downto 0));
end entity pixel_ram;

architecture syn of pixel_ram is

  type pixel_ram_type is array (511 downto 0) of std_logic_vector(4 downto 0);
  signal RAM : pixel_ram_type;
  signal reg_dpra : std_logic_vector(8 downto 0);

begin  -- syn
  process (clk)
  begin  -- process
    if clk'event and clk = '1' then  -- rising clock edge
      if (we = '1') then
        RAM(to_integer(unsigned(a))) <= di;
      end if;
      reg_dpra <= dpra;
    end if;
  end process;
  dpo <= RAM(to_integer(unsigned(reg_dpra)));
end syn;
