-- A dual-port 2KiB RAM described as recommended by Xilinx
-- (C) Copyright 2011 Christopher D. Kilgour
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

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;

entity dpram2k is
  port(clka : in std_logic;
       clkb : in std_logic;
       ena : in std_logic;
       enb : in std_logic;
       wea : in std_logic;
       web : in std_logic;
       addra : in std_logic_vector(10 downto 0);
       addrb : in std_logic_vector(10 downto 0);
       dia : in std_logic_vector(7 downto 0);
       dib : in std_logic_vector(7 downto 0);
       doa : out std_logic_vector(7 downto 0);
       dob : out std_logic_vector(7 downto 0));
end dpram2k;

architecture syn of dpram2k is
  type ram_type is array (0 to 2047) of std_logic_vector(7 downto 0);
  shared variable RAM : ram_type;
begin
  process (CLKA)
  begin
    if CLKA'event and CLKA = '1' then
      if ENA = '1' then
        if WEA = '1' then
          RAM(conv_integer(ADDRA)) := DIA;
        end if;
        DOA <= RAM(conv_integer(ADDRA));
      end if;
    end if;
  end process;
  process (CLKB)
  begin
    if CLKB'event and CLKB = '1' then
      if ENB = '1' then
        if WEB = '1' then
          RAM(conv_integer(ADDRB)) := DIB;
        end if;
        DOB <= RAM(conv_integer(ADDRB));
      end if;
    end if;
  end process;
end syn;
