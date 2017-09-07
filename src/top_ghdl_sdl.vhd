-- SDL adapter for Konami Arcade Emulator
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

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.all;
use work.sdl_ghdl.all;

-- no-ports top-level entity
entity top_ghdl_sdl is
end entity top_ghdl_sdl;

architecture behaviour of top_ghdl_sdl is

  -- clocks and resets
  signal mclk    : std_logic;
  signal mreset  : std_logic;

  -- video signals
  signal red     : std_logic_vector(7 downto 0);
  signal grn     : std_logic_vector(7 downto 0);
  signal blu     : std_logic_vector(7 downto 0);
  signal xpixel  : std_logic_vector(7 downto 0);
  signal ypixel  : std_logic_vector(7 downto 0);
  signal pxclk   : std_logic;
  signal hsync   : std_logic;
  signal hblank  : std_logic;
  signal phblank : std_logic;
  signal vsync   : std_logic;
  signal vblank  : std_logic;
  signal pvblank : std_logic;

  -- audio signals
  signal audio   : std_logic_vector(17 downto 0);
  signal aclk    : std_logic;

begin  -- behaviour
    -- generate the master reset
  master_reset : process
    variable dummy: integer := 0;
  begin
    dummy := init_sdl(0);
    mreset <= '1';
    wait for 100 us;
    mreset <= '0';
    wait;
  end process master_reset;
  
  -- generate the 36.864 MHz master clock
  master_clk : process
  begin
    loop
      mclk <= '0';
      wait for 13.942 ns;
      mclk <= '1';
      wait for 13.942 ns;
    end loop;
  end process master_clk;

  -- game implementation
  TIME_PILOT : entity work.time_pilot
    generic map (
      LOGINFO => true
    )
    port map (
      mclk      => mclk,
      mreset    => mreset,
      
      p1start   => '1',
      p2start   => '1',
      coin1     => '1',
      coin2     => '1',

      fire      => '1',
      joy_up    => '1',
      joy_down  => '1',
      joy_left  => '1',
      joy_right => '1',

      dip1      => X"ff",
      dip2      => X"f3",

      red       => red,
      grn       => grn,
      blu       => blu,
      hsync     => hsync,
      hblank    => hblank,
      vsync     => vsync,
      vblank    => vblank,
      xpixel    => xpixel,
      ypixel    => ypixel,
      pxclk     => pxclk,

      audio     => audio,
      aclk48k   => aclk
    );

  -- marshal video signal to SDL frame
  -- original screen rotated 90 degrees (
  sdl_video : process(pxclk)
    variable dummy : integer := 0;
  begin
    if pxclk'event and pxclk = '1' then
      if vblank = '0' and hblank = '0' then
        dummy := put_pixel(255 - to_integer(unsigned(ypixel)),
                           to_integer(unsigned(xpixel)),
                           to_integer(unsigned(red)),
                           to_integer(unsigned(grn)),
                           to_integer(unsigned(blu)));
      end if;
    end if;
  end process sdl_video;
  
end behaviour;
