-- VHDL for Time Pilot (Konami Arcade Emulator) on Terasic DE10 Lite
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

-- ======================================================================

-- top-level entity for DE-10 Lite
entity TimePilotDE10 is
  --    clocks
  port (ADC_CLK_10      : in std_logic;
        MAX10_CLK1_50   : in std_logic;
        MAX10_CLK2_50   : in std_logic;

        -- pushbuttons
        KEY             : in std_logic_vector(1 downto 0);

        -- slide switches
        SW              : in std_logic_vector(9 downto 0);

        -- discrete LEDs
        LEDR            : out std_logic_vector(9 downto 0);

        -- 7-segment LED displays
        HEX0            : out std_logic_vector(7 downto 0);
        HEX1            : out std_logic_vector(7 downto 0);
        HEX2            : out std_logic_vector(7 downto 0);
        HEX3            : out std_logic_vector(7 downto 0);
        HEX4            : out std_logic_vector(7 downto 0);
        HEX5            : out std_logic_vector(7 downto 0);

        -- expansion header
        GPIO            : inout std_logic_vector(35 downto 0);

        -- Arduino Uno expansion
        ARDUINO_IO      : inout std_logic_vector(15 downto 0);
        ARDUINO_RESET_N : in std_logic;

        -- VGA
        VGA_R           : out std_logic_vector(3 downto 0);
        VGA_G           : out std_logic_vector(3 downto 0);
        VGA_B           : out std_logic_vector(3 downto 0);
        VGA_HS          : out std_logic;
        VGA_VS          : out std_logic;

        -- SDRAM
        DRAM_ADDR       : out std_logic_vector(12 downto 0);
        DRAM_DQ         : inout std_logic_vector(15 downto 0);
        DRAM_BA         : out std_logic_vector(1 downto 0);
        DRAM_LDQM       : out std_logic;
        DRAM_UDQM       : out std_logic;
        DRAM_RAS_N      : out std_logic;
        DRAM_CAS_N      : out std_logic;
        DRAM_CKE        : out std_logic;
        DRAM_CLK        : out std_logic;
        DRAM_WE_N       : out std_logic;
        DRAM_CS_N       : out std_logic;

        -- Accelerometer
        GSENSOR_SDI     : in std_logic;
        GSENSOR_SDO     : out std_logic;
        GSENSOR_CS_n    : out std_logic;
        GSENSOR_SCLK    : out std_logic;
        GSENSOR_INT     : in std_logic_vector(2 downto 1)
        );
end entity TimePilotDE10;

-- ======================================================================

architecture behaviour of TimePilotDE10 is

  -- clocks and resets
  signal clk36864      : std_logic;
  signal clk25000   : std_logic;
  signal clk12288   : std_logic;
  signal tpreset    : std_logic;

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

  -- audio signals
  signal audio   : std_logic_vector(17 downto 0);
  signal aclk    : std_logic;

  -- VGA signals
  signal vga_red   : std_logic_vector(3 downto 0);
  signal vga_grn   : std_logic_vector(3 downto 0);
  signal vga_blu   : std_logic_vector(3 downto 0);
  signal vga_hsync : std_logic;
  signal vga_vsync : std_logic;

  -- audio output signals as per MicroElecktronika WM8731 board
  signal aud_mosi  : std_logic;
  signal aud_dacl  : std_logic;
  signal aud_sda   : std_logic;
  signal aud_scl   : std_logic;
  signal aud_cfg   : std_logic;
  
begin  -- behaviour

  tpreset <= not KEY(0);

  -- PLL for master clocks
  MAIN_CLK : entity work.pll73728
    port map (
      inclk0 => MAX10_CLK1_50,
      c0     => clk36864,             -- 36.864 MHz for time pilot
      c1     => clk25000,             -- 25.000 MHz for VGA conversion
      c2     => clk12288              -- 12.288 MHz for audio codec
    );

  -- time pilot!
  TIME_PILOT : entity work.time_pilot
    port map (
      mclk      => clk36864,
      mreset    => tpreset,
      
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

  -- VGA conversion
  VGA_CONV : entity work.Vid2Vga
    generic map (
      VLINES  => 256,
      HPIXELS => 256,
      VBITS   => 8,
      HBITS   => 8,
      SRCBITS => 8,
      OUTBITS => 4
      )
    port map (
      vreset => tpreset,
      vgaclk => clk25000,

      red    => red,
      grn    => grn,
      blu    => blu,
      hblank => hblank,
      vblank => vblank,
      xpixel => not ypixel,
      ypixel => xpixel,
      pxclk  => pxclk,

      vga_red   => vga_red,
      vga_grn   => vga_grn,
      vga_blu   => vga_blu,
      vga_hsync => vga_hsync,
      vga_vsync => vga_vsync
    );

  -- VGA
  VGA_R  <= vga_red;
  VGA_G  <= vga_grn;
  VGA_B  <= vga_blu;
  VGA_HS <= vga_hsync;
  VGA_VS <= vga_vsync;

  AUDIO_CONV : entity work.wm8731_adapt
     port map (
       areset   => tpreset,

       audio    => audio,
       aclk48k  => aclk,

       aud_sck  => clk12288,
       aud_mosi => aud_mosi,
       aud_dacl => aud_dacl,
       aud_sda  => aud_sda,
       aud_scl  => aud_scl,

       cfg_done => aud_cfg
     );

  -- GPIO header for audio
  GPIO(0) <= clk12288 when (aud_cfg = '1') else 'Z';
  GPIO(1) <= aud_mosi when (aud_cfg = '1') else 'Z';
  GPIO(2) <= aud_dacl when (aud_cfg = '1') else 'Z';
  GPIO(3) <= '0' when ((aud_cfg = '1') and (aud_sda = '0')) else 'Z';
  GPIO(4) <= '0' when ((aud_cfg = '1') and (aud_scl = '0')) else 'Z';

  -- unused outputs
  LEDR <= (others => '0');

  HEX0 <= (others => '1');
  HEX1 <= (others => '1');
  HEX2 <= (others => '1');
  HEX3 <= (others => '1');
  HEX4 <= (others => '1');
  HEX5 <= (others => '1');

  GPIO(35 downto 5) <= (others => 'Z');

  ARDUINO_IO <= (others => 'Z');

  DRAM_ADDR  <= (others => '0');
  DRAM_DQ    <= (others => 'Z');
  DRAM_BA    <= (others => '0');
  DRAM_LDQM  <= '0';
  DRAM_UDQM  <= '0';
  DRAM_RAS_N <= '1';
  DRAM_CAS_N <= '1';
  DRAM_CKE   <= '0';
  DRAM_CLK   <= '0';
  DRAM_WE_N  <= '1';
  DRAM_CS_N  <= '1';

  GSENSOR_SDO  <= 'Z';
  GSENSOR_CS_n <= '1';
  GSENSOR_SCLK <= '0';
  
end behaviour;
