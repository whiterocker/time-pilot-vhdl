-- Video-to-VGA translation
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
--

-- horizontal: 640 pixels display        0..639
--              16 pixels front porch  640..655
--              96 pixels sync pulse   656..751
--              48 pixels back porch   752..799
--             ---
--             800
--
-- vartical:   480 lines display         0..479
--              10 lines front porch   480..489
--               2 lines sync pulse    490..491
--              33 lines back porch    492..524
--             ---
--             525

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

library work;
use work.all;

use IEEE.math_real.ceil;
use IEEE.math_real.log2;

-- ======================================================================

-- top-level entity for 60Hz video to VGA
entity Vid2Vga is

  generic (
    VLINES  : integer := 256;
    HPIXELS : integer := 256;
    VBITS   : integer := 8;
    HBITS   : integer := 8;
    SRCBITS : integer := 8;
    OUTBITS : integer := 4
  );

  port (
    vreset    : in std_logic;
    vgaclk    : in std_logic;       -- pixel clock for VGA

    -- inputs from video subsystem
    red       : in std_logic_vector((SRCBITS-1) downto 0);
    grn       : in std_logic_vector((SRCBITS-1) downto 0);
    blu       : in std_logic_vector((SRCBITS-1) downto 0);
    hblank    : in std_logic;
    vblank    : in std_logic;
    xpixel    : in std_logic_vector((HBITS-1) downto 0);
    ypixel    : in std_logic_vector((VBITS-1) downto 0);
    pxclk     : in std_logic;

    -- outputs for VGA
    vga_red   : out std_logic_vector((OUTBITS-1) downto 0);
    vga_grn   : out std_logic_vector((OUTBITS-1) downto 0);
    vga_blu   : out std_logic_vector((OUTBITS-1) downto 0);
    vga_hsync : out std_logic;
    vga_vsync : out std_logic
  );

end entity Vid2Vga;

-- ======================================================================

architecture behaviour of Vid2Vga is

  function msb_set_by_width(width: natural) return std_logic_vector is
    variable return_vector: std_logic_vector(width-1 downto 0);
  begin
    for i in return_vector'range loop
      if (i = 0) then
        return_vector(i) := '1';
      else
        return_vector(i) := '1';
      end if;
    end loop;
    return return_vector;
  end;

  -- derive VGA and display parameters from generics
  constant pvinit : unsigned(9 downto 0) := conv_unsigned(480, 10);

  constant pvgry2 : unsigned(9 downto 0) := conv_unsigned((480-VLINES)/2, 10);
  constant pvgry1 : unsigned(9 downto 0) := pvgry2 - (VLINES / 16);
  constant pvgry3 : unsigned(9 downto 0) := conv_unsigned((480+VLINES)/2, 10);
  constant pvgry4 : unsigned(9 downto 0) := pvgry3 + (VLINES / 16);
  
  constant phgry2 : unsigned(9 downto 0) := conv_unsigned((640-HPIXELS)/2, 10);
  constant phgry1 : unsigned(9 downto 0) := phgry2 - (HPIXELS / 16);
  constant phgry3 : unsigned(9 downto 0) := conv_unsigned((640+HPIXELS)/2, 10);
  constant phgry4 : unsigned(9 downto 0) := phgry3 + (HPIXELS / 16);

  constant vgahalf : std_logic_vector((OUTBITS-1) downto 0) := msb_set_by_width(OUTBITS);

  -- signals tracking video and VGA scanning
  signal pvpos   : unsigned(9 downto 0);  -- VGA picture vertical (800x525)
  signal phpos   : unsigned(9 downto 0);  -- VGA picture horizontal (800x525)
  signal fvposp  : unsigned(9 downto 0);  -- frame vertical within VGA
  signal fhposp  : unsigned(9 downto 0);  -- frame horizontal within VGA

  signal blackout  : std_logic;
  signal borderout : std_logic;

  -- frame buffer signals
  signal rgb_wdata : std_logic_vector((3*OUTBITS-1) downto 0);
  signal rgb_waddr : std_logic_vector((VBITS+HBITS-1) downto 0);
  signal rgb_we    : std_logic;
  signal rgb_rdata : std_logic_vector((3*OUTBITS-1) downto 0);
  signal rgb_raddr : std_logic_vector((VBITS+HBITS-1) downto 0);

  -- registered frame buffer pixel
  signal rgb_pixel : std_logic_vector((3*OUTBITS-1) downto 0);

begin

  -- frame buffer
  frame_buffer : entity work.ram
    generic map (
      ADDRESS_WIDTH => VBITS+HBITS,
      DATA_WIDTH    => 3*OUTBITS
    )
    port map (
      clock         => vgaclk,
      data          => rgb_wdata,
      write_address => rgb_waddr,
      read_address  => rgb_raddr,
      we            => rgb_we,
      q             => rgb_rdata
    );

  -- frame buffer writing
  rgb_wdata <= red((SRCBITS-1) downto (SRCBITS-OUTBITS)) &
               grn((SRCBITS-1) downto (SRCBITS-OUTBITS)) &
               blu((SRCBITS-1) downto (SRCBITS-OUTBITS));
  rgb_waddr <= ypixel & xpixel;
  rgb_we    <= not (vblank or hblank) and pxclk;

  -- establish VGA picture positions
  sync_gen : process(vgaclk, vreset) is
  begin
    if vreset = '1' then
      pvpos   <= (others => '0');
      phpos   <= (others => '0');                        
    elsif vgaclk'event and vgaclk = '0' then
      if phpos = 799 then
        phpos <= (others => '0');
        if pvpos = 524 then
          pvpos   <= (others => '0');
        else
          pvpos <= pvpos + 1;
        end if;
      else
        phpos <= phpos + 1;
      end if;
    end if;
  end process;

  -- frame position within VGA picture
  fvposp    <= pvpos - pvgry2;
  fhposp    <= phpos - phgry2;
  rgb_raddr <= std_logic_vector(fvposp((VBITS-1) downto 0)) &
               std_logic_vector(fhposp((HBITS-1) downto 0));

  -- black and gray region detect
  blackout <= '1' when ((pvpos <= pvgry1) or (pvpos > pvgry4) or
                        (phpos <= phgry1) or (phpos > phgry4)) else '0';
  borderout <= '1' when ((pvpos < pvgry2) or (pvpos >= pvgry3) or
                         (phpos < phgry2) or (phpos >= phgry3)) else '0';

  -- pixel register
  pixel : process(vgaclk, vreset) is
  begin
    if vreset = '1' then
      rgb_pixel <= (others => '0');
    elsif vgaclk'event and vgaclk = '1' then
      if (blackout = '1') then
        rgb_pixel <= (others => '0');
      elsif (borderout = '1') then
        rgb_pixel <= vgahalf & vgahalf & vgahalf;
      else
        rgb_pixel <= rgb_rdata;
      end if;
    end if;
  end process;

  -- output VGA colours
  vga_red <= rgb_pixel((3*OUTBITS-1) downto (2*OUTBITS));
  vga_grn <= rgb_pixel((2*OUTBITS-1) downto     OUTBITS);
  vga_blu <= rgb_pixel(  (OUTBITS-1) downto           0);

  -- VGA syncs
  vga_hsync <= '0' when ((phpos >= 656) and (phpos < 752)) else '1';
  vga_vsync <= '0' when ((pvpos >= 490) and (pvpos < 492)) else '1';

end behaviour;
