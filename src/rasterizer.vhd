-- Video rasterizer in VHDL for Konami Arcade Emulator
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
-- Premise: a dual-port block RAM is accessed from a Z80 processor
-- which populates sections with video, color, and sprite data.  Separately,
-- a ROM set loader has populated portions of the dual-port block RAM with
-- static tile and sprite bitmap data.  The rasterizer's job is to scan the
-- data from the second access port, and marshall that data to a 256x256 CRT
-- signal via a triple 8-bit video DAC.

-- 256x256 CRT:
--
-- horizontal:
-- __ _   __ ___________________________________________________
--   |_|_|__|                                                   |_
--    A B C                             D
--
-- D = 256 pixels
-- A =   1 pixel back porch
-- B =  22 pixels sync
-- C =   1 pixel front porch
--     ----------
--     280 pixels
--
-- vertical:
-- __ _   __ ___________________________________________________
--   |_|_|__|                                                   |_
--    E F G                             H
--
-- H = 256 lines
-- E =   8 lines back porch
-- F =   3 lines sync
-- G =  11 lines front porch
--     ---------
--     278 lines
--
-- Video data is held ROMs and in two 2 kiB dual-port RAMs that are byte-addressable
-- in this implementation.  The dual-port RAM is organized as follows:
--
-- sprite tiles ROM     16 KiB
-- static tiles ROM      8 KiB
-- sprite RAM            2 KiB
-- tile RAM              2 KiB
-- sprite palette ROM    256 B
-- tile palette ROM      256 B

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity rasterizer is
 port (
   clk:                 in std_logic;
   reset:               in std_logic;
   rgb_out_red:         out std_logic_vector(7 downto 0);
   rgb_out_grn:         out std_logic_vector(7 downto 0);
   rgb_out_blu:         out std_logic_vector(7 downto 0);
   rgb_out_pixel_clock: out std_logic;
   rgb_out_x:           out std_logic_vector(7 downto 0);
   rgb_hsync:           out std_logic;
   rgb_hblank:          out std_logic;  -- active-high
   rgb_vsync:           out std_logic;
   rgb_vblank:          out std_logic;  -- active-high
   video_line:          out std_logic_vector(7 downto 0);

   sprom_addr:          out std_logic_vector(13 downto 0);
   sprom_data:          in std_logic_vector(7 downto 0);
   strom_addr:          out std_logic_vector(12 downto 0);
   strom_data:          in std_logic_vector(7 downto 0);  
   spram_addr:          out std_logic_vector(9 downto 0);
   spraml_data:         in std_logic_vector(7 downto 0);
   spramh_data:         in std_logic_vector(7 downto 0);
   tiram_addr:          out std_logic_vector(9 downto 0);
   tcram_data:          in std_logic_vector(7 downto 0);
   tvram_data:          in std_logic_vector(7 downto 0);
   pprom_addr:          out std_logic_vector(7 downto 0);
   pprom_data:          in std_logic_vector(3 downto 0);
   tprom_addr:          out std_logic_vector(7 downto 0);
   tprom_data:          in std_logic_vector(3 downto 0)
 );

 attribute SIGIS : string;
 attribute SIGIS of clk                 : signal is "CLK";
 attribute SIGIS of reset               : signal is "RST";
 attribute SIGIS of rgb_out_pixel_clock : signal is "CLK";

end entity rasterizer;

-- Maintain sline which is the scan line of the output signal.

architecture behaviour of rasterizer is

  component pixel_ram
    port (
      clk  : in  std_logic;
      we   : in  std_logic;
      a    : in  std_logic_vector(8 downto 0);
      dpra : in  std_logic_vector(8 downto 0);
      di   : in  std_logic_vector(4 downto 0);
      dpo  : out std_logic_vector(4 downto 0)
    );
  end component;

  for pixel_buf : pixel_ram use entity work.pixel_ram;

  signal pixel_clk : std_logic;  
  signal div8    : unsigned(3 downto 0);
  signal sline   : unsigned(8 downto 0) := (others => '0');  -- scan vertical (278 lines)
  signal vpos    : unsigned(7 downto 0);  -- frame vertical (256x256)
  signal vpos2   : unsigned(7 downto 0);  -- frame vertical (256x256)
  signal vblank  : std_logic;
  signal spixel  : unsigned(8 downto 0) := (others => '0');  -- scan pixel count (280 per line)
  signal hpos    : unsigned(7 downto 0);  -- frame horizontal (256x256)
  signal hblank  : std_logic;
  signal phblank : std_logic;
  signal colour  : std_logic_vector(4 downto 0);

  constant zero8  : unsigned := "00000000";
  constant zero10 : unsigned := "0000000000";
  constant zero5  : std_logic_vector := "00000";

  signal pixel_we : std_logic;
  signal pixel_waddr : std_logic_vector(8 downto 0);
  signal pixel_raddr : std_logic_vector(8 downto 0);
  signal pixel_wdata : std_logic_vector(4 downto 0);
  signal pixel_rdata : std_logic_vector(4 downto 0);

  subtype state_type is unsigned(3 downto 0);
  constant INIT                     : state_type := "0000";

  constant GET_TILE_BYTES           : state_type := "0001";
  constant GET_TILE_PIXEL           : state_type := "0010";
  constant ADVANCE_TILE_PIXEL       : state_type := "0011";

  constant GET_SPRITE_B31_SEL_B20   : state_type := "1000";
  constant GET_SPRITE_B20           : state_type := "1001";
  constant CHECK_SPRITE_ROW         : state_type := "1010";
  constant GET_SPRITE_PIXEL         : state_type := "1011";
  constant ADVANCE_SPRITE_PIXEL     : state_type := "1100";

  -- main rasterizer FSM state
  signal state      : state_type;
  -- desired and current tile plane
  signal dplane, tplane : std_logic;
  -- tile flip x/y
  signal tflipx, tflipy : std_logic;
  -- sprite flip x/y
  signal sflipx, sflipy : std_logic;
  -- tile/sprite bytes as described above
  signal tb0, tb1           : std_logic_vector(7 downto 0);
  signal sb0, sb1, sb2, sb3 : std_logic_vector(7 downto 0);
  -- element index into the tile or sprite ROM
  signal index      : unsigned(4 downto 0);
  -- horizontal pixel position within tile on screen (always left-to-right)
  signal thpos      : unsigned(2 downto 0);
  -- horizontal half-tile offset, reflects whether tile is vertically flipped
  signal thoff      : std_logic;
  -- vertical pixel position within tile ROM: forward or reverse of thpos
  signal tvoff      : std_logic_vector(2 downto 0);
  -- pixel's location in tile ROM byte is 0x88 shifted right by this amount
  signal tpshift    : std_logic_vector(1 downto 0);
  -- palette index for current sprite pixel
  signal spalix     : std_logic_vector(1 downto 0);
  -- active sprite row calculated from sprite's Y coordinate and vertical
  -- orientation (normal and flipped row cached to meet timing) 
  signal sprow, nrow, frow : unsigned(7 downto 0);
  -- active sprite pixel X
  signal sprpx      : unsigned(7 downto 0);
  -- horizontal pixel position within sprite on screen (always left-to-right)
  signal shpos      : unsigned(3 downto 0);
  -- horizontal pixel position within sprite ROM: forward or reverse of shpos
  signal shoff      : unsigned(3 downto 0);
  -- sprite low/high byte select
  signal splohi     : std_logic;

begin  -- behaviour

  rgb_out_pixel_clock <= pixel_clk;
  
  -- free-running pixel position counters and pixel clock
  -- input clock is 36.864 MHz, pixel clock is 4.608 MHz (div 8)
  -- sline is 0..277 and spixel is 0..279
  count: process(clk, reset)
  begin
    if reset = '1' then
      div8      <= "0000";
      spixel    <= (others => '0');
      sline     <= (others => '0');
      pixel_clk <= '0';
    elsif clk'event and clk = '0' then
      case div8 is
        when X"1" => div8 <= X"2";
           if (spixel = 279) then
             spixel <= (others => '0');
             if (sline = 277) then
               sline <= (others => '0');
             else
               sline <= sline + 1;
             end if;
           else
             spixel <= spixel + 1;
           end if;
        when X"3" => div8 <= X"4"; pixel_clk <= '1';
        when X"7" => div8 <= X"0"; pixel_clk <= '0';
        when others => div8 <= div8 + 1;
      end case;
    end if;
  end process count;

  hpos  <= spixel(7 downto 0) when (spixel <= 255) else X"00";
  vpos  <= sline(7 downto 0) when (sline <= 255) else X"00";
  vpos2 <= vpos - 1;

  -- horizontal and vertical sync and blank pulses
  rgb_hsync  <= '0' when ((spixel >= 257) and (spixel < 279)) else '1';
  rgb_vsync  <= '0' when ((sline >= 264) and (sline < 267)) else '1';  
  hblank <= '1' when (spixel > 255) else '0';
  vblank <= '1' when (sline > 255) else '0';
  rgb_vblank <= vblank;
  rgb_hblank <= hblank;

  -- raster position
  rgb_out_x  <= std_logic_vector(hpos);
  video_line <= std_logic_vector(vpos2);

  -----------------------------------------------------------------------------
  -- screen renders flipped on its side:
  --
  -- (origin) 16 lines of black
  --     +-------------------------+0
  --     |                         |0
  -- P   |                         |
  -- U   |                         |T
  -- -   |                         |I
  -- 2   |                         |D
  --     |                         |E
  --     |                         |R
  --     |                         |C
  --     |                         |
  -- E   |                         |
  -- R0  |                         |
  -- O0  |                         |
  -- C0  |                         |
  -- S0  |                         |
  -- -1  |                         |
  -- I   |                         |
  -- H   |                         |
  --     |                         |
  --     |                         |
  --     |                         |
  --  0  |                         |
  -- P0  |                         |
  -- U   |                         |
  -- -   |                         |
  -- 1   |                         |
  --     |                         |
  --     +-------------------------+
  -- 16 lines of black

  -----------------------------------------------------------------------------
  -- tile RAM: 16 bits per playfield tile position, 32x32=1024 tiles
  -- color RAM:
  -- 0x000: b0 .. .. ..    .. .. .. ..
  -- 
  -- 0x3f0: .. .. .. ..    .. .. .. b0
  -- video RAM:
  -- 0x400: b1 .. .. ..    .. .. .. ..
  --
  -- 0x7ff: .. .. .. ..    .. .. .. b1
  --
  -- For each tile: romoff=(((b0 & 0x20) << 3) | b1), pal_base=(b0 & 0x1f),
  -- flipx=(b0 & 0x40), flipy=(b0 & 0x80)
  --
  -- Each static tile is 8x8 pixels, each pixel is 2 bits, so each tile is 16
  -- bytes long.  The 16-byte tile is organized as follows (no flipping).  If
  -- b0_7 is the most-significant bit of byte 0, and bf_0 is the least-significant
  -- bit of byte f, then 2-bit pixels are derived as:
  --
  -- (0,0) is [b0_7,b0_3] .. (3,0) is [b0_4,b0_0] (4,0) is [b8_7,b8_3] .. (7,0) is [b8_4,b8_0]
  --                                             ..
  -- (0,7) is [b7_7,bf_3] .. (3,0) is [b7_4,b7_0] (4,0) is [bf_7,bf_3] .. (7,0) is [bf_4,bf_0]
  --
  -----------------------------------------------------------------------------

  -----------------------------------------------------------------------------
  -- sprite RAM: 32 bits per sprite, 24 sprites
  --
  -- Unlike tile RAM which is organized in a playfield matrix, the sprite data
  -- block describes the x,y location of each sprite as well as other
  -- operational parameters such as the applicable sprite tile in a 32-bit
  -- structure.
  --
  -- 0x010: b0 b1 .. ..    .. .. .. ..
  -- 0x020: .. .. .. ..    .. .. .. ..
  -- 0x030: .. .. .. ..    .. .. b0 b1
  --
  -- 0x410: b2 b3 .. ..    .. .. .. ..
  -- 0x420: .. .. .. ..    .. .. .. ..
  -- 0x430: .. .. .. ..    .. .. b2 b3
  --
  -- For each sprite: y=(241-b3), x=b0, spr_tile=b1, pal_base=(b2 & 0x3f),
  -- flipy=(b2 & 0x80), flipx=~(b2 & 0x40)
  --
  -- Each sprite tile is 16x16 pixels, each pixel is 2 bits, so each tile is 64
  -- contiguous bytes long.  The 64-byte tile is organized as follows (no
  -- flipping). If b00_7 is the most-significant bit of byte 00, and b3f_0 is
  -- the least-significant bit of byte 3f, then 2-bit pixels are derived as:
  --
  -- (0,0)[b00_3,b00_7] .. (3,0)[b00_0,b00_4] .. (7,0)[b08_0,b08_4] .. (b,0)[b10_0,b10_4] .. (f,0)[b18_0,b18_4]
  --                                                            ..
  -- (0,7)[b07_3,b07_7] .. (3,7)[b07_0,b07_4] .. (7,7)[b0f_0,b0f_4] .. (b,7)[b17_0,b17_4] .. (f,7)[b1f_0,b1f_4]
  -- (0,8)[b20_3,b20_7] .. (3,8)[b20_0,b20_4] .. (7,8)[b28_0,b28_4] .. (b,7)[b30_0,b30_4] .. (f,8)[b38_0,b38_4]
  --                                                            ..
  -- (0,f)[b27_3,b27_7] .. (3,f)[b27_0,b27_4] .. (7,f)[b2f_0,b2f_4] .. (b,f)[b37_0,b37_4] .. (f,f)[b3f_0,b3f_4]
  --
  -----------------------------------------------------------------------------

  -- tile flips and plane
  tflipy <= not tb0(7);
  tflipx <= not tb0(6);
  tplane <= tb0(4);
  
  -- 3-bit vertical pixel position within tile
  tvoff <= not std_logic_vector(vpos(2 downto 0)) when (tflipy = '0') else
           std_logic_vector(vpos(2 downto 0));

  -- 1-bit horizontal half-tile position
  thoff <= not std_logic(thpos(2)) when (tflipx = '0') else
           std_logic(thpos(2));

  -- sprite normal and flipped row calculations (within sprite)
  nrow <= (vpos + unsigned(sb3) - 241);         -- normal orientation: vpos - (241 - sy)
  frow <= (unsigned(not sb3) - vpos);           -- flipped orientation: (241 - sy) + 15 - vpos

  -- sprite flips
  sflipy <= sb2(7);
  sflipx <= not sb2(6);
  
  -- 4-bit horizontal pixel position within sprite
  shoff <= shpos when (sflipx = '0') else not shpos;

  -- sprite bitmap ROM address
  sprom_addr <= sb1 & std_logic(sprow(3)) &
                std_logic_vector(shoff(3 downto 2)) &
                std_logic_vector(sprow(2 downto 0));

  -- sprite pixel x position
  sprpx <= unsigned(sb0) + shpos;
  
  -- tile bitmap ROM address
  strom_addr <= tb0(5) & tb1(7 downto 0) & thoff & tvoff;

  -- tile color and video RAM address
  tiram_addr <= std_logic_vector(vpos(7 downto 3)) & std_logic_vector(index);

  -- tile bitmap bit selects
  tpshift <= std_logic_vector(thpos(1 downto 0)) when (tflipx = '0') else
             not std_logic_vector(thpos(1 downto 0));

  -- tile color palette ROM address
  tprom_addr <= '0' & tb0(4 downto 0) & strom_data(4) & strom_data(0) when (tpshift = "00") else
                '0' & tb0(4 downto 0) & strom_data(5) & strom_data(1) when (tpshift = "01") else
                '0' & tb0(4 downto 0) & strom_data(6) & strom_data(2) when (tpshift = "10") else
                '0' & tb0(4 downto 0) & strom_data(7) & strom_data(3);

  -- sprite color palette ROM address
  spalix <= sprom_data(3) & sprom_data(7) when shoff(1 downto 0) = 0 else
            sprom_data(2) & sprom_data(6) when shoff(1 downto 0) = 1 else
            sprom_data(1) & sprom_data(5) when shoff(1 downto 0) = 2 else
            sprom_data(0) & sprom_data(4);
  
  pprom_addr <= sb2(5 downto 0) & spalix;

  -- sprite RAM address
  spram_addr <= "0000" & std_logic_vector(index) & splohi;

  -- Finite state machine to paint a single horizontal line, by:
  --  0. wait for horizontal scan to start
  --  1. emit all static tile rows on plane 0
  --  2. overwrite all non-zero row pixels for all sprites intersecting the line
  --  3. overwrite all static tile rows on plane 1
  fsm: process (clk, reset)
  begin  -- process fsm
    if reset = '1' then
      state  <= INIT;
      tb0    <= X"00"; tb1 <= X"00";
      sb0    <= X"00"; sb1 <= X"00"; sb2 <= X"00"; sb3 <= X"00";
      sprow  <= X"00";
      shpos  <= "0000";
      index  <= "00000";
      thpos  <= "000";
      dplane <= '0';
      splohi <= '0';

      pixel_waddr <= "000000000";
      pixel_wdata <= "00000";
      pixel_we    <= '0';
      
    elsif clk'event and clk = '0' then  -- falling clock edge
      phblank <= hblank;
      
      case state is
        when INIT =>
          if ((phblank = '1') and (hblank = '0')) then
            state <= GET_TILE_BYTES;
            index <= "00000";
            thpos <= "000";
          end if;
        when GET_TILE_BYTES =>
          tb0   <= tcram_data;
          tb1   <= tvram_data;
          state <= GET_TILE_PIXEL;
        when GET_TILE_PIXEL =>
          if (tplane = dplane) then     -- only write pixel data on active plane 
            pixel_waddr <= std_logic(vpos(0)) & std_logic_vector(index) &
                           std_logic_vector(thpos);
            pixel_wdata <= '1' & tprom_data(3 downto 0);
            pixel_we <= '1';
          else
            thpos <= "111";             -- advance to end of tile index
          end if;
          state <= ADVANCE_TILE_PIXEL;
        when ADVANCE_TILE_PIXEL =>
          if ((thpos = 7) and (index = 31)) then
            if dplane = '0' then
              -- keep index at 31, sprite indices decrement from there
              state  <= GET_SPRITE_B31_SEL_B20;
              splohi <= '1';
            else
              dplane <= '0';
              state <= INIT;
            end if;
          elsif (thpos = 7) then
            index <= index + 1;
            state <= GET_TILE_BYTES;
          else
            state <= GET_TILE_PIXEL;
          end if;
          thpos <= thpos + 1;
          pixel_we <= '0';
          
        when GET_SPRITE_B31_SEL_B20 =>
          sb1    <= spraml_data; sb3 <= spramh_data;
          splohi <= '0';
          state  <= GET_SPRITE_B20;
        when GET_SPRITE_B20 =>
          sb0   <= spraml_data; sb2 <= spramh_data;
          shpos <= "0000";
          state <= CHECK_SPRITE_ROW;
        when CHECK_SPRITE_ROW =>
          -- process this sprite if this scan line intersects, skip otherwise
          if ((nrow < 16) and (sflipy = '0')) then
            state <= GET_SPRITE_PIXEL;
            sprow <= nrow;
          elsif ((frow < 16) and (sflipy = '1')) then
            state <= GET_SPRITE_PIXEL;
            sprow <= frow;
          else
            shpos <= "1111";
            state <= ADVANCE_SPRITE_PIXEL;
          end if;
        when GET_SPRITE_PIXEL =>
          -- only write pixel data on active sprite, colour zero is transparent
          if ((sprow < 16) and not (spalix = "00")) then
            pixel_waddr <= std_logic(vpos(0)) & std_logic_vector(sprpx);
            pixel_wdata <= '0' & pprom_data(3 downto 0);
            pixel_we    <= '1';
          end if;
          state <= ADVANCE_SPRITE_PIXEL;
        when ADVANCE_SPRITE_PIXEL =>
          if ((shpos = 15) and (index = 8)) then
            dplane <= '1';
            state  <= GET_TILE_BYTES;
            index  <= "00000";
          elsif (shpos = 15) then
            index  <= index - 1;
            splohi <= '1';
            state  <= GET_SPRITE_B31_SEL_B20;
          else
            state <= GET_SPRITE_PIXEL;
          end if;
          shpos    <= shpos + 1;
          pixel_we <= '0';
          
        when others => null;
      end case;
    end if;
  end process fsm;

  -----------------------------------------------------------------------------

  -- double-buffering pixel RAM holds a single scan line for output while
  -- composing the next one
  pixel_buf : pixel_ram
    port map (
      clk  => clk,
      we   => pixel_we,
      a    => pixel_waddr,
      dpra => pixel_raddr,
      di   => pixel_wdata,
      dpo  => pixel_rdata
    );

  -----------------------------------------------------------------------------
  
  -- output the assembled scan line from double-buffering pixel RAM
  scanout : process(clk, reset)
  begin
    if reset = '1' then
      pixel_raddr <= "000000000";
      colour <= "11111";
    elsif clk'event and clk = '0' then
      pixel_raddr <= std_logic(vpos2(0)) & std_logic_vector(hpos);
      if ((vblank = '1') or (hblank = '1')) then
        colour <= "00000";
      else
        colour <= pixel_rdata;
      end if;
    end if;
  end process scanout;
  
  -----------------------------------------------------------------------------

  -- mapping 5-bit colours to 24-bit RGB
  rgb: process(clk, reset)

    type tColourROM is array(0 to 31) of std_logic_vector(7 downto 0); 
    constant redROM : tColourROM :=
      ( X"00",X"e5",X"e5",X"00",X"e5",X"00",X"95",X"ab",
        X"95",X"00",X"59",X"00",X"74",X"ad",X"70",X"ce",
        X"00",X"e5",X"00",X"00",X"e5",X"00",X"e5",X"00",
        X"e5",X"b5",X"00",X"00",X"00",X"00",X"50",X"ce" ); 
    constant grnROM : tColourROM :=
      ( X"00",X"00",X"74",X"9e",X"e5",X"44",X"95",X"ab",
        X"95",X"ab",X"74",X"00",X"74",X"00",X"00",X"ce",
        X"00",X"00",X"00",X"e5",X"e5",X"ab",X"00",X"59",
        X"74",X"b5",X"00",X"50",X"59",X"74",X"00",X"ce" ); 
    constant bluROM : tColourROM :=
      ( X"00",X"00",X"00",X"00",X"00",X"e5",X"00",X"ab",
        X"95",X"e5",X"00",X"95",X"74",X"ad",X"00",X"ce",
        X"00",X"00",X"e5",X"00",X"00",X"e5",X"e5",X"e5",
        X"00",X"b5",X"59",X"59",X"50",X"e5",X"50",X"ce" );
    
  begin  -- process rgb
    if reset = '1' then
      rgb_out_red <= X"00";
      rgb_out_grn <= X"00";
      rgb_out_blu <= X"00";
    elsif clk'event and clk = '0' then
      rgb_out_red <= redROM( to_integer(unsigned(colour)) );
      rgb_out_grn <= grnROM( to_integer(unsigned(colour)) );
      rgb_out_blu <= bluROM( to_integer(unsigned(colour)) );
    end if;
  end process rgb;
end behaviour;
