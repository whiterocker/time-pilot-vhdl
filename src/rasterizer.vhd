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
-- __        ___________________________________________________
--   |_ _ __|                                                   |_
--    A B C                             D
--
-- A =   2 pixels
-- B =  10 pixels
-- C =   5 pixels
-- D = 256 pixels
--     ----------
--     273 pixels
--
-- vertical:
-- __        ___________________________________________________
--   |_ _ __|                                                   |_
--    E F G                             H
--
-- E =   5 lines
-- F =   1 line
-- G =  10 lines
-- H = 256 lines
--     ---------
--     272 lines
--
-- Video data is held in a 32 kiB dual-port RAM that is byte-addressable in this
-- implementation.  The dual-port RAM is organized as follows:
--
-- 0x0000 to 0x3fff  sprite tiles ROM     16 KiB
-- 0x4000 to 0x5fff  static tiles ROM      8 KiB
-- 0x6000 to 0x67ff  (reserved)            2 KiB
-- 0x6800 to 0x6fff  sprite RAM            2 KiB
-- 0x7000 to 0x77ff  tile RAM              2 KiB
-- 0x7800 to 0x78ff  sprite palette ROM    256 B
-- 0x7900 to 0x79ff  tile palette ROM      256 B
-- 0x7a00 to 0x7fff  (reserved)           

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
   rgb_hsync:           out std_logic;
   rgb_hblank:          out std_logic;  -- active-high
   rgb_vsync:           out std_logic;
   rgb_vblank:          out std_logic;  -- active-high
   video_line:          out std_logic_vector(7 downto 0);
   rram_addr:           out std_logic_vector(14 downto 0);
   rram_dout:           in std_logic_vector(7 downto 0));

 attribute SIGIS : string;
 attribute SIGIS of clk                 : signal is "CLK";
 attribute SIGIS of reset               : signal is "RST";
 attribute SIGIS of rgb_out_pixel_clock : signal is "CLK";

end entity rasterizer;

-- Maintain sline which is the scan line of the output signal.
-- The first sixteen lines, 0..15, are the VSYNC pulse.
-- When sline is 15..270, the line (sline-15) is composed.
-- When sline is 16..271, the line (sline-16) is output.

architecture behaviour of rasterizer is

  component pixel_ram
    port (
      clk  : in  std_logic;
      we   : in  std_logic;
      a    : in  std_logic_vector(8 downto 0);
      dpra : in  std_logic_vector(8 downto 0);
      di   : in  std_logic_vector(4 downto 0);
      dpo  : out std_logic_vector(4 downto 0));
  end component;

  for pixel_buf : pixel_ram use entity work.pixel_ram;

  signal pixel_clk : std_logic;  
  signal div15  : unsigned(3 downto 0);
  signal sline  : unsigned(8 downto 0);  -- scan vertical (272 lines)
  signal vpos   : unsigned(8 downto 0);  -- frame vertical (256x256)
  signal vpos2  : unsigned(8 downto 0);  -- frame vertical (256x256)
  signal vsync  : std_logic;
  signal vblank : std_logic;
  signal spixel : unsigned(8 downto 0);  -- scan pixel count (273 per line)
  signal hpos   : unsigned(7 downto 0);  -- frame horizontal (256x256)
  signal hsync  : std_logic;
  signal hblank : std_logic;
  signal colour : std_logic_vector(4 downto 0);

  constant zero8  : unsigned := "00000000";
  constant zero10 : unsigned := "0000000000";
  constant zero5  : std_logic_vector := "00000";

  signal pixel_we : std_logic;
  signal pixel_waddr : std_logic_vector(8 downto 0);
  signal pixel_raddr : std_logic_vector(8 downto 0);
  signal pixel_wdata : std_logic_vector(4 downto 0);
  signal pixel_rdata : std_logic_vector(4 downto 0);

  subtype state_type is unsigned(4 downto 0);
  constant INIT                     : state_type := "11111";

  constant SELECT_TILE_B0           : state_type := "00000";
  constant SELECT_TILE_B1           : state_type := "00001";
  constant GET_TILE_B0_SEL_B1       : state_type := "00010";
  constant GET_TILE_B1_SEL_PI       : state_type := "00011";
  constant SELECT_TILE_PI           : state_type := "00100";
  constant WAIT_PALETTE_INDEX       : state_type := "00101";  
  constant GET_TILE_PALETTE_INDEX   : state_type := "00110";
  constant WAIT_TILE_PIXEL          : state_type := "00111";
  constant GET_TILE_PIXEL           : state_type := "01000";
  constant ADVANCE_TILE_PIXEL       : state_type := "01001";

  constant SELECT_SPRITE_B2         : state_type := "10000";
  constant GET_SPRITE_B2_SEL_B0     : state_type := "10001";
  constant GET_SPRITE_B0_SEL_B1     : state_type := "10010";
  constant CALC_SPRITE_ROW          : state_type := "10011";
  constant GET_SPRITE_PALETTE_INDEX : state_type := "10100";
  constant GET_SPRITE_B1_SEL_B3     : state_type := "10101";
  constant GET_SPRITE_B3_SEL_PI     : state_type := "10110";
  constant SELECT_SPRITE_PI         : state_type := "10111";
  constant GET_SPRITE_PIXEL         : state_type := "11000";
  constant ADVANCE_SPRITE_PIXEL     : state_type := "11001";
  

  -- main rasterizer FSM state
  signal state      : state_type;
  -- tile plane
  signal plane      : std_logic;
  -- tile/sprite bytes as described above
  signal b0, b1, b2, b3 : std_logic_vector(7 downto 0);
  -- element index into the tile or sprite ROM
  signal index      : unsigned(4 downto 0);
  -- horizontal pixel position within tile on screen (always left-to-right)
  signal thpos      : unsigned(2 downto 0);
  -- horizontal pixel position within tile ROM: forward or reverse of thpos
  signal thoff      : std_logic_vector(2 downto 0);
  -- vertical half-tile offset, reflects whether tile is vertically flipped
  signal tvoff      : std_logic;
  -- pixel's location in tile ROM byte is 0x88 shifted right by this amount
  signal tpshift    : std_logic_vector(1 downto 0);
  -- palette index for current sprite pixel
  signal spalix     : std_logic_vector(1 downto 0);
  -- active sprite row calculated from sprite's Y coordinate and vertical
  -- orientation (normal and flipped row cached to meet timing) 
  signal sprow, nrow, frow : unsigned(7 downto 0);
  -- active sprite pixel X position
  signal sprpx      : unsigned(7 downto 0);
  -- horizontal pixel position within sprite on screen (always left-to-right)
  signal shpos      : unsigned(3 downto 0);
  -- horizontal pixel position within sprite ROM: forward or reverse of shpos
  signal shoff      : unsigned(3 downto 0);

begin  -- behaviour

  rgb_out_pixel_clock <= pixel_clk;
  
  -- free-running pixel position counters and pixel clock
  -- input clock is 73.728 MHz, pixel clock is 4.9152 MHz
  -- sline is  0..271
  -- vpos  is 15..270
  -- vpos2 is 16..271
  count: process(clk, reset)
  begin
    if reset = '1' then
      div15     <= "0000";
      spixel    <= "000000000";
      hpos      <= X"00";
      sline     <= "000000000";
      pixel_clk <= '0';
    elsif clk'event and clk = '0' then
      case div15 is
        when X"1" => div15 <= X"2";
                  if (spixel = 272) then
                    hpos   <= X"00";
                    spixel <= "000000000";
                    if (sline = 271) then
                      sline <= "000000000";
                    else
                      sline <= sline + 1;
                    end if;
                  elsif (spixel > 16) then
                    hpos   <= hpos + 1;
                    spixel <= spixel + 1;
                  else
                    hpos   <= X"00";
                    spixel <= spixel + 1;
                  end if;
        when X"2" => div15 <= X"3"; pixel_clk <= '1';
        when X"9" => div15 <= X"a"; pixel_clk <= '0';
        when X"e" => div15 <= X"0";
        when others => div15 <= div15 + 1;
      end case;
    end if;
  end process count;

  vpos <=
    (sline - 15) when ((sline >= 15) and (sline <= 270)) else
    "000000000";
  vpos2 <=
    (sline - 16) when ((sline >= 16) and (sline <= 271)) else
    "000000000";

  -- horizontal and vertical sync pulses
  sync: process(clk, reset)
  begin  -- process sync
    if reset = '1' then               -- asynchronous reset (active low)
      hsync  <= '1';
      hblank <= '1';
      vsync  <= '1';
      vblank <= '1';
    elsif clk'event and clk = '0' then  -- falling clock edge
      if (spixel <= 16) then
        hblank <= '1';
      else
        hblank <= '0';
      end if;
      if (spixel >= 2) and (spixel <= 11) then
        hsync <= '0';
      else
        hsync <= '1';
      end if;
      if (sline <= 15) then
        vblank <= '1';
      else
        vblank <= '0';
      end if;
      if (sline = 5) then
        vsync <= '0';
      else
        vsync <= '1';
      end if;
    end if;
  end process sync;

  rgb_vsync  <= vsync;
  rgb_vblank <= vblank;
  rgb_hsync  <= hsync;
  rgb_hblank <= hblank;

  -- video line
  video_line <= std_logic_vector(vpos2(7 downto 0));

  -----------------------------------------------------------------------------
  -- tile RAM: 16 bits per playfield tile position, 32x32=1024 tiles
  -- 0x7000: b0 .. .. ..    .. .. .. ..
  --
  -- 0x73f0: .. .. .. ..    .. .. .. b0
  -- 0x7400: b1 .. .. ..    .. .. .. ..
  --
  -- 0x77ff: .. .. .. ..    .. .. .. b1
  --
  -- For each tile: x=(8*((addr & 0x3e0) >> 5)), y=(8*(addr & 0x1f)),
  -- plane=(((b0 & 0x20) << 3) | b1), pal_base=(b0 & 0x1f),
  -- flipx=(b0 & 0x40), flipy=(b0 & 0x80)
  --
  -- Each static tile is 8x8 pixels, each pixel is 2 bits, so each tile is 16
  -- bytes long.  The 16-byte tile is organized as follows.  If b0_7 is the
  -- most-significant bit of byte 0, and bf_0 is the least-significant bit of
  -- byte f, then 2-bit pixels are derived as:
  --
  -- (0,0) is [b7_7,b7_3] .. (7,0) is [b0_7,b0_3]
  --                      ..
  -- (0,3) is [b7_4,b7_0] .. (7,3) is [b0_4,b0_0]
  -- (0,4) is [bf_7,bf_3] .. (7,4) is [b8_7,b8_3]
  --                      ..
  -- (0,7) is [bf_4,bf_0] .. (7,7) is [b8_4,b8_0]
  --
  -- Therefore, each tile row requires a maximum of 10 byte reads, with 32
  -- tiles per line, each line requires 320 read cycles (per plane).
  -----------------------------------------------------------------------------

  -----------------------------------------------------------------------------
  -- sprite RAM: 32 bits per sprite, 24 sprites
  --
  -- Unlike tile RAM which is organized in a playfield matrix, the sprite data
  -- block describes the x,y location of each sprite as well as other
  -- operational parameters such as the applicable sprite tile in the 32-bit
  -- structure.
  --
  -- 0x6810: b0 b1 .. ..    .. .. .. ..
  -- 0x6820: .. .. .. ..    .. .. .. ..
  -- 0x6830: .. .. .. ..    .. .. b0 b1
  --
  -- 0x6c10: b2 b3 .. ..    .. .. .. ..
  -- 0x6c20: .. .. .. ..    .. .. .. ..
  -- 0x6c30: .. .. .. ..    .. .. b2 b3
  --
  -- For each sprite: x=b3, y=b0, spr_tile=b1, pal_base=(b2 & 0x3f),
  -- flipx=(b2 & 0x80), flipy=(b2 & 0x40)
  --
  -- Each sprite tile is 16x16 pixels, each pixel is 2 bits, so each tile is 64
  -- contiguous bytes long.  The 64-byte tile is organized as follows.  If b00_7
  -- is the most-significant bit of byte 00, and b3f_0 is the least-significant
  -- bit of byte 3f, then 2-bit pixels are derived as:
  --
  -- (00,00) is [b27_7,b27_3] .. (15,00) is [b00_7,b00_3]
  --                          ..
  -- (00,03) is [b27_4,b27_0] .. (15,03) is [b00_4,b00_0]
  -- (00,04) is [b2f_7,b2f_3] .. (15,04) is [b08_7,b08_3]
  --                          ..
  -- (00,07) is [b2f_4,b2f_0] .. (15,07) is [b08_4,b08_0]
  -- (00,08) is [b37_7,b37_3] .. (15,08) is [b10_7,b10_3]
  --                          ..
  -- (00,11) is [b37_4,b37_0] .. (15,11) is [b10_4,b10_0]
  -- (00,12) is [b3f_7,b3f_3] .. (15,12) is [b18_7,b18_3]
  --                          ..
  -- (00,15) is [b3f_4,b3f_0] .. (15,15) is [b18_4,b18_0]
  --
  -- Therefore, each sprite requires a maximum of 20 read cycles, and with 24
  -- sprites checked per line, each line requires 480 read cycles.
  -----------------------------------------------------------------------------
  
  -----------------------------------------------------------------------------
  -- Using the 15x pixel clock, 4080 cycles are available per horizontal line.
  -----------------------------------------------------------------------------

  -- 3-bit horizontal pixel position within tile
  thoff <= std_logic_vector(thpos) when (b0(6) = '1') else
           not std_logic_vector(thpos); 

  -- 1-bit vertical half-tile position
  tvoff <= std_logic(vpos(2)) when (b0(7) = '0') else
           not std_logic(vpos(2));

  -- 4-bit horizontal pixel position within sprite
  shoff <= shpos when (b2(7) = '1') else not shpos;

  -- Finite state machine to paint a single horizontal line, by:
  --  0. wait for horizontal scan to start
  --  1. emit all static tile rows on plane 0
  --  2. overwrite all non-zero row pixels for all sprites intersecting the line
  --  3. overwrite all static tile rows on plane 1
  fsm: process (clk, reset)
    variable flipoff : unsigned(7 downto 0);
  begin  -- process fsm
    if reset = '1' then
      state  <= INIT;
      b0     <= X"00";
      b1     <= X"00";
      b2     <= X"00";
      sprow  <= X"00";
      nrow   <= X"00";
      frow   <= X"00";
      sprpx  <= X"00";
      shpos  <= "0000";
      index  <= "00000";
      thpos  <= "000";
      spalix <= "00";
      plane  <= '0';

      tpshift <= "00";

      rram_addr <= "000000000000000";

      pixel_waddr <= "000000000";
      pixel_wdata <= "00000";
      pixel_we    <= '0';
      flipoff := X"00";
      
    elsif clk'event and clk = '0' then  -- falling clock edge
      case state is
        when INIT =>
          if (spixel = 1) then
            state <= SELECT_TILE_B0;
            index <= "00000";
            thpos <= "000";
          end if;
        when SELECT_TILE_B0 =>
          rram_addr <= "11100" & not std_logic_vector(index) &
                      std_logic_vector(vpos(7 downto 3));
          state <= GET_TILE_B0_SEL_B1;
        when GET_TILE_B0_SEL_B1 =>
          rram_addr <= "11101" & not std_logic_vector(index) &
                      std_logic_vector(vpos(7 downto 3));
          if (rram_dout(4) /= plane) then
            thpos <= "111";
            state <= ADVANCE_TILE_PIXEL;
          else
            b0 <= rram_dout;
            if rram_dout(7) = '0' then
              tpshift <= std_logic_vector(vpos(1 downto 0));
            else
              tpshift <= not std_logic_vector(vpos(1 downto 0)); 
            end if;
            state <= GET_TILE_B1_SEL_PI;
          end if;  
        when GET_TILE_B1_SEL_PI =>
          b1 <= rram_dout;
          rram_addr <= "10" & b0(5) & rram_dout(7 downto 0) & tvoff & thoff;
          state <= GET_TILE_PALETTE_INDEX;
        when SELECT_TILE_PI =>
          rram_addr <= "10" & b0(5) & b1(7 downto 0) & tvoff & thoff;
          state <= GET_TILE_PALETTE_INDEX;
        when GET_TILE_PALETTE_INDEX =>
          if tpshift = "00" then
            rram_addr <= "11110010" & b0(4 downto 0) & rram_dout(3) &
                        rram_dout(7);
          elsif tpshift = "01" then
            rram_addr <= "11110010" & b0(4 downto 0) & rram_dout(2) &
                        rram_dout(6);
          elsif tpshift = "10" then
            rram_addr <= "11110010" & b0(4 downto 0) & rram_dout(1) &
                        rram_dout(5);
          elsif tpshift = "11" then
            rram_addr <= "11110010" & b0(4 downto 0) & rram_dout(0) &
                        rram_dout(4);
          end if;
          state <= GET_TILE_PIXEL;
        when GET_TILE_PIXEL =>
          if (b0(4) = plane) then       -- only write pixel data on active
                                        -- plane 
            pixel_waddr <= std_logic(vpos(0)) & std_logic_vector(index) &
                           std_logic_vector(thpos);
            pixel_wdata <= '1' & rram_dout(3 downto 0);
            pixel_we <= '1';
          end if;
          state <= ADVANCE_TILE_PIXEL;
        when ADVANCE_TILE_PIXEL =>
          if ((thpos = 7) and (index = 31)) then
            if plane = '0' then
              -- keep index at 31, sprite indices decrement from there
              state <= SELECT_SPRITE_B2;
              shpos <= "0000";
            else
              plane <= '0';
              state <= INIT;
            end if;
          elsif (thpos = 7) then
            index <= index + 1;
            state <= SELECT_TILE_B0;
          else
            state <= SELECT_TILE_PI;
          end if;
          thpos <= thpos + 1;
          pixel_we <= '0';

        when SELECT_SPRITE_B2 =>
          rram_addr <= "110110000" & std_logic_vector(index) & '0';
          state <= GET_SPRITE_B2_SEL_B0;
        when GET_SPRITE_B2_SEL_B0 =>
          b2 <= rram_dout;
          rram_addr <= "110100000" & std_logic_vector(index) & '0';
          state <= GET_SPRITE_B0_SEL_B1;
          flipoff := 15 - vpos(7 downto 0); -- determine this partial value at this state
                                            -- to improve timing closure on frow
        when GET_SPRITE_B0_SEL_B1 =>
          -- retain the raw b0
          b0 <= rram_dout;
          rram_addr <= "110100000" & std_logic_vector(index) & '1';
          state <= CALC_SPRITE_ROW;
        when CALC_SPRITE_ROW =>
          nrow <= (vpos(7 downto 0) - unsigned(b0));   -- normal orientation
          frow <= (flipoff + unsigned(b0));  -- flipped orientation   
          state <= GET_SPRITE_B1_SEL_B3;
        when GET_SPRITE_B1_SEL_B3 =>
          b1 <= rram_dout;
          rram_addr <= "110110000" & std_logic_vector(index) & '1';
          -- process this sprite if this scan line intersects, skip otherwise
          if ((nrow < 16) and (b2(6) = '1')) then
            state <= GET_SPRITE_B3_SEL_PI;
            sprow <= nrow;
          elsif ((frow < 16) and (b2(6) = '0')) then
            state <= GET_SPRITE_B3_SEL_PI;
            sprow <= frow;
          else
            shpos <= "1111";
            state <= ADVANCE_SPRITE_PIXEL;
          end if;
        when GET_SPRITE_B3_SEL_PI =>
          b3 <= rram_dout;
          rram_addr <= '0' & b1 & std_logic(shpos(3)) &
                      std_logic_vector(sprow(3 downto 2)) &
                      std_logic_vector(shpos(2 downto 0));
          state <= GET_SPRITE_PALETTE_INDEX;
        when SELECT_SPRITE_PI =>
          rram_addr <= '0' & b1 & std_logic(shpos(3)) &
                      std_logic_vector(sprow(3 downto 2)) &
                      std_logic_vector(shpos(2 downto 0));
          state <= GET_SPRITE_PALETTE_INDEX;
        when GET_SPRITE_PALETTE_INDEX =>
          if sprow(1 downto 0) = 0 then
            rram_addr <= "1111000" & b2(5 downto 0) & rram_dout(3) &
                        rram_dout(7);
            spalix <= rram_dout(3) & rram_dout(7);
          elsif sprow(1 downto 0) = 1 then
            rram_addr <= "1111000" & b2(5 downto 0) & rram_dout(2) &
                        rram_dout(6);
            spalix <= rram_dout(2) & rram_dout(6);
          elsif sprow(1 downto 0) = 2 then
            rram_addr <= "1111000" & b2(5 downto 0) & rram_dout(1) &
                        rram_dout(5);
            spalix <= rram_dout(1) & rram_dout(5);
          elsif sprow(1 downto 0) = 3 then
            rram_addr <= "1111000" & b2(5 downto 0) & rram_dout(0) &
                        rram_dout(4);
            spalix <= rram_dout(0) & rram_dout(4);
          end if;
          sprpx <= (unsigned(b3) + shoff - 1);
          state <= GET_SPRITE_PIXEL;
        when GET_SPRITE_PIXEL =>
          -- only write pixel data on active sprite, colour zero is transparent
          if ((sprow < 16) and not (spalix = "00")) then
            pixel_waddr <= std_logic(vpos(0)) & std_logic_vector(sprpx);
            pixel_wdata <= '0' & rram_dout(3 downto 0);        
            pixel_we <= '1';
          end if;
          state <= ADVANCE_SPRITE_PIXEL;
        when ADVANCE_SPRITE_PIXEL =>
          if ((shpos = 15) and (index = 8)) then
            plane <= '1';
            state <= SELECT_TILE_B0;
            index <= "00000";
          elsif (shpos = 15) then
            index <= index - 1;
            state <= SELECT_SPRITE_B2;
          else
            state <= SELECT_SPRITE_PI;
          end if;
          shpos <= shpos + 1;
          pixel_we <= '0';
        when others => null;
      end case;
    end if;
  end process fsm;

  -----------------------------------------------------------------------------

  pixel_buf : pixel_ram
    port map (
      clk => clk,
      we => pixel_we,
      a => pixel_waddr,
      dpra => pixel_raddr,
      di => pixel_wdata,
      dpo => pixel_rdata );

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
