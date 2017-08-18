-- VHDL for Time Pilot (Konami Arcade Emulator)
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

-- top-level entity for time pilot
entity time_pilot is
   port (mclk      : in std_logic;      -- 73.728 MHz
         mreset    : in std_logic;      -- active-high

         -- discrete inputs
         p1start   : in std_logic;
         p2start   : in std_logic;         
         coin1     : in std_logic;
         coin2     : in std_logic;
         
         fire      : in std_logic;
         joy_up    : in std_logic;
         joy_down  : in std_logic;
         joy_left  : in std_logic;
         joy_right : in std_logic;

         dip1      : in std_logic_vector(7 downto 0);
         dip2      : in std_logic_vector(7 downto 0);

         -- video output
         red       : out std_logic_vector(7 downto 0);
         grn       : out std_logic_vector(7 downto 0);
         blu       : out std_logic_vector(7 downto 0);
         hsync     : out std_logic;
         vsync     : out std_logic;
         pxclk     : out std_logic;

         -- audio output
         audio     : out std_logic_vector(17 downto 0);
         aclk48k   : out std_logic
         );
end entity time_pilot;

-- ======================================================================

architecture behaviour of time_pilot is

  -- clocks and resets
  signal mz80_clk          : std_logic;
  signal mz80_reset        : std_logic;
  signal az80_clk          : std_logic;
  signal az80_reset        : std_logic;  
  signal psg_clk           : std_logic;
  signal psg_reset         : std_logic;
  signal ac97_bit_clk      : std_logic;

  -- ROMs
  signal tm1_data_out      : std_logic_vector(7 downto 0);
  signal tm2_data_out      : std_logic_vector(7 downto 0);
  signal tm3_data_out      : std_logic_vector(7 downto 0);
  signal tm4_data_out      : std_logic_vector(7 downto 0);
  signal tm5_data_out      : std_logic_vector(7 downto 0);
  signal tm6_data_out      : std_logic_vector(7 downto 0);
  signal e9_data_out       : std_logic_vector(7 downto 0);
  signal e12_data_out      : std_logic_vector(7 downto 0);
  signal tm7_data_out      : std_logic_vector(7 downto 0);

  -- RAMs
  signal mz80_sprite_we    : std_logic;
  signal mz80_sprite_rdata : std_logic_vector(7 downto 0);
  signal rast_sprite_data  : std_logic_vector(7 downto 0);
  signal mz80_tile_we      : std_logic;
  signal mz80_tile_rdata   : std_logic_vector(7 downto 0);
  signal rast_tile_data    : std_logic_vector(7 downto 0);
  signal mz80_ram_we       : std_logic;
  signal mz80_ram_rdata    : std_logic_vector(7 downto 0);
  signal az80_ram_we       : std_logic;
  signal az80_ram_rdata    : std_logic_vector(7 downto 0);

  -- Main Z80 interface
  signal mz80_addr         : std_logic_vector(15 downto 0);
  signal mz80_data_in      : std_logic_vector(7 downto 0);
  signal mz80_data_out     : std_logic_vector(7 downto 0);
  signal mz80_m1           : std_logic;
  signal mz80_mreq         : std_logic;
  signal mz80_iorq         : std_logic;
  signal mz80_rd           : std_logic;
  signal mz80_wr           : std_logic;
  signal mz80_rfsh         : std_logic;
  signal mz80_halt         : std_logic;
  signal mz80_busak        : std_logic;
  signal mz80_nmi          : std_logic;

  -- rasterizer interface
  signal rast_red          : std_logic_vector(7 downto 0);
  signal rast_grn          : std_logic_vector(7 downto 0);
  signal rast_blu          : std_logic_vector(7 downto 0);
  signal rast_pixel_clk    : std_logic;
  signal rast_hsync        : std_logic;
  signal rast_vsync        : std_logic;
  signal rast_video_line   : std_logic_vector(7 downto 0);
  signal rast_addr         : std_logic_vector(14 downto 0);
  signal rast_data         : std_logic_vector(7 downto 0);

  -- rasterizer integration
  signal mz80_nmi_enable   : std_logic;

  -- audio integration
  signal audio_int         : std_logic;
  signal audio_command     : std_logic_vector(7 downto 0);
  signal audio_filt_1      : std_logic_vector(5 downto 0);
  signal audio_filt_2      : std_logic_vector(5 downto 0);
  signal audio_sample_1    : std_logic_vector(7 downto 0);
  signal audio_sample_2    : std_logic_vector(7 downto 0);
  signal audio_select_1    : std_logic_vector(1 downto 0);
  signal audio_select_2    : std_logic_vector(1 downto 0);
  signal audio_data        : std_logic_vector(17 downto 0);
  signal audio_strobe      : std_logic;
  signal audio_clk         : std_logic;
  signal audio_psgnew      : std_logic;

  -- discrete inputs
  signal iw1               : std_logic_vector(7 downto 0);
  signal iw2               : std_logic_vector(7 downto 0);
  signal iw3               : std_logic_vector(7 downto 0);

  -- Audio Z80 interface
  signal az80_addr         : std_logic_vector(15 downto 0);
  signal az80_data_in      : std_logic_vector(7 downto 0);
  signal az80_data_out     : std_logic_vector(7 downto 0);
  signal az80_m1           : std_logic;
  signal az80_mreq         : std_logic;
  signal az80_iorq         : std_logic;
  signal az80_rd           : std_logic;
  signal az80_wr           : std_logic;
  signal az80_rfsh         : std_logic;
  signal az80_halt         : std_logic;
  signal az80_busak        : std_logic;

  -- Programmable sound generators
  signal psg1_di           : std_logic_vector(7 downto 0);
  signal psg1_do           : std_logic_vector(7 downto 0);
  signal psg1_oe           : std_logic;
  signal psg1_bdir         : std_logic;
  signal psg1_bc1          : std_logic;
  signal psg1_audioA       : std_logic_vector(7 downto 0);
  signal psg1_audioB       : std_logic_vector(7 downto 0);
  signal psg1_audioC       : std_logic_vector(7 downto 0);
  signal psg1_portAo       : std_logic_vector(7 downto 0);
  signal psg1_portBi       : std_logic_vector(7 downto 0);
  signal psg1_portBo       : std_logic_vector(7 downto 0);
  signal psg1_portAoe      : std_logic;
  signal psg1_portBoe      : std_logic;
  signal psg1_awe          : std_logic;
  signal psg1_dwe          : std_logic;

  signal psg2_di           : std_logic_vector(7 downto 0);
  signal psg2_do           : std_logic_vector(7 downto 0);
  signal psg2_oe           : std_logic;
  signal psg2_bdir         : std_logic;
  signal psg2_bc1          : std_logic;
  signal psg2_audioA       : std_logic_vector(7 downto 0);
  signal psg2_audioB       : std_logic_vector(7 downto 0);
  signal psg2_audioC       : std_logic_vector(7 downto 0);
  signal psg2_portAo       : std_logic_vector(7 downto 0);
  signal psg2_portBo       : std_logic_vector(7 downto 0);
  signal psg2_portAoe      : std_logic;
  signal psg2_portBoe      : std_logic;
  signal psg2_awe          : std_logic;
  signal psg2_dwe          : std_logic;

begin  -- behaviour

  -- derive the 3.072 MHz clock for the main Z80
  main_t80_clock : process(mclk, mreset)
    variable lcount : unsigned(7 downto 0);
  begin
    if (mreset = '1') then
      lcount := X"00";
    elsif (mclk'event and mclk = '1') then
      if (lcount = X"0b") then
        mz80_clk <= '1';
        lcount := lcount + 1;
      elsif (lcount = X"17") then
        mz80_clk <= '0';
        lcount := X"00";
      else
        lcount := lcount + 1;
      end if;
    end if;
  end process main_t80_clock;

  -- derive a 1.8432 MHz clock for the audio Z80 (3% faster than original)
  audio_t80_clock : process(mclk, mreset)
    variable lcount : unsigned(7 downto 0);
  begin
    if (mreset = '1') then
      lcount := X"00";
    elsif (mclk'event and mclk = '1') then
      if (lcount = X"13") then
        az80_clk <= '1';
        lcount := lcount + 1;
      elsif (lcount = X"27") then
        az80_clk <= '0';
        lcount := X"00";
      else
        lcount := lcount + 1;
      end if;
    end if;
  end process audio_t80_clock;

  -- derive the 12.288 MHz AC97 bit clock of arbitrary phase
  ac97_clock : process(mclk, mreset)
    variable lcount : unsigned(7 downto 0);
  begin
    if (mreset = '1') then
      lcount := X"00";
    elsif (mclk'event and mclk = '1') then
      if (lcount = X"02") then
        ac97_bit_clk <= '1';
        lcount := lcount + 1;
      elsif (lcount = X"05") then
        ac97_bit_clk <= '0';
        lcount := X"00";
      else
        lcount := lcount + 1;
      end if;
    end if;
  end process ac97_clock;  

  -- =======================================================================
  -- ROMs
  -- =======================================================================  

  TM1_ROM : entity work.tm1
    port map (
      addr => mz80_addr(12 downto 0),
      dout => tm1_data_out
    );

  TM2_ROM : entity work.tm2
    port map (
      addr => mz80_addr(12 downto 0),
      dout => tm2_data_out
    );

  TM3_ROM : entity work.tm3
    port map (
      addr => mz80_addr(12 downto 0),
      dout => tm3_data_out
    );

  TM4_ROM : entity work.tm4
    port map (
      addr => rast_addr(12 downto 0),
      dout => tm4_data_out
    );

  TM5_ROM : entity work.tm5
    port map (
      addr => rast_addr(12 downto 0),
      dout => tm5_data_out
    );

  TM6_ROM : entity work.tm6
    port map (
      addr => rast_addr(12 downto 0),
      dout => tm6_data_out
    );

  E9_ROM : entity work.e9
    port map (
      addr => rast_addr(7 downto 0),
      dout => e9_data_out
    );

  E12_ROM : entity work.e12
    port map (
      addr => rast_addr(7 downto 0),
      dout => e12_data_out
    );

  TM7_ROM : entity work.tm7
    port map (
      addr => az80_addr(11 downto 0),
      dout => tm7_data_out
    );
  
  -- =======================================================================
  -- RAMs
  -- =======================================================================

  SPRITE_RAM : entity work.dpram2k
    port map (
      clka  => mz80_clk,
      clkb  => mclk,
      ena   => '1',
      enb   => '1',
      wea   => mz80_sprite_we,
      web   => '0',
      addra => mz80_addr(10 downto 0),
      addrb => rast_addr(10 downto 0),
      dia   => mz80_data_out,
      dib   => (others => '0'),
      doa   => mz80_sprite_rdata,
      dob   => rast_sprite_data
    );

  mz80_sprite_we <= '1' when ((mz80_mreq = '0') and
                              (mz80_wr = '0') and
                              (mz80_addr(15 downto 11) = "10110"))
                    else '0';

  TILE_RAM : entity work.dpram2k
    port map (
      clka  => mz80_clk,
      clkb  => mclk,
      ena   => '1',
      enb   => '1',
      wea   => mz80_tile_we,
      web   => '0',
      addra => mz80_addr(10 downto 0),
      addrb => rast_addr(10 downto 0),
      dia   => mz80_data_out,
      dib   => (others => '0'),
      doa   => mz80_tile_rdata,
      dob   => rast_tile_data
    );

  mz80_tile_we <= '1' when ((mz80_mreq = '0') and
                            (mz80_wr = '0') and
                            (mz80_addr(15 downto 11) = "10100"))
                  else '0';

  MZ80_RAM : entity work.ram2k
    port map (
      clk => mz80_clk,
      we  => mz80_ram_we,
      a   => mz80_addr(10 downto 0),
      di  => mz80_data_out,
      do  => mz80_ram_rdata
    );

  mz80_ram_we <= '1' when ((mz80_mreq = '0') and
                           (mz80_wr = '0') and
                           (mz80_addr(15 downto 11) = "10101"))
                 else '0';

  AZ80RAM : entity work.ram1k
    port map (
      clk => az80_clk,
      we  => az80_ram_we,
      a   => az80_addr(9 downto 0),
      di  => az80_data_out,
      do  => az80_ram_rdata
    );

  az80_ram_we <= '1' when ((az80_mreq = '0') and
                           (az80_wr = '0') and
                           (az80_addr(15 downto 12) = X"3"))
                 else '0';

  -- =======================================================================
  -- main Z80
  -- =======================================================================

  MZ80 : entity work.t80se
    port map (
      RESET_n => mz80_reset,
      CLK_n   => mz80_clk,
      CLKEN   => '1',
      WAIT_n  => '1',
      INT_n   => '1',
      NMI_n   => mz80_nmi,
      BUSRQ_n => '1',
      M1_n    => mz80_m1,
      MREQ_n  => mz80_mreq,
      IORQ_n  => mz80_iorq,
      RD_n    => mz80_rd,
      WR_n    => mz80_wr,
      RFSH_n  => mz80_rfsh,
      HALT_n  => mz80_halt,
      BUSAK_n => mz80_busak,
      A       => mz80_addr,
      DI      => mz80_data_in,
      DO      => mz80_data_out
    );

  mz80_reset <= not mreset;
  mz80_nmi   <= rast_vsync or not mz80_nmi_enable;
  
  mz80_data_in <=
    tm1_data_out when (mz80_addr(15 downto 13) = "000") else
    tm2_data_out when (mz80_addr(15 downto 13) = "001") else
    tm3_data_out when (mz80_addr(15 downto 13) = "010") else
    mz80_tile_rdata when (mz80_addr(15 downto 11) = "10100") else    -- tile RAM
    mz80_ram_rdata when (mz80_addr(15 downto 11) = "10101") else     -- program RAM
    mz80_sprite_rdata when (mz80_addr(15 downto 11) = "10110") else  -- sprite RAM
    rast_video_line when (mz80_addr(15 downto 4) = X"c00") else      -- video scan line
    dip2 when (mz80_addr(15 downto 4) = X"c20") else  -- DIP switch 2
    iw1 when (mz80_addr(15 downto 4) = X"c30") else   -- inputs 1
    iw2 when (mz80_addr(15 downto 4) = X"c32") else   -- inputs 2
    iw3 when (mz80_addr(15 downto 4) = X"c34") else   -- inputs 3
    dip1 when (mz80_addr(15 downto 4) = X"c36") else  -- DIP switch 1
    X"ff";

  -- Main Z80 integration
  MZ80W : process(mz80_reset, mz80_clk) is
    variable inthold : unsigned(2 downto 0);
  begin
    if (mz80_reset = '0') then
      audio_int      <= '1';
      audio_command  <= X"00";
      mz80_nmi_enable <= '0';
    elsif mz80_clk'event and mz80_clk = '1' then
      if ((mz80_mreq = '0') and (mz80_wr = '0')) then
        if (mz80_addr = X"c000") then
          audio_command <= mz80_data_out;
        elsif (mz80_addr = X"c304") then
          audio_int <= not mz80_data_out(0);
          inthold := "111";
        elsif (mz80_addr = X"c300") then
          mz80_nmi_enable <= mz80_data_out(0);
        elsif (inthold > 0) then
          if (inthold = 1) then
            audio_int <= '1';
          end if;
          inthold := inthold - 1;
        end if;
      end if;
    end if;
  end process;

  iw1 <= "111" & p2start & p1start & '1' & coin2 & coin1;
  iw2 <= "111" & fire & joy_down & joy_up & joy_right & joy_left;
  iw3 <= "111" & fire & joy_down & joy_up & joy_right & joy_left;


  -- =======================================================================
  -- Rasterizer
  -- =======================================================================

  RAST : entity work.rasterizer
    port map (
      clk                 => mclk,
      reset               => mreset,
      rgb_out_red         => rast_red,
      rgb_out_grn         => rast_grn,
      rgb_out_blu         => rast_blu,
      rgb_out_pixel_clock => rast_pixel_clk,
      rgb_hsync           => rast_hsync,
      rgb_vsync           => rast_vsync,
      video_line          => rast_video_line,
      rram_addr           => rast_addr,
      rram_dout           => rast_data
    );

  rast_data <=
    tm4_data_out when (rast_addr(14 downto 13) = "00") else
    tm5_data_out when (rast_addr(14 downto 13) = "01") else
    tm6_data_out when (rast_addr(14 downto 13) = "10") else
    rast_sprite_data when (rast_addr(14 downto 11) = "1101") else
    rast_tile_data when (rast_addr(14 downto 11) = "1110") else
    e9_data_out when (rast_addr(14 downto 8) = "1111000") else
    e12_data_out when (rast_addr(14 downto 8) = "1111001") else
    X"ff";

  red   <= rast_red;
  grn   <= rast_grn;
  blu   <= rast_blu;
  vsync <= rast_vsync;
  hsync <= rast_hsync;
  pxclk <= rast_pixel_clk;

  -- =======================================================================
  -- audio Z80
  -- =======================================================================

  AZ80 : entity work.t80se
    port map (
      RESET_n => az80_reset,
      CLK_n   => az80_clk,
      CLKEN   => '1',
      WAIT_n  => '1',
      INT_n   => audio_int,
      NMI_n   => '1',
      BUSRQ_n => '1',
      M1_n    => az80_m1,
      MREQ_n  => az80_mreq,
      IORQ_n  => az80_iorq,
      RD_n    => az80_rd,
      WR_n    => az80_wr,
      RFSH_n  => az80_rfsh,
      HALT_n  => az80_halt,
      BUSAK_n => az80_busak,
      A       => az80_addr,
      DI      => az80_data_in,
      DO      => az80_data_out
    );

  az80_reset <= not mreset;
  az80_data_in <=
    tm7_data_out when (az80_addr(15 downto 13) = "000") else
    az80_ram_rdata when (az80_addr(15 downto 13) = "001") else
    psg1_do when (az80_addr(15 downto 13) = "010") else
    psg2_do when (az80_addr(15 downto 13) = "011") else
    X"ff";

  -- =======================================================================
  -- Programmable sound generators
  -- =======================================================================

  PSG1 : entity work.ay_3_8910_psg
    port map (
      I_DA       => psg1_di,
      O_DA       => psg1_do,
      O_DA_OE_L  => psg1_oe,            -- ignore, unused
      I_BDIR     => psg1_bdir,
      I_BC2      => '1',
      I_BC1      => psg1_bc1,
      O_AUDIO_A  => psg1_audioA,
      O_AUDIO_B  => psg1_audioB,
      O_AUDIO_C  => psg1_audioC,
      I_IOA      => X"ff",
      O_IOA      => psg1_portAo,  -- ignore, unused
      O_IOA_OE_L => psg1_portAoe, -- ignore, unused
      I_IOB      => psg1_portBi,
      O_IOB      => psg1_portBo,  -- ignore, unused
      O_IOB_OE_L => psg1_portBoe, -- ignore, unused
      ENA        => '1',
      RESET_L    => psg_reset,
      CLK        => psg_clk
    );

  psg_reset <= not mreset;
  
  psg1_awe <= '1' when ((az80_mreq = '0') and
                        (az80_wr = '0') and
                        (az80_addr(15 downto 12) = X"5"))
              else '0';
  psg1_dwe <= '1' when ((az80_mreq = '0') and
                        (az80_wr = '0') and
                        (az80_addr(15 downto 12) = X"4"))              
              else '0';

  -- PSG1 strobe 
  p_psg1io : process(psg_clk, mreset) is
  begin
    if mreset = '1' then
      psg1_bdir <= '0';
      psg1_bc1  <= '0';
      psg1_di   <= (others => '0');
    elsif psg_clk'event and psg_clk = '0' then
      if (psg1_awe = '1') then
        psg1_di   <= az80_data_out;
        psg1_bdir <= '1';
        psg1_bc1  <= '1';
      elsif (psg1_dwe = '1') then
        psg1_di   <= az80_data_out;
        psg1_bdir <= '1';
        psg1_bc1  <= '0';
      else
        psg1_bdir <= '0';
        psg1_bc1  <= '1';
      end if;
    end if;
  end process;

  -- PSG1 Port B inputs
  psg1_portBi(3 downto 0) <= (others => '0');

  p_psg1Bup : process(psg_clk, mreset) is
    variable div512 : unsigned(8 downto 0);
    variable index : unsigned(3 downto 0);
    type NIB10 is array(0 to 9) of std_logic_vector(3 downto 0);
    constant UPPERNIB : NIB10 := (X"0", X"1", X"2", X"3", X"4",
                                  X"9", X"A", X"B", X"A", X"D");
  begin
    if mreset = '1' then
      div512 := (others => '0');
      index  := X"0";
      psg1_portBi(7 downto 4) <= (others => '0');
    elsif psg_clk'event and psg_clk = '1' then
      if div512 = 511 then
        if index = 9 then
          index := X"0";
        else
          index := index + 1;  
        end if;
      end if;
      div512 := div512 + 1;
      psg1_portBi(7 downto 4) <= UPPERNIB(to_integer(index));
    end if;
  end process;

  PSG2 : entity work.ay_3_8910_psg
    port map (
      I_DA       => psg2_di,
      O_DA       => psg2_do,
      O_DA_OE_L  => psg2_oe,            -- ignore, unused
      I_BDIR     => psg2_bdir,
      I_BC2      => '1',
      I_BC1      => psg2_bc1,
      O_AUDIO_A  => psg2_audioA,
      O_AUDIO_B  => psg2_audioB,
      O_AUDIO_C  => psg2_audioC,
      I_IOA      => X"ff",
      O_IOA      => psg2_portAo,  -- ignore, unused
      O_IOA_OE_L => psg2_portAoe, -- ignore, unused
      I_IOB      => X"ff",
      O_IOB      => psg2_portBo,  -- ignore, unused
      O_IOB_OE_L => psg2_portBoe, -- ignore, unused
      ENA        => '1',
      RESET_L    => psg_reset,
      CLK        => psg_clk
    );

  psg2_awe <= '1' when ((az80_mreq = '0') and
                        (az80_wr = '0') and
                        (az80_addr(15 downto 12) = X"7"))
              else '0';
  psg2_dwe <= '1' when ((az80_mreq = '0') and
                        (az80_wr = '0') and
                        (az80_addr(15 downto 12) = X"6"))
              else '0';

  -- PSG2 strobe 
  p_psg2io : process(psg_clk, mreset) is
  begin
    if mreset = '1' then
      psg2_bdir <= '0';
      psg2_bc1  <= '0';
      psg2_di   <= (others => '0');
    elsif psg_clk'event and psg_clk = '0' then
      if (psg2_awe = '1') then
        psg2_di   <= az80_data_out;
        psg2_bdir <= '1';
        psg2_bc1  <= '1';
      elsif (psg2_dwe = '1') then
        psg2_di   <= az80_data_out;
        psg2_bdir <= '1';
        psg2_bc1  <= '0';
      else
        psg2_bdir <= '0';
        psg2_bc1  <= '1';
      end if;
    end if;
  end process;

  -- audio filter selections
  p_filt : process(psg_clk, mreset) is
  begin
    if mreset = '1' then
      audio_filt_1 <= (others => '0');
      audio_filt_2 <= (others => '0');
    elsif psg_clk'event and psg_clk = '1' then
      if ((az80_mreq = '0') and
          (az80_wr = '0') and
          (az80_addr(15) = '1')) then
        audio_filt_1 <= az80_addr(11 downto 6);
        audio_filt_2 <= az80_addr(5 downto 0);
      end if;
    end if;
  end process;

  -- =======================================================================
  -- Resampler/filter
  -- =======================================================================

  RESAMPLER : entity work.resampler
    port map (
      clk         => ac97_bit_clk,
      reset       => mreset,
      psgclk      => psg_clk,
      psgnew      => audio_psgnew,      -- ignored
      sample_1    => audio_sample_1,
      select_1    => audio_select_1,
      chan1_en    => "111",
      chan1_filt  => audio_filt_1, 
      sample_2    => audio_sample_2,
      select_2    => audio_select_2,
      chan2_en    => "111",
      chan2_filt  => audio_filt_2, 
      audio       => audio_data,
      audio_en    => audio_strobe
    );

  audio_sample_1 <= psg1_audioA when (audio_select_1 = "00") else
                    psg1_audioB when (audio_select_1 = "01") else
                    psg1_audioC when (audio_select_1 = "10") else
                    X"00";
  audio_sample_2 <= psg2_audioA when (audio_select_2 = "00") else
                    psg2_audioB when (audio_select_2 = "01") else
                    psg2_audioC when (audio_select_2 = "10") else
                    X"00";

  audio   <= audio_data;
  aclk48k <= audio_strobe;
  
end behaviour;
