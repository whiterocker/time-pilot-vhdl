-- Audio Resampler And Filter for Konami Arcade Chassis Emulator
-- Copyright (C) 2011 Christopher D. Kilgour
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

-- The main clock is 12288000 Hz, where an 18-bit output audio sample is
-- delivered every 256 clocks at 48000 Hz.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity resampler is
 port (
   clk:                 in std_logic;
   reset:               in std_logic;

   psgclk:              out std_logic;
   psgnew:              out std_logic;   -- high when a new sample taken

   sample_1:            in std_logic_vector(7 downto 0);
   select_1:            out std_logic_vector(1 downto 0);
   chan1_en:            in std_logic_vector(2 downto 0);  -- CBA enables
   chan1_filt:          in std_logic_vector(5 downto 0);  -- 2-bits each for CBA

   sample_2:            in std_logic_vector(7 downto 0);
   select_2:            out std_logic_vector(1 downto 0);
   chan2_en:            in std_logic_vector(2 downto 0);  -- CBA enables
   chan2_filt:          in std_logic_vector(5 downto 0);  -- 2-bits each for CBA

   audio:               out std_logic_vector(17 downto 0);
   audio_en:            out std_logic    -- high for one clock when the samples
                                         -- are taken by the sink
 );

 attribute SIGIS : string;
 attribute SIGIS of clk                 : signal is "CLK";
 attribute SIGIS of reset               : signal is "RST";

end entity resampler;

-------------------------------------------------------------------------------

architecture behaviour of resampler is

  signal head : unsigned(4 downto 0);
  signal local_sample_en, start_filt, output_en : std_logic;
  type psgbuf is array (0 to 31) of signed(23 downto 0);
  signal psg1a, psg1b, psg1c, psg2a, psg2b, psg2c : psgbuf;
  signal clk3of5, clk18432 : std_logic;
  signal clk96, clk48 : std_logic;
  signal countB : unsigned(7 downto 0);

  -- 32 bits in 9.23 format
  signal filt1a, filt1b, filt1c,
    filt2a, filt2b, filt2c : signed(31 downto 0);

  -- indices into filters
  signal fix1a, fix1b, fix1c, fix2a, fix2b, fix2c : integer;
  
begin

  -- map filter indices
  fix1a <= to_integer(unsigned(chan1_filt(1 downto 0)));
  fix1b <= to_integer(unsigned(chan1_filt(3 downto 2)));
  fix1c <= to_integer(unsigned(chan1_filt(5 downto 4)));
  fix2a <= to_integer(unsigned(chan2_filt(1 downto 0)));
  fix2b <= to_integer(unsigned(chan2_filt(3 downto 2)));
  fix2c <= to_integer(unsigned(chan2_filt(5 downto 4)));

  -- generate a 3-of-5, 96k and 48k
  gen3of5 : process( clk, reset ) is
    variable countA : unsigned(2 downto 0);
  begin
    if reset = '1' then
      countA := "000";
      countB <= X"00";
      clk3of5 <= '0';
    elsif clk'event and clk = '1' then
      countB <= countB - 1;
      if countA = 4 then
        countA := "000";
      else
        countA := countA + 1;
      end if;
      if (countA = 0) or (countA = 2) or (countA = 4) then
        clk3of5 <= not clk3of5;
      end if;
    end if;
  end process;

  clk96 <= std_logic(countB(6));
  clk48 <= std_logic(countB(7));
  
  -- generate clocks for the PSG chips
  gen18432 : process( clk3of5, reset ) is
  begin
    if reset = '1' then
      clk18432 <= '0';
    elsif clk3of5'event and clk3of5 = '1' then
      clk18432 <= not clk18432;
    end if;
  end process;
  
  -- the PSG sampling runs once every 16 at 115200 kHz
  psggen : process( clk18432, reset ) is
    variable count : unsigned(3 downto 0);
  begin
    if reset = '1' then
      count := "0000";
      local_sample_en <= '0';
    elsif clk18432'event and clk18432 = '1' then
      if (count = 15) then
        local_sample_en <= '1';
      else
        local_sample_en <= '0';
      end if;
      count := count + 1;
    end if;
  end process;

  psgclk <= clk18432;
  psgnew <= local_sample_en;

  -- sample the PSG outputs at 115200 Hz, drop 1-of-6 for 96000 Hz
  -- original samples are in 0.8 unsigned format, which are extended to 1.8 format,
  -- then multiplied by gain in 1.14 format yielding 2.22 format
  psggrab : process( clk, reset ) is
    variable decount : unsigned(3 downto 0);
    type TSTATE is (IDLE, CHANA, CHANB, CHANC, HALT);
    variable state : TSTATE := IDLE;
    -- constant gain in 1.14 format
    type TGAIN is array(0 to 3) of signed(14 downto 0);
    constant GAINS : TGAIN := ( "010" & X"023",  -- 1/1.9913, 24kHz
                                "000" & X"35b",  -- 1/19.070, 723Hz
                                "000" & X"45d",  -- 1/14.656, 3386Hz
                                "000" & X"357"   -- 1/19.144, 596Hz
                                );
  begin
    if reset = '1' then
      head <= "00000";
      decount := "0000";
      state := IDLE;
      select_1 <= "11";
      select_2 <= "11";
    elsif clk'event and clk = '1' then
      case state is
        when IDLE =>
          if local_sample_en = '1' then
            if decount /= 5 then
              decount := decount + 1;
              select_1 <= "00";
              select_2 <= "00";
              state := CHANA;
            else
              decount := "0000";
              state := HALT;
            end if;
          end if;
        when CHANA =>
          if (chan1_en(0) = '1') then
            psg1a(to_integer(head)) <= signed('0' & sample_1) * GAINS(fix1a);
          else
            psg1a(to_integer(head)) <= (others => '0');
          end if;
          if (chan2_en(0) = '1') then
            psg2a(to_integer(head)) <= signed('0' & sample_2) * GAINS(fix2a);
          else
            psg2a(to_integer(head)) <= (others => '0');
          end if;
          select_1 <= "01";
          select_2 <= "01";
          state := CHANB;
        when CHANB =>
          if (chan1_en(1) = '1') then
            psg1b(to_integer(head)) <= signed('0' & sample_1) * GAINS(fix1b);
          else
            psg1b(to_integer(head)) <= (others => '0');
          end if;
          if (chan2_en(1) = '1') then
            psg2b(to_integer(head)) <= signed('0' & sample_2) * GAINS(fix2b);
          else
            psg2b(to_integer(head)) <= (others => '0');
          end if;
          select_1 <= "10";
          select_2 <= "10";
          state := CHANC;
        when CHANC =>
          if (chan1_en(2) = '1') then
            psg1c(to_integer(head)) <= signed('0' & sample_1) * GAINS(fix1c);
          else
            psg1c(to_integer(head)) <= (others => '0');
          end if;
          if (chan2_en(2) = '1') then
            psg2c(to_integer(head)) <= signed('0' & sample_2) * GAINS(fix2c);
          else
            psg2c(to_integer(head)) <= (others => '0');
          end if;
          head <= head + 1;
          state := HALT;
        when HALT =>
          if local_sample_en = '0' then
            state := IDLE;
          end if;
        when others => null;
      end case;      
    end if;
  end process;

  start_filt <= '1' when countB(6 downto 0) = 121  -- selected so as to
                                                   -- avoid the sampling of
                                                   -- the PSGs
                  else '0';

  -- At 96000 Hz, filter the oldest 17 samples for each channel
  -- Scaled PSG audio is available in 2.22 format, here truncated to 2.14 bits
  -- and MACed against 2.14 coefficients, each multiplication yields a 4.28
  -- result.
  filt : process( clk, reset )
    -- 16 bits in 2.14 format
    type TCOEFF is array(0 to 16) of signed(15 downto 0);
    constant FILT24K : TCOEFF := (X"ff2e", -- -0.012820
                                  X"00cb", -- 0.012419
                                  X"004d", -- 0.004739
                                  X"fe9f", -- -0.021598
                                  X"0363", -- 0.052918
                                  X"fb42", -- -0.074142
                                  X"f69d", -- -0.146680
                                  X"253d", -- 0.581888
                                  X"4ca8", -- 1.197806
                                  X"253d", -- 0.581888
                                  X"f69d", -- -0.146680
                                  X"fb42", -- -0.074142
                                  X"0363", -- 0.052918
                                  X"fe9f", -- -0.021598
                                  X"004d", -- 0.004739
                                  X"00cb", -- 0.012419
                                  X"ff2e"  -- -0.012820
                                  );
    constant FILT723 : TCOEFF := (X"463b", -- 1.097387
                                  X"46d0", -- 1.106476
                                  X"4752", -- 1.114392
                                  X"47c0", -- 1.121121
                                  X"481a", -- 1.126646
                                  X"4861", -- 1.130956
                                  X"4894", -- 1.134042
                                  X"48b2", -- 1.135895
                                  X"48bc", -- 1.136514
                                  X"48b2", -- 1.135895
                                  X"4894", -- 1.134042
                                  X"4861", -- 1.130956
                                  X"481a", -- 1.126646
                                  X"47c0", -- 1.121121
                                  X"4752", -- 1.114392
                                  X"46d0", -- 1.106476
                                  X"463b"  -- 1.097387
                                  );
    constant FILT3K4 : TCOEFF := (X"1da9", -- 0.463487
                                  X"25bf", -- 0.589803
                                  X"2dae", -- 0.713773
                                  X"3520", -- 0.830105
                                  X"3bc0", -- 0.933647
                                  X"4142", -- 1.019685
                                  X"4563", -- 1.084222
                                  X"47f3", -- 1.124215
                                  X"48d1", -- 1.137762
                                  X"47f3", -- 1.124215
                                  X"4563", -- 1.084222
                                  X"4142", -- 1.019685
                                  X"3bc0", -- 0.933647
                                  X"3520", -- 0.830105
                                  X"2dae", -- 0.713773
                                  X"25bf", -- 0.589803
                                  X"1da9"  -- 0.463487
                                  );
    constant FILT596 : TCOEFF := (X"4702", -- 1.109511
                                  X"4767", -- 1.115719
                                  X"47c0", -- 1.121119
                                  X"480b", -- 1.125701
                                  X"4849", -- 1.129460
                                  X"4879", -- 1.132389
                                  X"489b", -- 1.134485
                                  X"48b0", -- 1.135743
                                  X"48b6", -- 1.136163
                                  X"48b0", -- 1.135743
                                  X"489b", -- 1.134485
                                  X"4879", -- 1.132389
                                  X"4849", -- 1.129460
                                  X"480b", -- 1.125701
                                  X"47c0", -- 1.121119
                                  X"4767", -- 1.115719
                                  X"4702"  -- 1.109511
                                  );
    type TFILTS is array(0 to 3) of TCOEFF;
    variable FILTERS : TFILTS;
    variable bindex, findex : unsigned(4 downto 0);
    variable idle : std_logic;
  begin
    if reset = '1' then
      bindex := "00000";
      findex := "00000";
      idle   := '1';
      FILTERS(0) := FILT24K;
      FILTERS(1) := FILT723;
      FILTERS(2) := FILT3K4;
      FILTERS(3) := FILT596;
    elsif clk'event and clk = '1' then
      -- determine when to kick off
      if (idle = '1') then
        if (start_filt = '1') then
          idle   := '0';
          findex := "00000";
          bindex := head - 1;
          filt1a  <= (others => '0');
          filt1b  <= (others => '0');
          filt1c  <= (others => '0');
          filt2a  <= (others => '0');
          filt2b  <= (others => '0');
          filt2c  <= (others => '0');
        end if;
      else
        -- FIR MACs
        filt1a <= filt1a + (psg1a(to_integer(bindex))(23 downto 8) *
                            filters(fix1a)(to_integer(findex)));
        filt1b <= filt1b + (psg1b(to_integer(bindex))(23 downto 8) *
                            filters(fix1b)(to_integer(findex)));
        filt1c <= filt1c + (psg1c(to_integer(bindex))(23 downto 8) *
                            filters(fix1c)(to_integer(findex)));
        filt2a <= filt2a + (psg2a(to_integer(bindex))(23 downto 8) *
                            filters(fix2a)(to_integer(findex)));
        filt2b <= filt2b + (psg2b(to_integer(bindex))(23 downto 8) *
                            filters(fix2b)(to_integer(findex)));    
        filt2c <= filt2c + (psg2c(to_integer(bindex))(23 downto 8) *
                            filters(fix2c)(to_integer(findex)));

        -- index update and termination
        if (findex = "10000") then
          idle  := '1';
        else
          findex := findex + 1;
          bindex := bindex - 1;
        end if;
      end if;
    end if;
  end process;

  -- the mixer combines all six filtered channels to produce a 1.17 result that
  -- is scaled at 6/8ths of full-scale
  mix : process( clk, reset )
    variable count : unsigned(0 downto 0);
    variable idle : std_logic;
    variable result : signed(17 downto 0);
  begin
    if reset = '1' then
      audio <= "00" & X"0000";
      output_en <= '0';
      count := "0";
      idle := '1';
      result := (others => '0');
    elsif clk'event and clk = '1' then
      if (idle = '1') then
        output_en <= '0';
        if (start_filt = '1') then
          if (count = "1") then
            idle := '0';
          end if;
          result := filt1a(31 downto 14) +
                    filt1b(31 downto 14) +
                    filt1c(31 downto 14) +
                    filt2a(31 downto 14) +
                    filt2b(31 downto 14) +
                    filt2c(31 downto 14);
          count := count + 1;
        end if;
      else
        if (start_filt = '0') then
          idle := '1';
          output_en <= '1';
          audio <= std_logic_vector( result );
        end if;
      end if;
    end if;
  end process;

  audio_en <= output_en;
  
end architecture behaviour;
