-- Adapt to WM8731 codec on e.g. MikroElektronika dev board
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

-- top-level entity for WM8731 adapt
entity wm8731_adapt is
  port (
    -- async reset
    areset    : in std_logic;
    
    -- audio samples
    audio     : in std_logic_vector(17 downto 0);
    aclk48k   : in std_logic;

    -- codec interface
    aud_sck   : in std_logic;
    aud_mosi  : out std_logic;
    aud_dacl  : out std_logic;
    aud_sda   : out std_logic;
    aud_scl   : out std_logic;

    cfg_done  : out std_logic
  );
end entity wm8731_adapt;

-- ======================================================================

-- We operate the WM8731 with the following register configuration:
-- 00: 010000000: LRINBOTH=0b, LINMUTE=1b, LINVOL=00000b
-- 01: 010000000: RLINBOTH=0b, RINMUTE=1b, RINVOL=00000b
-- 02: 000000000: LRHPBOTH=0b, LZCEN=0b, LHPVOL=0000000b
-- 03: 000000000: RLHPBOTH=0b, RZCEN=0b, RHPVOL=0000000b
-- 04: 000010010: SIDEATT=00b, SIDETONE=0b, DACSEL=1b, BYPASS=0b, INSEL=0b, MUTEMIC=1b, MICBOOST=0b
-- 05: 000000000: HPOR=0b, DACMU=0b, DEEMP=00b, ADCHPD=0b
-- 06: 001100111: POWEROFF=0b, CLKOUTPD=1b, OSCPD=1b, OUTPD=0b, DACPD=0b, ADCPD=1b, MICPD=1b, LINEINPD=1b
-- 07: 000000010: BCLKINV=0b, MS=0b, LRSWAP=0b, LRP=0b, IWL=00b, FORMAT=10b
-- 08: 000000000: CLKODIV2=0b, CLKIDIV2=0b, SR=0000b, BOSR=0b, USB/NORMAL=0b
-- 09: 000000001: ACTIVE=1b

architecture behaviour of wm8731_adapt is

  signal sync_48k   : std_logic;
  signal psync_48k  : std_logic;
  signal dacl       : std_logic;
  signal sda        : std_logic;
  signal scl        : std_logic;

  -- I2C configuration state machine
  subtype scl_type is unsigned(1 downto 0);
  constant SCL0     : scl_type := "01";
  constant SCL1     : scl_type := "11";
  constant SCL2     : scl_type := "10";
  constant SCL3     : scl_type := "00";
  signal phase      : scl_type := SCL0;
  
  subtype state_type is unsigned(4 downto 0);
  constant IDLE     : state_type := "10100";
  constant START    : state_type := "11100";
  constant BIT7     : state_type := "01100";
  constant BIT6     : state_type := "01101";
  constant BIT5     : state_type := "01111";
  constant BIT4     : state_type := "01110";
  constant BIT3     : state_type := "01010";
  constant BIT2     : state_type := "01011";
  constant BIT1     : state_type := "01001";
  constant BIT0     : state_type := "01000";
  constant ACK      : state_type := "11000";
  constant STOP     : state_type := "10000";
  constant DONE     : state_type := "10001";
  signal state      : state_type := IDLE;
  signal bytes      : unsigned(1 downto 0) := "00";  -- 3 bytes transferred per word
  signal words      : unsigned(3 downto 0) := X"0";  -- 10 words to program
  type TCONFIG is array(0 to 9) of std_logic_vector(23 downto 0);
  constant CONFIG : TCONFIG := (X"350080", X"350280", X"350400", X"350600",
                                X"350812", X"350a00", X"350c67", X"350e02",
                                X"351000", X"351201");
  signal cfgword    : std_logic_vector(23 downto 0);

  -- stream buffer
  signal sample     : std_logic_vector(15 downto 0);
  signal scount     : unsigned(7 downto 0);
  
begin  -- behaviour

  -- 48 kHz sample clock to aligned to audio source
  p_clk48 : process(aud_sck, areset) is
  begin
    if areset = '1' then
      sync_48k  <= '0';
      psync_48k <= '0';
    elsif aud_sck'event and aud_sck = '1' then
      psync_48k <= sync_48k;
      sync_48k  <= aclk48k;
    end if;
  end process;

  -- configuration at startup
  -- bare-bones I2C running at 12 kHz
  -- writes CONFIG blindly, without checking for byte ACKs
  p_config : process(aclk48k, areset) is
  begin
    if areset = '1' then
      phase <= SCL0;
      state <= IDLE;
      bytes <= (others => '0');
      words <= (others => '0');
      sda   <= '1';
      scl   <= '1';
    elsif aclk48k'event and aclk48k = '1' then
      case phase is
        when SCL0 =>
          if ((state = IDLE) or (state = STOP) or (state = DONE)) then
            scl <= '1';
          else
            scl <= '0';
          end if;
          phase <= SCL1;
          
        when SCL1 =>                 
        case state is
          when IDLE =>
            sda     <= '0';
            state   <= START;
            cfgword <= CONFIG(to_integer(words));
          when START =>
            sda     <= cfgword(23);
            cfgword <= cfgword(22 downto 0) & '0';
            state   <= BIT7;
          when BIT7 =>
            sda     <= cfgword(23);
            cfgword <= cfgword(22 downto 0) & '0';
            state   <= BIT6;            
          when BIT6 =>
            sda     <= cfgword(23);
            cfgword <= cfgword(22 downto 0) & '0';
            state   <= BIT5;
          when BIT5 =>
            sda     <= cfgword(23);
            cfgword <= cfgword(22 downto 0) & '0';            
            state   <= BIT4;
          when BIT4 =>
            sda     <= cfgword(23);
            cfgword <= cfgword(22 downto 0) & '0';            
            state   <= BIT3;
          when BIT3 =>
            sda     <= cfgword(23);
            cfgword <= cfgword(22 downto 0) & '0';            
            state   <= BIT2;
          when BIT2 =>
            sda     <= cfgword(23);
            cfgword <= cfgword(22 downto 0) & '0';            
            state   <= BIT1;
          when BIT1 =>
            sda     <= cfgword(23);
            cfgword <= cfgword(22 downto 0) & '0';            
            state   <= BIT0;
          when BIT0 =>
            sda   <= '1';
            state <= ACK;
          when ACK =>
            sda   <= '0';
            state <= STOP;
          when STOP =>
            sda <= '1';
            if ((bytes >= 2) and (words >= 9)) then
              state <= DONE;
            else
              if (bytes < 2) then
                bytes <= bytes + 1;
              else
                words <= words + 1;
                bytes <= (others => '0');
              end if;
              state <= IDLE;
            end if;
          when others => null;
        end case;
        phase <= SCL2;
        
        when SCL2 =>
          scl   <= '1';
          phase <= SCL3;
          
        when SCL3 =>
          phase <= SCL0;
      end case;
    end if;
  end process;

  -- formatted audio stream (I2S right-justified)
  -- relies on fact that 16-bit sample rotates evenly into 128 bits per channel
  p_stream : process(aud_sck, areset) is
  begin
    if areset = '1' then
      dacl      <= '0';
      sample    <= (others => '0');
      scount    <= X"00";
    elsif aud_sck'event and aud_sck = '0' then
      if (sync_48k = '1') and (psync_48k = '0') then
        -- start: left channel
        sample <= audio(17 downto 2);
        dacl   <= '1';
        scount <= X"00";
      else
        if (scount = 127) then
          dacl <= '0';
        end if;
        sample <= sample(14 downto 0) & sample(15);
        scount <= scount + 1;
      end if;      
    end if;
  end process;

  -- wire in outputs
  aud_mosi <= sample(15);
  aud_dacl <= dacl;
  aud_sda  <= sda;
  aud_scl  <= scl;
  cfg_done <= '1' when (state = DONE) else '0';
    
end behaviour;
