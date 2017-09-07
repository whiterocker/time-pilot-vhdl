## Generated SDC file "TimePilotDE10.sdc"

## Copyright (C) 2017  Intel Corporation. All rights reserved.
## Your use of Intel Corporation's design tools, logic functions 
## and other software and tools, and its AMPP partner logic 
## functions, and any output files from any of the foregoing 
## (including device programming or simulation files), and any 
## associated documentation or information are expressly subject 
## to the terms and conditions of the Intel Program License 
## Subscription Agreement, the Intel Quartus Prime License Agreement,
## the Intel MegaCore Function License Agreement, or other 
## applicable license agreement, including, without limitation, 
## that your use is for the sole purpose of programming logic 
## devices manufactured by Intel and sold by Intel or its 
## authorized distributors.  Please refer to the applicable 
## agreement for further details.


## VENDOR  "Altera"
## PROGRAM "Quartus Prime"
## VERSION "Version 17.0.0 Build 595 04/25/2017 SJ Lite Edition"

## DATE    "Fri Sep  1 10:17:34 2017"

##
## DEVICE  "10M50DAF484C7G"
##


#**************************************************************
# Time Information
#**************************************************************

set_time_format -unit ns -decimal_places 3



#**************************************************************
# Create Clock
#**************************************************************

create_clock -name {max10_clk1_50} -period 20.000 -waveform { 0.000 10.000 } [get_ports {max10_clk1_50}]
create_clock -name {max10_clk2_50} -period 20.000 -waveform { 0.000 10.000 } [get_ports {max10_clk2_50}]
create_clock -name {adc_clk_10} -period 100.000 -waveform { 0.000 50.000 } [get_ports {adc_clk_10}]


#**************************************************************
# Create Generated Clock
#**************************************************************

create_generated_clock -name {MAIN_CLK|altpll_component|auto_generated|pll1|clk[0]} -source [get_pins {MAIN_CLK|altpll_component|auto_generated|pll1|inclk[0]}] -duty_cycle 50/1 -multiply_by 59 -divide_by 40 -master_clock {max10_clk1_50} [get_pins {MAIN_CLK|altpll_component|auto_generated|pll1|clk[0]}] 
create_generated_clock -name {MAIN_CLK|altpll_component|auto_generated|pll1|clk[1]} -source [get_pins {MAIN_CLK|altpll_component|auto_generated|pll1|inclk[0]}] -duty_cycle 50/1 -multiply_by 59 -divide_by 120 -master_clock {max10_clk1_50} [get_pins {MAIN_CLK|altpll_component|auto_generated|pll1|clk[1]}] 
create_generated_clock -name {MAIN_CLK|altpll_component|auto_generated|pll1|clk[2]} -source [get_pins {MAIN_CLK|altpll_component|auto_generated|pll1|inclk[0]}] -duty_cycle 50/1 -multiply_by 59 -divide_by 240 -master_clock {max10_clk1_50} [get_pins {MAIN_CLK|altpll_component|auto_generated|pll1|clk[2]}] 


#**************************************************************
# Set Clock Latency
#**************************************************************



#**************************************************************
# Set Clock Uncertainty
#**************************************************************



#**************************************************************
# Set Input Delay
#**************************************************************



#**************************************************************
# Set Output Delay
#**************************************************************



#**************************************************************
# Set Clock Groups
#**************************************************************



#**************************************************************
# Set False Path
#**************************************************************



#**************************************************************
# Set Multicycle Path
#**************************************************************



#**************************************************************
# Set Maximum Delay
#**************************************************************



#**************************************************************
# Set Minimum Delay
#**************************************************************



#**************************************************************
# Set Input Transition
#**************************************************************

