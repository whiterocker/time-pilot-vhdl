#
# Makefile for Time Pilot in VHDL
# (C) 2017 Christopher D. Kilgour techie@whiterocker.com
#
# 
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
# 

#
# Supported targets:
#  ghdl-sdl builds a standalone binary using SDL for AV.
#
default : ghdl-sdl

# --------------------------------------------------------------------------
#                               Konami ROMs
# --------------------------------------------------------------------------

#
# ROMs are not included in the source distribution, please obtain
# copyrighted material legally.
#

ROMFILES = tm1 tm2 tm3 tm4 tm5 tm6 timeplt.e9 timeplt.e12 tm7
ROMS     = $(addprefix roms/,$(ROMFILES))

roms/romset : $(ROMS)
	cat $^ > $@

$(ROMS) : roms/.zipokay
	(cd roms; unzip timeplt.zip $(notdir $@))

roms/.zipokay : roms/timeplt.zip roms/FILES
	sha1sum -c roms/FILES
	touch -r $< $@

roms/FILES :
	echo "f21b49b403a86d63ad04d0cc72145034e7ff6ec2 roms/timeplt.zip" > $@
	touch -r roms/timeplt.zip $@

.SECONDARY :
roms/%.vhdl : roms/% 
	python util/rom2vhdl.py $<
	mv $*.vhdl $@

roms/timeplt.zip:
	@echo "$@ is missing but required to build"
	@/bin/false

roms/% : roms/timeplt.%
	cp $< $@

clean ::
	rm -f roms/romset $(ROMS) roms/.zipokay roms/FILES roms/*.vhdl

# --------------------------------------------------------------------------
#                            GHDL-SDL Build
# --------------------------------------------------------------------------

SDL_GCC_ARGS  = $(shell pkg-config --cflags sdl)
SDL_LINK_ARGS = $(shell pkg-config --libs sdl | sed 's/-l/-Wl,-l/g')

GHDL_OPTS = --ieee=synopsys -fexplicit --workdir=work

OBJS  = tm1.o tm2.o tm3.o tm4.o tm5.o tm6.o e9.o e12.o tm7.o \
	ay_3_8910_psg.o dpram1k.o dpram2k.o pixel_ram.o ram1k.o ram2k.o rasterizer.o resampler.o \
	t80_mcode.o t80_pack.o t80_alu.o t80_reg.o t80.o t80se.o \
	sdl_adapt.o 
WOBJS = $(addprefix work/,$(OBJS))
TPOBJ = work/time_pilot.o
COBJS = work/sdl_ghdl.o
TOBJ  = work/top_ghdl_sdl.o

ghdl-sdl : $(WOBJS) $(TOBJ) $(COBJS)
	ghdl -e $(GHDL_OPTS) \
		$(addprefix -Wl,,$(COBJS)) \
		$(SDL_LINK_ARGS) \
		-o $@ top_ghdl_sdl

work/%.o : src/%.vhd
	@mkdir -p work
	ghdl -a $(GHDL_OPTS) $<

work/%.o : src/t80/%.vhd
	@mkdir -p work
	ghdl -a $(GHDL_OPTS) $<

work/%.o : roms/%.vhdl
	@mkdir -p work
	ghdl -a $(GHDL_OPTS) $<

work/rasterizer.o : work/pixel_ram.o
work/t80_mcode.o  : work/t80_pack.o
$(TOBJ)  : $(TPOBJ)
$(TPOBJ) : $(WOBJS)

work/sdl_ghdl.o : src/sdl_ghdl.c
	@mkdir -p work
	gcc $(SDL_GCC_ARGS) -c $< -o $@

clean :: 
	rm -rf work/
	rm -f ghdl-sdl
