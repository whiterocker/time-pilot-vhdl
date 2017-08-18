# Convert a binary image to a VHDL data vector.
# Copyright (C) 2011 Christopher D. Kilgour
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

ENDIAN = '>'

import math
import os.path
import struct
import sys

if len( sys.argv ) < 2:
    sys.exit( 1 )

infname  = sys.argv[1]
name     = os.path.basename( infname ).split( '.' )[-1]
outfname = name + '.vhdl'
try:
    f = file( infname, 'r' )
except:
    sys.exit( 2 )

data = f.read( )
f.close( )

width = 1
if len( sys.argv ) > 2:
    width = int( sys.argv[2] )

bytes = struct.unpack( 'B'*len(data), data )
hwords = struct.unpack( ENDIAN + 'H'*(len(data)/2), data )
dwords = struct.unpack( ENDIAN + 'L'*(len(data)/4), data )

if width == 1:
    frags = [ 'X"%02x"' % b for b in bytes ]
elif width == 2:
    frags = [ 'X"%04x"' % h for h in hwords ]
elif width == 4:
    frags = [ 'X"%08x"' % d for d in dwords ]
else:
    raise Exception("Unsupported width: %d"  % width)

dbuf  = ',\n'.join( ['    '+l for l in frags] )

f = file( outfname, 'w+' )

f.write( '''-- generated by %s, do not edit
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity %s is
  port (
    addr : in std_logic_vector(%d downto 0);
    dout : out std_logic_vector(%d downto 0));
end entity %s;

architecture ROM of %s is

  type tROM is array(0 to %d) of std_logic_vector(%d downto 0);
  constant thisROM : tROM := (
%s
  );

begin
  dout <= thisROM( to_integer(unsigned(addr)) );
end architecture ROM;
''' % (sys.argv[0],
       name,
       int( math.log( (len( data )/width), 2 ) )-1,
       (8*width)-1,
       name,
       name,
       (len( data )/width)-1, (8*width)-1,
       dbuf) )


f.close( )
