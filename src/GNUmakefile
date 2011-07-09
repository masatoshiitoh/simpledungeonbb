#
#      Copyright (C) 2010 by Masatoshi Itoh
#      http://www.simpledungeon.com/
#
#  This Program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2, or (at your option)
#  any later version.
#
#  This Program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with simple dungeon; see the file COPYING.  If not, write to
#  the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
#  http://www.gnu.org/copyleft/gpl.html
#

CC = gcc -o
CCO = gcc -c -o
RC = #
RM = rm -f
REMOVEDIR = rm -rf

DESTDIR = ../ebin
INSTALL = mv
COPY = cp -p
YAWS_EBINDIR = /opt/yaws/lib/yaws/ebin

ERLC = erlc
## ERLCFLAGS = -o $(DESTDIR)
ERLCFLAGS = -DTEST -pa "/opt/yaws/ebin" -I "/opt/yaws/lib/yaws/include"


include CommonMakefile.in

