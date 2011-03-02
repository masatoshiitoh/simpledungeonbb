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


# Makefile for nmake.exe

program = simpledungeon
objs = admin.beam character.beam character_stream.beam db.beam mmoasp.beam morningcall.beam mout.beam move.beam npc.beam path_finder.beam test.beam trade.beam u.beam uauth.beam world.beam yaws_if.beam

DESTDIR = ..\ebin
INSTALL = MOVE
RM = DEL
COPY = COPY

ERLC = erlc
## ERLCFLAGS = -o $(DESTDIR)
ERLCFLAGS = -DTEST -pa "C:/Progra~2/erl5.7.4/lib/eunit-2.1.4/ebin" -pa "C:/Opt/Yaws/ebin"

.SUFFIXES: .erl .beam

all : $(objs)

install : $(objs)
	$(COPY) *.beam $(DESTDIR)

.erl.beam:
	$(ERLC) $(ERLCFLAGS) $<

.PHONY: clean
clean:
	$(RM) $(objs)

test: $(objs)
	erl -pa .  -noshell -boot start_clean -s test run_tests_with_log -s init stop
# erl -boot start_clean -noshell -eval 'eunit:test([test], [{report,{eunit_surefire,[{dir,"."}]}}])' -s init stop
