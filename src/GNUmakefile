CC = gcc -o
CCO = gcc -c -o
RC = #
RM = rm -f

DESTDIR = ../ebin
INSTALL = mv
COPY = cp -p

ERLC = erlc
## ERLCFLAGS = -o $(DESTDIR)
ERLCFLAGS = -DTEST -pa "/opt/yaws/ebin" -I "/opt/yaws/lib/yaws/include"


include CommonMakefile.in

