CFLAGS = -Wall  -g

OBJS = pascal.o symbols.o source.o constants.o types.o parse.o statements.o \
       expressions.o semantics.o code.o gen.o error.o

pascal: $(OBJS)
	cc -g -o pascal $(OBJS)

$(OBJS): pascal.h machine.h code.h gen.h


wholecopy.s: wholecopy.p
	pascal wholecopy.p

wholecopy.o: wholecopy.s
	as -g -o wholecopy.o wholecopy.s

wholecopy: wholecopy.o plib.a
	ld -g -o wholecopy wholecopy.o plib.a

install:
	cp pascal /usr/local/bin/

clean:
	rm -f *.o *~
