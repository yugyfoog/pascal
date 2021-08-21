CFLAGS = -Wall -Wextra -g

OBJS = pascal.o symbols.o source.o constants.o types.o parse.o statements.o \
       expressions.o semantics.o code.o gen.o error.o

pascal: $(OBJS)
	cc -g -o pascal $(OBJS)

$(OBJS): pascal.h machine.h code.h gen.h

install:
	cp pascal /usr/local/bin/

clean:
	rm -f *.o *~
