CFLAGS = -Wall -g

OBJS = main.o symbol.o semantics.o source.o parse.o stmts.o code.o error.o

pascal: $(OBJS)
	cc -g -o pascal $(OBJS)

$(OBJS): machine.h pascal.h

clean:
	rm -f *~ *.o *.stackdump
