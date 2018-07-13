CFLAGS = -Wall -g

OBJS = main.o source.o parse.o code.o error.o

pascal: $(OBJS)
	cc -g -o pascal $(OBJS)

$(OBJS): pascal.h

clean:
	rm -f *~ *.o *.stackdump
