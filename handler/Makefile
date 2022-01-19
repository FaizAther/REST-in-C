CC=cc
CFLAGS=-pedantic -Wall -Werror -Wextra -std=gnu99
LDFLAGS=
# LEVENT=-levent
DEBUG=-g -ggdb

BINS=check.exe
SRCS=$(wildcard *.c)
OBJS=$(patsubst %.c,%.o,${SRCS})

.PHONY: all clean run debug

all: ${OBJS} ${BINS}
	@echo $<

%.exe: ${OBJS}
	@echo $<
	gcc ${CFLAGS} ${DEBUG} ${LDFLAGS} ${LEVENT} -o $@ ${OBJS}

%.o: %.c
	@echo $@ $<
	gcc ${CFLAGS} ${LDFLAGS} ${DEBUG} -c $<

run: all
	./${BINS}

debug: all
	gdb ./${BINS} -q

clean:
	rm -rf *.o *.exe
