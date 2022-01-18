CC=gcc
CFLAGS=-pedantic -Wall -Wextra -Werror
DEBUG=-g -ggdb

BIN=epoll-demo

epoll-demo: epoll-demo.c
	${CC} ${DEBUG} epoll-demo.c -o epoll-demo

.PHONY: clean run debug

run: epoll-demo
	./epoll-demo

debug: epoll-demo
	gdb ./epoll-demo -q

clean:
	rm -rf ${BIN}

