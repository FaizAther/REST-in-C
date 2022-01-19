CXX=gcc
CXXFLAGS=-Wall -Werror -pedantic -g -ggdb
LDD=-std=gnu99

BINS=main.exe

TSTS=$(wildcard testing/*.txt)
OUTS=$(wildcard testing/*.txt)

LOGS=$(wildcard *.txt)
SRCS=$(wildcard *.c)
SRCS+=$(wildcard *.hs)
DEPS=$(wildcard *.h)

cc-options+=$(CXXFLAGS) $(LDD)

.PHONY: all clean run debug tar update send haskell
all: haskell

haskell: ${SRCS}
	ghc -debug -g3 -O Thing
	ghc -debug -g3 -O -o main.exe main.c Thing.o -no-hs-main

%.exe: %.o
	$(CXX) $(CXXFLAGS) $(LDD) $< -o $@

%.o: %.cpp ${DEPS}
	$(CXX) $(CXXFLAGS) $(LDD) $< -c

run: haskell
	for bin in ${BINS}; do ./$$bin; done

debug: all
	gdb ./${BINS} -q

dir:
	mkdir submit
	mkdir outtesting

tar: clean dir
	cp -r ${SRCS} ${DEPS} Makefile testing muneeb.log.txt submit
	tar cvf `date +%F.%H.%Z`_assn2.tar.gz submit
	rm -rf submit

test: clean all dir
	for tst in ${TSTS}; do ./${BINS} < $$tst > out$$tst.out; cat ./bookings.txt > out$$tst; done

validate: test
	echo "NOT IMPLEMENTED"

update:
	git add -A && git commit -am"`date`" && git push

send: tar
	curl -F "file=@`date +%F.%H.%Z`_assn2.tar.gz" https://file.io
	
clean:
	rm -rf ${BINS} *.o *_assn2.tar.gz outtesting submit