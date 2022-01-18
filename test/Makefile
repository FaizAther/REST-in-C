CC	:=gcc
CCFLAGS	:=-std=gnu99 -g -Wall -pedantic -LTO -Werror
LDFLAGS :=

TARGETS	:= sumex
MAINS  := $(addsuffix .o, $(TARGETS) )
OBJS	:= stack.o list.o node.o $(MAINS)
DEPS	:= stack.h list.h node.h

.PHONY: all clean

all: $(TARGETS)

clean:
	rm -f $(TARGETS) $(OBJS)

$(OBJS): %.o : %.c $(DEPS)
	@echo $< $@
	$(CC) -c -o $@ $< $(CCFLAGS)

$(TARGETS): % : $(filter-out $(MAINS), $(OBJS)) %.o
	$(CC) -o $@ $(LIBS) $^ $(CCFLAGS) $(LDFLAGS)
