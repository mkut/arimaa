################################
## Makefile Template for Haskell
## Author: mkut
## Date  : 2012-09-25
## Rev   : 0.1.2
################################

## project settings
TARGET = main
SOURCES = Main.hs
MILIBS = 

## command settings
HC = ghc
HCFLAGS = --make -O

## directory settings
MILIB_DIR = /home/mkut/milib/haskell

## file name macros
OBJS = $(SOURCES:%.hs=%.hi) $(SOURCES:%.hs=%.o)
MILIB_SRC = $(MILIBS:%=$(MILIB_DIR)/%.hs)

## make rules
.PHONY: default clean

default: $(TARGET)

$(TARGET): $(SOURCES) $(MILIB_SRC)
	$(HC) $(HCFLAGS) -o $@ $^

clean:
	rm -f *.o $(TARGET) $(OBJS) $(GEN_OBJS)
