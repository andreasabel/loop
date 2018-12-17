# Makefile for LOOP programming language

## Variables
###########################################################################

grm         = LoopLang
dir         = $(grm)
bnfc_output = $(patsubst %,$(dir)/%,Abs.hs ErrM.hs Print.hs Lex.x Par.y Test.hs)
alex_file   = $(dir)/Lex
happy_file  = $(dir)/Par
main        = ./loop

files       = loop.hs $(grm).cf Makefile TypeChecker.hs Interpreter.hs

bnfc        = bnfc
###########################################################################

.PHONY: build test clean distclean

default : test

build : $(alex_file).hs $(happy_file).hs $(main)

loop : loop.hs Interpreter.hs $(alex_file).hs $(happy_file).hs
	ghc --make loop.hs -o loop

test : $(main)
	$(main) test/zero.loop
	$(main) test/pred.loop
	$(main) test/div.loop
	$(main) test/twice.loop


$(bnfc_output): $(grm).cf
	$(bnfc) -d $<

%.hs: %.x
	alex --ghc $<

%.hs: %.y
	happy --ghc --coerce --array --info $<

# Rules for cleaning generated files:

clean :
	-rm -f $(grm)/*.log $(grm)/*.aux $(grm)/*.hi $(grm)/*.o $(grm)/*.dvi *.hi *.o

distclean : clean
	-rm -f $(grm)/Doc.* $(grm)/Lex.* $(grm)/Par.* $(grm)/Layout.* $(grm)/Skel.* $(grm)/Print.* $(grm)/Test.* $(grm)/Abs.* $(grm)/Test $(grm)/ErrM.* $(grm)/SharedString.* $(grm)/ComposOp.* $(grm)/$(grm).dtd $(grm)/XML.* $(grm)/*.bak
	-rmdir -p $(grm)/
	-rm loop

# EOF
