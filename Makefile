.PHONY : all

all :
	cabal-dev configure && cabal-dev build
