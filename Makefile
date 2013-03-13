.PHONY : all

all :
	cabal-dev configure ; cabal-dev build

prep :
	cabal-dev install-deps
