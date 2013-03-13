.PHONY : all clean veryclean

all:
	cabal-dev configure && cabal-dev build

prep:
	cabal-dev install-deps

clean:
	cabal-dev clean

veryclean: clean
	rm -rf cabal-dev

