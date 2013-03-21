.PHONY : all clean veryclean install css

all:
	cabal-dev configure && cabal-dev build

install:
	cabal-dev install

prep:
	cabal-dev install-deps

clean:
	cabal-dev clean

veryclean: clean
	rm -rf cabal-dev

