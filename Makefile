.PHONY : build prep clean veryclean install css

install: prep
	cabal install

prep:
	cabal sandbox init
	cabal install --only-dependencies

clean:
	cabal clean

veryclean: clean
	rm -rf cabal-dev

