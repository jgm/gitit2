.PHONY : all

all :
	cabal-dev configure --cabal-install-arg='-fblaze_html_0_5'; cabal-dev build

prep :
	cabal-dev install-deps --cabal-install-arg='-fblaze_html_0_5'
