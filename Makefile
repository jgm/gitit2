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

css: static/css/screen.css static/css/bootstrap.min.css

static/css/bootstrap.min.css: bootstrap/less/bootstrap.less
	lessc --compress $@ > $@

static/css/screen.css: data/screen.less
	lessc --compress --include-path=bootstrap/less $< > $@
