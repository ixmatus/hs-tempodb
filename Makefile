.PHONY: test

all: config build install

config:
	cabal configure

build:
	cabal build

install:
	cabal install

test:
	$(MAKE) -C test

changelog:
	gitlog-to-changelog --format='%s%n%n%b%n' --no-cluster --strip-tab --strip-cherry-pick > CHANGELOG.tmp && cat CHANGELOG.tmp >> CHANGELOG && rm -f CHANGELOG.tmp
