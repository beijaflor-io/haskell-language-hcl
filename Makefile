all: FORCE
	stack setup
	stack build
	stack test

build: FORCE
	stack build

test: FORCE
	stack test

FORCE:
