all: FORCE
	stack setup
	stack build
	stack test

build: FORCE
	stack build

test: FORCE
	stack test

conffmt:
	stack build --flag language-hcl:conffmt

FORCE:
