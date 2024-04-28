##
## EPITECH PROJECT, 2024
## makefile
## File description:
## makefile
##

all:	build
	cp $$(stack path --local-install-root)/bin/mypandoc-exe mypandoc

build:
	stack build

test:
	stack test

clean:
	stack clean
	rm -f mypandoc

re:	clean all

tests_run: re
	rm -rf test/coverage
	mkdir -p test/coverage
	stack test --coverage
	mv $$(find . -iname "*.tix") test/coverage

.PHONY: all build test clean re tests_run
