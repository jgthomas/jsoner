
PROJECT=jsoner

LINTER=hlint
FORMATTER=ormolu
DEAD_CODE=weeder
IDE=ghcid


.PHONY: test lint clean coverage weed ide setup


all: test lint weed

build:
	stack build --pedantic

test:
	stack build --pedantic --test

lint:
	stack exec -- ${LINTER} src/ app/

coverage: clean
	stack test --coverage && stack hpc report ${PROJECT} --open

clean:
	stack clean

docs:
	stack haddock --no-haddock-deps ${PROJECT}

weed:
	stack exec ${DEAD_CODE}

ide:
	stack exec ${IDE}

setup:
	stack build --copy-compiler-tool ${LINTER} ${FORMATTER} ${DEAD_CODE} ${IDE}
