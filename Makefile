# Makefile k projektu do předmětu FLP
# Daniel Pátek (xpatek08)
# 2023-03-21

.PHONY: flp22-fun

all: flp22-fun

flp22-fun:
	ghc -Wall -o flp22-fun src/Main.hs src/Parser.hs src/Types.hs src/Solve.hs src/SolveGenetic.hs
	rm -rf src/*.hi src/*.o

clean:
	rm -rf flp22-fun src/*.o src/*.hi flp-fun-xpatek08.zip

zip:
	zip flp-fun-xpatek08.zip Makefile src/* doc/* test/*