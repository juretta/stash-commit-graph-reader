.phony: all clean


all: 
	ghc -Wall -Werror --make -O2 index-file-reader.hs

clean:
	rm index-file-reader.{hi,o}
	rm index-file-reader
