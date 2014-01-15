.phony: all clean


all: 
	ghc -Wall -Werror --make -O2 index-file-reader.hs
	strip index-file-reader
	upx index-file-reader

clean:
	rm index-file-reader.{hi,o}
	rm index-file-reader
