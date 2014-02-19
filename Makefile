.phony: all clean

NAME="stash-commit-graph-reader"

all: 
	ghc -Wall -Werror --make -O2 ${NAME}.hs
	strip ${NAME}
	upx ${NAME}

clean:
	rm -f ${NAME}.{hi,o}
	rm -f ${NAME}
