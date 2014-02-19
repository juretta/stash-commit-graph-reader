.phony: all clean dist

NAME="stash-commit-graph-reader"
VERSION="0.2"
BINARY="${NAME}-${VERSION}.tar.gz"

all: 
	ghc -Wall -Werror --make -O2 ${NAME}.hs
	strip ${NAME}
	upx ${NAME}

dist: all
	mkdir -p dist/${NAME}/bin
	cp ${NAME} dist/${NAME}/bin
	tar -C dist -czf ${BINARY} ${NAME}
	sha1sum ${BINARY}

clean:
	rm -f ${NAME}.{hi,o}
	rm -f ${NAME}
	rm -rf dist
	rm -f ${NAME}*.tar.gz
