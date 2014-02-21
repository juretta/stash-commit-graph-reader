.phony: all clean dist

NAME="stash-commit-graph-reader"
VERSION="0.3"
BIN="dist/build/stash-commit-graph-reader/stash-commit-graph-reader"
BINARY="${NAME}-${VERSION}.tar.gz"

all: 
	cabal clean
	cabal build
	strip ${BIN}
	upx ${BIN}

dist: all
	mkdir -p build/${NAME}/bin
	cp ${BIN} build/${NAME}/bin
	tar -C build -czf ${BINARY} ${NAME}
	sha1sum ${BINARY}

clean:
	cabal clean
	rm -rf build
	rm -f ${NAME}*.tar.gz
