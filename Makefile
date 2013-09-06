all:
	obuild configure --enable-tests
	obuild build

install:
	ocamlfind install mips32dis src/META $(wildcard dist/build/lib-mips32dis/*)

uninstall:
	ocamlfind remove mips32dis

test:
	obuild test

clean:
	rm -rf dist
