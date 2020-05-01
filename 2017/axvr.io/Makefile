all: src/2sp src/gen

src/alist.o: src/alist.scm
	csc -c -j alist src/alist.scm

src/gen: src/alist.o src/gen.scm
	csc -c src/gen.scm
	csc src/gen.o src/alist.o -o src/gen

src/2sp: src/2sp.scm
	csc -o src/2sp src/2sp.scm

serve:
	cd dist && python3 -m http.server 8000

clean:
	rm -f src/*.o
	rm -f alist.import.scm
	rm -f src/2sp
	rm -f scriprs/gen

site: all
	./src/build.sh

watch: all
	WATCH_DIR=src watch-dir make build

.PHONY: all serve site watch clean
