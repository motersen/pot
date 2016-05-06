PREFIX = /usr

# should be gsc or gambitc
GSC := gambitc
GAMBIT_LIBDIR := $(shell gsi -e '(println (path-expand "~~lib"))')

sources := srfi-1.scm io.scm list-procedures.scm db.scm parser.scm \
	cli-parser.scm main.scm
transpiled := $(sources:.scm=.c)
linkfile := link.c
cfiles := $(transpiled) $(linkfile)

pot: $(cfiles)
	gcc -o pot -L$(GAMBIT_LIBDIR) -O2 $(cfiles) -lgambit -lm -lc -ldl -lutil

clean:
	rm -f $(cfiles)

install: build
	install -D pot $(DESTDIR)$(PREFIX)/bin/pot
	install -Dm644 LICENSE $(DESTDIR)$(PREFIX)/share/licenses/pot/LICENSE

$(linkfile): $(transpiled)
	$(GSC) -o $(linkfile) -link $(transpiled)

%.c: %.scm
	$(GSC) -c $<

.PHONY: build clean install
