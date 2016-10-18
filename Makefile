PREFIX = /usr
MANPREFIX = $(PREFIX)/share/man

GSC := gsc
GAMBIT_LIBDIR := $(shell gsi -e '(println (path-expand "~~lib"))')
GAMBIT_INCDIR := $(shell gsi -e '(println (path-expand "~~include"))')

CC := musl-gcc
CFLAGS := -static -O2 -idirafter $(GAMBIT_INCDIR)
LIBS := -lgambit -lm -lc -ldl -lutil

sources := srfi-1.scm io.scm exceptions.scm list-procedures.scm db.scm \
	parser.scm cli-parser.scm main.scm
transpiled := $(sources:.scm=.c)
linkfile := link.c
cfiles := $(transpiled) $(linkfile)

pot: $(cfiles)
	$(CC) -o pot -L$(GAMBIT_LIBDIR) $(CFLAGS) $(cfiles) $(LIBS)
	strip pot

clean:
	rm -f $(cfiles)

install: build
	install -D pot $(DESTDIR)$(PREFIX)/bin/pot
	install -Dm644 pot.1 $(DESTDIR)$(MANPREFIX)/man1/pot.1
	install -Dm644 LICENSE $(DESTDIR)$(PREFIX)/share/licenses/pot/LICENSE

$(linkfile): $(transpiled)
	$(GSC) -o $(linkfile) -link $(transpiled)

%.c: %.scm
	$(GSC) -c $<

.PHONY: build clean install
