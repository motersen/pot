PREFIX = /usr

# should be gsc or gambitc
GSC := gambitc

sources := srfi-1.scm io.scm list-procedures.scm db.scm parser.scm \
	cli-parser.scm main.scm
transpiled := $(sources:.scm=.c)
linkfile := link.c
cfiles := $(transpiled) $(linkfile)
objects := $(cfiles:.c=.o)

pot: $(objects)
	$(GSC) -exe -o pot $(objects)

clean:
	rm -f $(objects) $(cfiles)

install: build
	install -D pot $(DESTDIR)$(PREFIX)/bin/pot
	install -Dm644 LICENSE $(DESTDIR)$(PREFIX)/share/licenses/pot/LICENSE

%.o: %.c
	$(GSC) -obj $<

$(linkfile): $(transpiled)
	$(GSC) -o $(linkfile) -link $(transpiled)

%.c: %.scm
	$(GSC) -c $<

.PHONY: build clean install
