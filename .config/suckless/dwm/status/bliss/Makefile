LISP := sbcl
BIN := bliss.lisp blocks.lisp config.lisp package.lisp bliss.asd
PREFIX := /usr/local

all: bliss

bliss: $(BIN)
	$(LISP) --load bliss.asd \
		--eval '(ql:quickload :bliss)' \
		--eval '(asdf:make :bliss)' \
		--eval '(quit)'

install: bliss
	mkdir -p $(DESTDIR)$(PREFIX)/bin
	mkdir -p $(HOME)/.config/bliss
	cp config.lisp $(HOME)/.config/bliss
	cp $^ $(DESTDIR)$(PREFIX)/bin

uninstall:
	$(RM) -rf $(DESTDIR)$(PREFIX)/bin/bliss

clean:
	$(RM) -rf bliss

.PHONY: install uninstall
