# Simple makefile
all: install

install: module.lang
	mkdir -p ~/.local/share/gtksourceview-2.0/language-specs
	cp -f ./*.lang ~/.local/share/gtksourceview-2.0/language-specs/
	mkdir -p ~/.local/share/gtksourceview-3.0/language-specs
	cp -f ./*.lang ~/.local/share/gtksourceview-3.0/language-specs/


.PHONY: install

