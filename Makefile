default: install

# Building

clean:
	cabal clean

configure:
	cabal configure --enable-library-profiling --enable-executable-profiling --enable-tests

build: configure
	cabal build --ghc-options=-Werror

test: build
	cabal test --test-option=--hide-successes --test-option=--color

haddock: configure
	cabal haddock

copy: build test haddock
	cabal copy

~/.local/share/gtksourceview-2.0/language-specs/argo.lang: argo.lang
	mkdir -p ~/.local/share/gtksourceview-2.0/language-specs
	cp $< $@

install-lang: ~/.local/share/gtksourceview-2.0/language-specs/argo.lang

install: install-lang
	-ghc-pkg unregister yops
	cabal install --user --ghc-options=-Werror --enable-library-profiling --enable-executable-profiling --enable-tests

sdist: clean configure
	cabal sdist

# switch off intermediate file deletion
.SECONDARY:

.PHONY: default clean configure build haddock copy install install-lang test sdist
