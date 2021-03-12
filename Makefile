PACKAGE = ox-zola
VERSION = 0.1  # TODO: Pull this from the library file when present
EMACS ?= emacs
CASK  ?= cask
LANG = en_US.UTF-8

.PHONY: test

test:
	${CASK} exec ert-runner
