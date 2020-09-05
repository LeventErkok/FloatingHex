# (c) Copyright Levent Erkok. All rights reserved.
#
# The FloatingHex library is distributed with the BSD3 license. See the LICENSE file
# in the distribution for details.
SHELL     := /usr/bin/env bash
TSTSRCS   = $(shell find . -name '*.hs' -or -name '*.lhs' | grep -v Setup.hs)
DEPSRCS   = $(shell find . -name '*.hs' -or -name '*.lhs' -or -name '*.cabal' | grep -v Paths_FloatingHex.hs)
CABAL     = cabal
TIME      = /usr/bin/time

define mkTags
	@find . -name \*.\*hs | xargs fast-tags
endef

.PHONY: all install sdist clean docs hlint tags

all: install

install: $(DEPSRCS) Makefile
	$(call mkTags)
	@$(TIME) cabal new-install --lib

test: install
	@echo "*** Starting inline tests.."
	@$(TIME) doctest Data/Numbers/FloatingHex --fast --no-magic -package random

sdist: install
	@(set -o pipefail; $(CABAL) sdist)

veryclean: clean

clean:
	@rm -rf dist

docs:
	cabal new-haddock --haddock-option=--hyperlinked-source --haddock-option=--no-warnings

release: clean install sdist hlint test docs
	@echo "*** FloatingHex is ready for release!"

hlint:
	@echo "Running HLint.."
	@hlint Data -i "Use otherwise" -i "Parse error"

tags:
	$(call mkTags)
