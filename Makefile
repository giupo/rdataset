# Makefile for generating R packages.
# 2011 Andrew Redd
# 2014 Giuseppe Acito

PKG_VERSION=$(shell grep -i ^version DESCRIPTION | cut -d : -d \  -f 2)
PKG_NAME=$(shell grep -i ^package DESCRIPTION | cut -d : -d \  -f 2)

R_BIN ?= R
RSCRIPT_BIN ?= Rscript

R_FILES := $(wildcard R/*.[R|r])
SRC_FILES := $(wildcard src/*) $(addprefix src/, $(COPY_SRC))
PKG_FILES := DESCRIPTION NAMESPACE $(R_FILES) $(SRC_FILES)

.PHONY: NAMESPACE tarball clean CHANGELOG.md

tarball:$(PKG_NAME)_$(PKG_VERSION).tar.gz 

$(PKG_NAME)_$(PKG_VERSION).tar.gz: $(PKG_FILES)
	$(R_BIN) CMD build .

check:
	$(RSCRIPT_BIN) -e 'devtools::check()'

build: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	$(R_BIN) --vanilla CMD INSTALL --build $(PKG_NAME)_$(PKG_VERSION).tar.gz

install: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	$(R_BIN) --vanilla CMD INSTALL $(PKG_NAME)_$(PKG_VERSION).tar.gz

NAMESPACE:
	$(RSCRIPT_BIN) -e "devtools::document()"

DOCS: NAMESPACE

clean:
	-rm -f $(PKG_NAME)_*.tar.gz
	-rm -r -f $(PKG_NAME).Rcheck
	-rm -r -f src/*.o src/*.so

.PHONY: list

list:
	@echo "R files:"
	@echo $(R_FILES)
	@echo "Source files:"
	@echo $(SRC_FILES)

test:
	$(RSCRIPT_BIN) -e 'devtools::test()'

autotest:
	$(RSCRIPT_BIN) -e 'testthat::auto_test_package()'


coverage:
	$(RSCRIPT_BIN) -e 'covr::package_coverage()'

NEWS.md:
	@gitchangelog | grep -v "git-svn-id" > NEWS.md

changelog: NEWS.md
