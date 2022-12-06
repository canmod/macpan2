ENUM_RE = ^[ ]*MP2_[A-Z_]*[ ]*=[ ]*[0-9][0-9]*
SED_RE = \(\,\)*[ ]*\/\/[ ]*\(.*\)
#VERSION := $(shell sed -n '/^Version: /s///p' DESCRIPTION)
VERSION = 0.0.0.9000

all: R/enum.R doc-update pkg-build pkg-install


R/enum.R: misc/experiments/structured_expressions/macpan2.cpp
	echo "## Auto-generated - do not edit by hand" > $@
	echo "valid_funcs = c(" >> $@
	grep "$(ENUM_RE)" $^ | sed 's/$(ENUM_RE)$(SED_RE)/  \"\2\"\1/' >> $@
	#grep "$(ENUM_RE)" misc/experiments/structured_expressions/macpan2.cpp | sed 's/$(ENUM_RE)$(SED_RE)/  \"\2\"\1/' >> $@
	echo ")" >> $@
	echo "valid_funcs = setNames(as.list(valid_funcs), valid_funcs)" >> $@


doc-update: R/*.R
	echo "suppressWarnings(roxygen2::roxygenize(\".\",roclets = c(\"collate\", \"rd\", \"namespace\")))" | R --slave

pkg-build:
	R CMD build .

pkg-check:
	R CMD check macpan2_$(VERSION).tar.gz

pkg-install:
	R CMD INSTALL macpan2_$(VERSION).tar.gz
