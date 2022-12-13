ENUM_RE = ^[ ]*MP2_[A-Z_]*[ ]*=[ ]*[0-9][0-9]*
SED_RE = \(\,\)*[ ]*\/\/[ ]*\(.*\)
ROXY_RE = ^.*\(\#'.*\)$
VERSION := $(shell sed -n '/^Version: /s///p' DESCRIPTION)

all:
	make src-update
	make enum-update
	make engine-doc-update
	make doc-update
	make pkg-build
	make pkg-install
	make pkg-check


# Avoid package checks
cpp-dev-update:
	make src-update
	make enum-update
	make quick-install


src-update:: src/macpan2.cpp
src/macpan2.cpp: misc/dev/dev.cpp
	echo "// Auto-generated - do not edit by hand" > $@
	echo "" >> $@
	cat $^ >> $@

enum-update:: R/enum.R
R/enum.R: src/macpan2.cpp
	echo "## Auto-generated - do not edit by hand" > $@
	echo "valid_funcs = c(" >> $@
	grep "$(ENUM_RE)" $^ | sed 's/$(ENUM_RE)$(SED_RE)/  \"\2\"\1/' >> $@
	echo ")" >> $@
	echo "valid_funcs = setNames(as.list(valid_funcs), valid_funcs)" >> $@

engine-doc-update:: R/engine_functions.R
R/engine_functions.R: src/macpan2.cpp
	echo "## Auto-generated - do not edit by hand" > $@
	echo "" >> $@
	grep "$(ROXY_RE)" $^ | sed "s/$(ROXY_RE)/\1/" >> $@
	echo "#' @name engine_functions" >> $@
	echo "NULL" >> $@


doc-update: R/*.R
	echo "suppressWarnings(roxygen2::roxygenize(\".\",roclets = c(\"collate\", \"rd\", \"namespace\")))" | R --slave

pkg-build:: macpan2_$(VERSION).tar.gz
macpan2_$(VERSION).tar.gz: R/*.R src/*.cpp
	R CMD build .

pkg-check: macpan2_$(VERSION).tar.gz
	R CMD check macpan2_$(VERSION).tar.gz

pkg-install: macpan2_$(VERSION).tar.gz
	R CMD INSTALL macpan2_$(VERSION).tar.gz

quick-install: enum-update
	R CMD INSTALL .
