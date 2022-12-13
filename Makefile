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


# Use this rule if you are doing R development or want to promote
# on dev.cpp to macpan2.cpp before doing R development.
R-dev-install:
	make src-update
	make enum-update
	make engine-doc-update
	make doc-update
	make pkg-build
	make pkg-install


# Use this rule if you are developing dev.cpp and need to update the
# R package because someone madded new engine functions and therefore
# needs to update enum.R or R-side changes need to be present for
# dev.cpp to work as expected.
cpp-dev-install:
	make enum-update
	make quick-install


quick-install: enum-update
	R CMD INSTALL . --no-multiarch


enum-update:: R/enum.R
R/enum.R: misc/dev/dev.cpp
	echo "## Auto-generated - do not edit by hand" > $@
	echo "valid_funcs = c(" >> $@
	grep "$(ENUM_RE)" $^ | sed 's/$(ENUM_RE)$(SED_RE)/  \"\2\"\1/' >> $@
	echo ")" >> $@
	echo "valid_funcs = setNames(as.list(valid_funcs), valid_funcs)" >> $@


src-update:: src/macpan2.cpp
src/macpan2.cpp: misc/dev/dev.cpp
	echo "// Auto-generated - do not edit by hand" > $@
	echo "" >> $@
	cat $^ >> $@


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
