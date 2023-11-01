COMMA_RE = ^\([, ]*\)
ENUM_RE = [ ]*MP2_[A-Z_]*[ ]*=[ ]*[0-9][0-9]*
SED_RE = \(\,\)*[ ]*\/\/[ ]*\(.*\)
ALIAS_RE = [ ]*MP2_\(.*\)\: \(.*\)(\(.*\))
ROXY_RE = ^.*\(\#'.*\)$
VERSION := $(shell sed -n '/^Version: /s///p' DESCRIPTION)
TEST := testthat::test_package(\"macpan2\", reporter = \"progress\")


all:
	make full-install
	make pkg-check


install-deps:
	Rscript -e "remotes::install_github('canmod/oor@validity')"


# Use this rule if you are doing R development or want to promote
# dev.cpp to macpan2.cpp before doing R development.
full-install:
	make src-update
	make enum-update
	make enum-meth-update
	make engine-doc-update
	make doc-update
	make pkg-build
	make pkg-install


# Use this rule if (1) you are in a development cycle, (2) you
# haven't updated macpan.cpp (but have perhaps modified dev.cpp)
# and (3) do not require a roxygen update.
quick-install: enum-update enum-meth-update
	R CMD INSTALL --no-multiarch --install-tests .


quick-doc-install: R/*.R misc/dev/dev.cpp
	make engine-doc-update
	make doc-update
	make quick-install


quick-test-all:
	make quick-doc-install
	make run-vignette-code
	make run-tests
	make run-examples


quick-test:
	make quick-doc-install
	make run-tests


run-examples:
	Rscript misc/build/run_examples.R


run-tests:
	Rscript -e "library(macpan2); $(TEST)"


run-vignette-code:
	cd vignettes
	Rscript misc/build/run_vignette_code.R


coverage-report:: coverage.html
coverage.html: R/*.R src/macpan2.cpp tests/testthat/*.R
	Rscript -e "covr::report(file = \"coverage.html\")"


enum-update:: R/enum.R
R/enum.R: misc/dev/dev.cpp misc/build/enum_tail.R
	echo "## Auto-generated - do not edit by hand" > $@
	echo "valid_func_sigs = c(" >> $@
	grep "$(COMMA_RE)$(ENUM_RE)" misc/dev/dev.cpp | sed 's/$(COMMA_RE)$(ENUM_RE)$(SED_RE)/  \1\"\3\"\2/' >> $@
	echo ")" >> $@
	cat misc/build/enum_tail.R >> $@
	echo "valid_funcs = setNames(as.list(valid_funcs), valid_funcs)" >> $@


enum-meth-update:: R/enum_methods.R
R/enum_methods.R: misc/dev/dev.cpp misc/build/method_head.R misc/build/build_from_enum_methods.R
	Rscript misc/build/build_from_enum_methods.R


src-update:: src/macpan2.cpp
src/macpan2.cpp: misc/dev/dev.cpp
	echo "// Auto-generated - do not edit by hand" > $@
	sed "s/#define MP_VERBOSE//" $^ >> $@


engine-doc-update:: R/engine_functions.R
R/engine_functions.R: src/macpan2.cpp
	echo "## Auto-generated - do not edit by hand" > $@
	echo "" >> $@
	grep "$(ROXY_RE)" $^ | sed "s/$(ROXY_RE)/\1/" >> $@
	echo "#' @name engine_functions" >> $@
	grep "$(COMMA_RE)$(ENUM_RE)" $^ | sed "s/$(COMMA_RE)$(ALIAS_RE)/#' @aliases \3/" >> $@
	echo "NULL" >> $@


doc-update: R/*.R misc/dev/dev.cpp
	echo "suppressWarnings(roxygen2::roxygenize(\".\",roclets = c(\"collate\", \"rd\", \"namespace\")))" | R --slave


pkg-build:: ../macpan2_$(VERSION).tar.gz
../macpan2_$(VERSION).tar.gz: DESCRIPTION man/*.Rd R/*.R src/*.cpp tests/testthat/test-*.R tests/testthat.R inst/starter_models/**/*.csv inst/starter_models/**/*.json
	cd .. && R CMD build --no-build-vignettes macpan2


pkg-check: ../macpan2_$(VERSION).tar.gz
	cd .. && R CMD check macpan2_$(VERSION).tar.gz


pkg-install: ../macpan2_$(VERSION).tar.gz
	cd .. && R CMD INSTALL --no-multiarch --install-tests macpan2_$(VERSION).tar.gz


compile-dev: misc/dev/dev.cpp
	cd misc/dev; echo "TMB::compile(\"dev.cpp\")" | R --slave


misc/dev/%.run: misc/dev/%.R
	cd misc/dev; Rscript ../../$^
