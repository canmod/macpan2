export R_MAKEVARS_USER := $(PWD)/src/Makevars


COMMA_RE = ^\([, ]*\)
ENUM_RE = [ ]*MP2_[A-Z_]*[ ]*=[ ]*[0-9][0-9]*
SED_RE = \(\,\)*[ ]*\/\/[ ]*\(.*\)
ALIAS_RE = [ ]*MP2_\(.*\)\: \(.*\)(\(.*\))
ROXY_RE = ^.*\(\#'.*\)$
LOG_RE = std\:\:string[ ]*bail_out_log_file
VERSION := $(shell sed -n '/^Version: /s///p' DESCRIPTION)
TEST := testthat::test_package(\"macpan2\", reporter = \"progress\")


all:
	make full-install
	make pkg-check


install-deps:
	Rscript -e "remotes::install_deps()"
	Rscript -e "remotes::install_github('canmod/oor@validity')"
	Rscript -e "install.packages('TMB', type = 'source', repos = 'https://cran.r-project.org')"

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
	@echo "Using the following Makevars file:"
	@echo $(R_MAKEVARS_USER)
	R CMD INSTALL --no-multiarch --install-tests .


quick-doc-install: R/*.R misc/dev/dev.cpp
	make engine-doc-update
	make doc-update
	make quick-install

forced-quick-doc-install: 
	touch misc/old-r-source/*.R
	make doc-update
	make quick-install


quick-test-all:
	make quick-doc-install
	make run-vignette-code
	make run-tests
	make run-examples
	make run-starter-readme-code

quick-test:
	make quick-doc-install
	make run-tests

run-starter-readme-code:
	Rscript misc/build/run_starter_readme_code.R

run-examples:
	Rscript misc/build/run_examples.R


run-tests:
	Rscript -e "library(macpan2); $(TEST)"


run-vignette-code:
	cd vignettes
	Rscript misc/build/run_vignette_code.R


coverage-report:: coverage.html
coverage.html: R/*.R src/macpan2.cpp tests/testthat/*.R
	#Rscript -e "covr::report(file = \"coverage.html\")"
	# Some evidence that this version might be more portable
	Rscript -e "covr::report(covr::package_coverage(quiet = FALSE), file = \"coverage.html\")"


misc/dev/%.run: misc/dev/%.R
	cd misc/dev; Rscript ../../$^


misc/**/%.svg: misc/**/%.drawio
	draw.io --export $^ --format svg


misc/**/%.png: misc/**/%.drawio
	draw.io --export $^ --format png


svg-readme:: misc/readme/*.svg
png-readme:: misc/readme/*.png


readme:: README.md
README.md: README.Rmd R/*.R NAMESPACE
	Rscript -e "rmarkdown::render('README.Rmd')"
	echo '<!-- Auto-generated - do not edit by hand -->' > temp
	echo '<!-- Edit README.Rmd instead -->' | cat - $@ >> temp && mv temp $@


push-readme:
	make README.md
	git add man/figures/*.svg
	git add man/figures/*.png
	git add man/figures/*.drawio
	git add README.md README.Rmd
	git commit -m "[skip ci] update readme" || true
	git push || true


enum-update:: R/enum.R
R/enum.R: misc/dev/dev.cpp misc/build/enum_tail.R
	echo "## Auto-generated - do not edit by hand" > $@
	echo "valid_func_sigs = c(" >> $@
	grep "$(COMMA_RE)$(ENUM_RE)" misc/dev/dev.cpp | sed 's/$(COMMA_RE)$(ENUM_RE)$(SED_RE)/  \1\"\3\"\2/' >> $@
	echo ")" >> $@
	grep "$(LOG_RE)" misc/dev/dev.cpp | sed 's|$(LOG_RE)|bail_out_log_file|' | sed 's|;||' >> $@
	cat misc/build/enum_tail.R >> $@
	echo "valid_funcs = setNames(as.list(valid_funcs), valid_funcs)" >> $@


enum-meth-update:: R/enum_methods.R
R/enum_methods.R: misc/dev/dev.cpp misc/build/method_head.R misc/build/build_from_enum_methods.R
	Rscript misc/build/build_from_enum_methods.R


src-update:: src/macpan2.cpp
src/macpan2.cpp: misc/dev/dev.cpp src/Makevars
	echo "// Auto-generated - do not edit by hand" > $@
	sed "s/#define MP_VERBOSE//" misc/dev/dev.cpp >> $@


engine-doc-update:: R/engine_functions.R
R/engine_functions.R: src/macpan2.cpp
	echo "## Auto-generated - do not edit by hand" > $@
	echo "" >> $@
	grep "$(ROXY_RE)" $^ | sed "s/$(ROXY_RE)/\1/" >> $@
	echo "#' @name engine_functions" >> $@
	grep "$(COMMA_RE)$(ENUM_RE)" $^ | sed "s/$(COMMA_RE)$(ALIAS_RE)/#' @aliases \3/" >> $@
	echo "NULL" >> $@


doc-update: R/*.R misc/dev/dev.cpp misc/old-r-source/*.R misc/build/roxygenise.R
	Rscript misc/build/roxygenise.R
	touch doc-update


pkg-build:: ../macpan2_$(VERSION).tar.gz
../macpan2_$(VERSION).tar.gz: .Rbuildignore DESCRIPTION man/*.Rd R/*.R src/*.cpp tests/testthat/test-*.R tests/testthat.R inst/starter_models/**/tmb.R doc-update
	cd .. && R CMD build --no-build-vignettes macpan2
	touch pkg-build


pkg-check: ../macpan2_$(VERSION).tar.gz pkg-build
	cd .. && R CMD check macpan2_$(VERSION).tar.gz
	touch pkg-check


pkg-install: ../macpan2_$(VERSION).tar.gz pkg-build
	cd .. && R CMD INSTALL --no-multiarch --install-tests macpan2_$(VERSION).tar.gz


compile-dev: misc/dev/dev.cpp
	cd misc/dev; echo "TMB::compile(\"dev.cpp\")" | R --slave


inst/starter_models/%/README.md: inst/starter_models/%/README.Rmd DESCRIPTION R/*.R
	echo "rmarkdown::render(\"$<\")" | R --slave

inst/starter_models/%/README.push: inst/starter_models/%/README.md
	@echo Pushing directory: $(dir $^)
	git add -u $(dir $^)
	git commit -m "[skip ci] work on starter models"
	git push
	touch $@

all-starters: inst/starter_models/*/README.md

pkgdown: 
	Rscript -e "pkgdown::build_site()"
	Rscript -e "pkgdown::preview_site()"

pkgdown-reference-index:
	make quick-doc-install
	Rscript -e "pkgdown::build_reference_index()"
	Rscript -e "pkgdown::preview_site()"

NEWS.md : news-narratives.md misc/build/update-news.sh misc/build/update-commit-version-map.sh misc/build/update-version-bumps.sh DESCRIPTION R/*.R vignettes/*.Rmd man/*.Rd tests/testthat/*.R Makefile inst/starter_models/**/*.R inst/starter_models/**/*.Rmd
	./misc/build/update-commit-version-map.sh
	./misc/build/update-version-bumps.sh
	./misc/build/update-news.sh
