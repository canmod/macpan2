## using load_source because it avoids triggering 
## recompilation (https://github.com/r-lib/roxygen2/issues/771), but
## this choice seems to mean that we will not be able to
## use markdown code chunks to produce Rd files
code_loader = roxygen2::load_source
suppressWarnings(roxygen2::roxygenize("."
  , roclets = c("collate", "rd", "namespace")
  , load_code = code_loader
))
