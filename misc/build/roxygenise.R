if (require(macpan2)) {
  code_loader = roxygen2::load_installed
} else {
  code_loader = roxygen2::load_source
}
suppressWarnings(roxygen2::roxygenize("."
  , roclets = c("collate", "rd", "namespace")
  , load_code = code_loader
))
