dev_in_root = function() "inst" %in% list.dirs(full.names = FALSE)
dev_in_dev = function() "dev.cpp" %in% list.files(full.names = FALSE)
dev_in_test = function() "testthat" %in% list.dirs("..", full.names = FALSE)
dev_in_vig = function() "vignette_status.Rmd" %in% list.files(full.names = FALSE)

dev_file = function(suffix = "", ext = "cpp") {
  cpp = function(path) {
    pp = file.path(path, sprintf("dev%s.%s", suffix, ext))
    print(pp)
    print(file.exists(pp))
    print(getwd())
    pp
  }
  if (dev_in_root()) return(cpp("misc/dev"))
  if (dev_in_dev()) return(cpp(""))
  if (dev_in_test()) return(cpp("../../misc/dev"))

  msg_break(
    msg_colon("You are developing here", getwd()),
    msg(
      "which is not where you should be developing.",
      "The current options are in the root of macpan2,",
      "or in misc/dev or tests within a macpan2 project."
    )
  ) |> stop()
}

dev_obj = function(suffix = "", ext = "cpp") {
  tools::file_path_sans_ext(dev_file(suffix = suffix, ext = ext))
}

dev_choose_cpp = function(suffix = "", ext = "cpp") {
  if (interactive()) return("macpan2")
  dev_compile(suffix = suffix, ext = ext)
  dev_obj(suffix = suffix, ext = ext)
}

dev_compile = function(suffix = "", ext = "cpp") {
  ff = dev_file(suffix = suffix, ext = ext)
  Rcpp_flags = paste(
        "-I", system.file("include", package = "TMB")
      , "-I", system.file("include", package = "Rcpp")
      , Rcpp:::RcppLdFlags()
  )
  TMB::compile(ff, flags = Rcpp_flags)
  dyn.load(TMB::dynlib(dev_obj(suffix = suffix, ext = ext)))
}

render_model_readme = function(file) {
  rmarkdown::render(input = file, output_format = "md_document", intermediates_dir = NULL)
  f = basename(file) |> tools::file_path_sans_ext()
  output_file = sprintf("%s.md", f)
  d = dirname(file)
  output_lines = readLines(file.path(d, output_file))
  input_lines = readLines(file)
  potential_yaml = which(grepl("^---", input_lines))
  if (potential_yaml[1L] == 1L) {
    if (length(potential_yaml) > 1L) {
      file[seq_len(potential_yaml[2L])]
    }
  }
}
