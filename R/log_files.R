#' Log File
#'
#' @param directory Directory within which to try to store a log file for a
#' model object. If appropriate file access is not available, a temporary
#' file will be used instead.
#'
#' @returns Object of class `LogFile` with the following methods.
#'
#' * `$log()` -- Character vector containing the lines in the log file.
#' * `$data_arg()` -- List containing the components of the `TMB` data
#' structure related to log files.
#' * `$copy(...)` -- Make a copy of the log file at `file.path(...)`.
#' * `$err_msg()` -- Return the current error message in the log file, if any.
#' * Other methods inherited from \code{\link{Files}}
#'
#' @export
LogFile = function(directory = tempdir()) {
  self = Files(fix_dir(directory), reader_spec("log.txt", TXTReader))
  self$log = function() self$get("log")
  self$data_arg = function() list(log_file = self$.file_path("log"))
  self$copy = function(...) file.copy(self$.file_path("log"), file.path(...))
  self$err_msg = function() {
    re = "^Error message = "
    m = grep(re, self$log(), value = TRUE)
    sub(re, "", m)
  }
  return_object(self, "LogFile")
}

make_file = function(directory) {
  file_path = file.path(directory, "log.txt")
  file.create(file_path)
  file_path
}
fix_dir = function(directory) {
  file_path = make_file(directory)
  if (file.access(file_path, c(0, 2, 4)) == -1L) {
    directory = tempdir()
    file_path = make_file(directory)
  }
  directory
}
