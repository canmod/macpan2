#' Log File
#'
#' @param directory Directory within which to store a log file for a
#' model object.
#'
#' @export
LogFile = function(directory = tempdir()) {
  file.create(file.path(directory, "log.txt"))
  self = Files(directory, reader_spec("log.txt", TXTReader))
  self$log = function() self$get("log")
  self$data_arg = function() list(log_file = self$.file_path("log"))
  return_object(self, "LogFile")
}
