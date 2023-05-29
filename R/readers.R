#' Reader
#'
#' Construct objects for reading data.
#'
#' @param ... Character vectors giving path components to the file to be read.
#'
#' @importFrom tools file_ext
#' @export
Reader = function(...) {
  self = Base()
  self$file = normalizePath(file.path(...), mustWork = TRUE)
  self$read = function() {
    suggestion = switch(file_ext(self$file)
      , csv = "CSVReader"
      , json = "JSONReader"
      , "TXTReader" # default suggestion
    )
    stop(
      "\nAbstract class that is not implemented.",
      "\nPlease try a specific reader like ",
      suggestion, "."
    )
  }
  return_object(self, "Reader")
}

#' @describeIn Reader Read CSV files.
#' @importFrom utils read.table
#' @export
CSVReader = function(...) {
  self = Reader(...)
  self$.empty = function(row) {
    isTRUE(all((row == "") | startsWith(row, " ")))
  }
  self$read = function() {
    data_frame = read.table(
      self$file, sep = ",", quote = "", na.strings = character(0L),
      colClasses = "character", header = TRUE,
      strip.white = TRUE, blank.lines.skip = TRUE,
      stringsAsFactors = TRUE
    )
    if (nrow(data_frame) == 0L) return(data_frame)
    data_frame[!apply(data_frame, 1, self$.empty), , drop = FALSE]
  }
  return_object(self, "CSVReader")
}

#' @describeIn Reader Read JSON files.
#' @importFrom jsonlite fromJSON
#' @export
JSONReader = function(...) {
  self = Reader(...)
  self$read = function() {
    jsonlite::fromJSON(self$file
      , simplifyDataFrame = FALSE
      , simplifyMatrix = FALSE
    )
  }
  return_object(self, "JSONReader")
}

#' @describeIn Reader Read TXT files.
#' @export
TXTReader = function(...) {
  self = Reader(...)
  self$read = function() readLines(self$file)
  return_object(self, "TXTReader")
}

#' @describeIn Reader Placeholder reader that always returns \code{NULL}.
#' @export
NULLReader = function(...) {
  self = self = Base()
  self$file = ""
  self$read = function() NULL
  return_object(self, "NULLReader")
}
