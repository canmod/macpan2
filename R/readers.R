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
  
  ## wrapper for handling errors in the reading functions
  self$read_base = function() {
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
  self$read = function() {
    x = try(self$read_base(), silent = TRUE)
    if (inherits(x, "try-error")) {
      stop(x, "\nCouldn't read this file:\n", self$file)
    }
    x
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
  self$read_base = function() {
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
  self$read_base = function() {
    l = jsonlite::fromJSON(self$file
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
  self$read_base = function() readLines(self$file)
  return_object(self, "TXTReader")
}

#' @describeIn Reader Read R files.
#' @export
RReader = function(...) {
  self = Reader(...)
  self$read_base = function() readLines(self$file)
  return_object(self, "RReader")
}

#' @describeIn Reader Placeholder reader that always returns \code{NULL}.
#' @export
NULLReader = function(...) {
  self = Base()
  self$file = ""
  self$read = function() NULL
  return_object(self, "NULLReader")
}
