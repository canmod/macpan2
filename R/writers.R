Writer = function(...) {
  self = Base()
  self$file = normalizePath(file.path(...), mustWork = TRUE)
  self$write = function(object, overwrite = FALSE) {
    suggestion = switch(file_ext(self$file)
      , csv = "CSVWriter"
      , json = "JSONWriter"
      , "TXTWriter" # default suggestion
    )
    stop(
      "\nAbstract class that is not implemented.",
      "\nPlease try a specific writer like ",
      suggestion, "."
    )
  }
  return_object(self, "Writer")
}

CSVWriter = function(...) {
  self = Writer(...)
  self$.empty = function(row) {
    isTRUE(all((row == "") | startsWith(row, " ")))
  }
  self$write = function(object, overwrite = FALSE) {
    if (file.exists(self$file) & !overwrite) {
      stop(
        sprintf("File %s already exists."), "\n",
        "Please use overwrite = TRUE if you really want to do so."
      )
    }
    object = as.data.frame(object)
    for (c in names(object)) object$c = as.character(object$c)
    write.table(object, self$file
      , sep = ","
      , quote = ""
      , na = ""
      , row.names = FALSE
    )
  }
  return_object(self, "CSVWriter")
}
