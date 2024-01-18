dev = readLines("misc/dev/dev.cpp")
meth_head = readLines("misc/build/method_head.R")
meth_tail = readLines("misc/build/method_tail.R")
repl = function(x, pat, repl) sub(pat, repl, x, perl = TRUE)
re = "^[, ]*[ ]*(METH_[A-Z_]*)[ ]*=[ ]*[0-9][0-9]*(\\,)*[ ]*//[ ]*(.*)"
meth_lines = (re
  |> grep(dev, value = TRUE)
)
meth_names = (meth_lines
  |> repl(re, "\\1")
  |> tolower()
)
meth_defs = (meth_lines
  |> repl(re, "MethodPrototype(\\3)")
)
output_lines = c("## Auto-generated - do not edit by hand"
  , ""
  , meth_head
  , ""
  , "MethodTypes = function() {"
  , "  self = MethodTypeUtils()"
  , sprintf("  self$method_ordering = c(%s)", paste0(
        sprintf('"%s"', meth_names),
        collapse = ", "))
  , sprintf("  self$%s = %s", meth_names, meth_defs)
  , "  return_object(self, \"MethodTypes\")"
  , "}"
)
writeLines(output_lines, "R/enum_methods.R")
