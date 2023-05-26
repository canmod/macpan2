library(macpan2)
library(oor)



EditorBase = function(parent, arg_ref) {
  self = Base()
  self$parent = parent
  self$arg_ref = arg_ref
  self$.dots = function(...) {
    as.character(unlist(list(...), use.names = FALSE))
  }
  return_object(self, "EditorBase")
}
MakeRemoveChar = function(parent, arg_ref) {
  self = EditorBase(parent, arg_ref)
  function(...) {
    p = self$parent
    p$.predefined_args[[self$arg_ref]] = base::setdiff(
      p$.predefined_args[[self$arg_ref]],
      self$.dots(...)
    )
    p$regenerate()
  }
}
MakeAddChar = function(parent, arg_ref) {
  self = EditorBase(parent, arg_ref)
  function(...) {
    p = self$parent
    p$.predefined_args[[self$arg_ref]] = unique(c(
      p$.predefined_args[[self$arg_ref]],
      self$.dots(...)
    ))
    p$regenerate()
  }
}
MakeReplaceChar = function(parent, arg_ref) {
  self = EditorBase(parent, arg_ref)
  function(...) {
    p = self$parent
    p$.predefined_args[[self$arg_ref]] = self$.dots(...)
    p$regenerate()
  }
}
MakeEditList = function(list_ref) {
  function(parent, arg_ref) {
    self = EditorBase(parent, arg_ref)
    self$.list_ref = list_ref
    self$.dots = function(...) list(...)
    function(...) {
      p = self$parent
      new_args = macpan2:::valid$named_list$assert(self$.dots(...))
      for (ref in names(new_args)) {
        if (self$.list_ref == ".predefined_args") {
          p[[self$.list_ref]][[self$arg_ref]][[ref]] = new_args[[ref]]
        } else {
          p[[self$.list_ref]][[ref]] = new_args[[ref]]
        }
      }
      p$regenerate()
    }
  }
}
MakeInsertList = function(list_ref) {
  function(parent, arg_ref) {
    self = EditorBase(parent, arg_ref)
    self$.list_ref = list_ref
    self$.dots = function(...) list(...)
    function(..., .at = 1L) {
      p = self$parent
      new_args = self$.dots(...)
      p[[self$.list_ref]][[self$arg_ref]] = append(
        p[[self$.list_ref]][[self$arg_ref]],
        new_args,
        after = .at - 1L
      )
      p$regenerate()
    }
  }
}

#' Arguments
#'
#' Class for representing arguments to a constructor. This class exists so that
#' \code{EditableArgs} can inherit from it.
#'
#' @param constructor Function for constructing a list-like object.
#' @param dot_args List of arguments to the \code{constructor} that enter
#' through dots.
#' @param predefined_args List of arguments to the \code{constructor} that are
#' named in the signature of the \code{constructor}.
#'
#' ## Methods
#'
#' * `$regenerate()` -- Create an object with the current set of arguments
#' by calling \code{constructor}.
Args = function(constructor, dot_args, predefined_args) {
  self = Base()
  self$.constructor = constructor
  self$.dot_args = dot_args
  self$.predefined_args = predefined_args
  self$regenerate = function() {
    do.call(self$.constructor, c(self$.dot_args, self$.predefined_args))
  }
  return_object(self, "Args")
}

#' Editable Arguments
#'
#' Class for representing arguments to a constructor, such that the arguments
#' can be edited through a set of methods.
#'
#' Each method modifies an argument or a component of an argument. The methods
#' are organized into lists of methods, such that each list (described below)
#' represents a different type of modification.
#'
#' * `edit` -- Either replace the value of a named list-valued argument
#' component by name, or add a new argument component with a particular name.
#' * `insert` -- Add argument components at a specific location within a
#' list-valued argument, by giving the index to that location. The components
#' appearing after the insertion are all shifted down to make room for the new
#' components.
#' * `remove_from` -- Remove argument components either by name (for
#' list-valued components) or by value (for character-valued components).
#' * `add_to` -- Add argument components to the end of a list-valued or
#' vector-valued argument.
#' * `replace` -- Provide a new set of components for an entire argument.
#'
#' @param edit,insert,remove_from,add_to,replace Named lists of functions
#' produced through `EditorBase` inheritance, such that each name becomes
#' the name of a method in a field with a list of methods.
#' @param insert
#' @param remove_from
#' @param add_to
#' @param replace
EditableArgs = function(constructor, dot_args, predefined_args
    , edit = list()
    , insert = list()
    , remove_from = list()
    , add_to = list()
    , replace = list()
  ) {
  self = Args(constructor, dot_args, predefined_args)

  self$edit = list()
  self$insert = list()
  self$remove_from = list()
  self$add_to = list()
  self$replace = list()
  for (ref in names(edit)) {
    self$edit[[ref]] = edit[[ref]](self, ref)
  }
  for (ref in names(insert)) {
    self$insert[[ref]] = insert[[ref]](self, ref)
  }
  for (ref in names(remove_from)) {
    self$remove_from[[ref]] = remove_from[[ref]](self, ref)
  }
  for (ref in names(add_to)) {
    self$add_to[[ref]] = add_to[[ref]](self, ref)
  }
  for (ref in names(replace)) {
    self$replace[[ref]] = replace[[ref]](self, ref)
  }

  return_object(self, "EditableArgs")
}

EditableArgs = macpan2:::EditableArgs
MakeEditList = macpan2:::MakeEditList
MakeRemoveChar = macpan2:::MakeRemoveChar
MakeAddChar = macpan2:::MakeAddChar
MakeReplaceChar = macpan2:::MakeReplaceChar

xx = EditableArgs(MatsList
  , list(x = 1:3)
  , list(.mats_to_return = "x",
         .mats_to_save = character(0L),
         .dimnames = list(x = list(letters, ""))
        )
  , edit = list(
    matrices = MakeEditList(".dot_args"),
    .dimnames = MakeEditList(".predefined_args")
  )
  , remove_from = list(
    .mats_to_return = MakeRemoveChar,
    .mats_to_save = MakeRemoveChar
  )
  , add_to = list(
    .mats_to_return = MakeAddChar,
    .mats_to_save = MakeAddChar
  )
  , replace = list(
    .mats_to_return = MakeReplaceChar,
    .mats_to_save = MakeReplaceChar
  )
)

xx$.predefined_args

yy = EditableArgs(ExprList
  , list()
  , list(
      before = list(x ~ 1 + 1),
      during = list(y ~ x / 2),
      after = list(),
      .simulate_exprs = character(0L)
  )
  , insert = list(
    before = MakeInsertList(".predefined_args"),
    during = MakeInsertList(".predefined_args"),
    after = MakeInsertList(".predefined_args")
  )
)

zz = yy$regenerate()
zz$.eval_schedule
yy$insert$during(z ~ 36, .at = 2L)$.expr_list
xx$replace$.mats_to_save("c", "y", "testing")$.mats_to_save
xx$add_to$.mats_to_save("x")$.mats_to_save
xx$add_to$.mats_to_save("x")$.mats_to_save
xx$regenerate()$.mats_to_return
xx$regenerate()$.initial_mats
xx$regenerate()$.dimnames
xx$edit$.dimnames(x = list(c("t", "h"), ""))$.dimnames
xx$edit$matrices(x = runif(10))$.initial_mats
xx$remove_from$.mats_to_return("x")$.mats_to_return



x = EditableArgs(MatsList
  , list(x = empty_matrix)
  , list(
    .mats_to_return = "x",
    .mats_to_save = character(0L),
    .dimnames = list(x = list(letters, ""))
  )
)
x$remove_from$dimnames("x")
x$.arg_modifiers
x$remove_from$mats_to_return("x")
x$replace$mats_to_return("x", "y")
x$.arg_modifiers

x$mats_to_return("x")
x$.arg_modifiers
x$remove_from_mats_to_return("x")
y = x$edit(g = 1:3, x = 1:4, .mats_to_save = "g")
y$.mats_return
y = x$remove_from_mats_to_return()
y$.initial_mats
y$.mats_save_hist
