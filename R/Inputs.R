
MakeModifierEditor = function(parent, modifier_name) {
  self = Base()
  self$parent = parent
  self$modifier_name = modifier_name
  self$.dots = function(...) {
    as.character(unlist(list(...), use.names = FALSE))
  }
  return_object(self, "MakeModifierEditor")
}


MakeRemove = function(parent, modifier_name) {
  self = MakeModifierEditor(parent, modifier_name)
  function(...) {
    p = self$parent
    p$.arg_modifiers[[self$modifier_name]] = base::setdiff(
      p$.arg_modifiers[[self$modifier_name]],
      self$.dots(...)
    )
    self$parent$regenerate()
  }
}

MakeAdd = function(parent, modifier_name) {
  self = MakeModifierEditor(parent, modifier_name)
  function(...) {
    p = self$parent
    p$.arg_modifiers[[self$modifier_name]] = unique(c(
      p$.arg_modifiers[[self$modifier_name]],
      self$.dots(...)
    ))
    self$parent$regenerate()
  }
}


MakeReplace = function(parent, modifier_name) {
  self = MakeModifierEditor(parent, modifier_name)
  function(...) {
    p = self$parent
    p$.arg_modifiers[[self$modifier_name]] = self$.dots(...)
    self$parent$regenerate()
  }
}

Args = function(constructor, arg_list, arg_modifiers) {
  self = Base()
  self$.constructor = constructor
  self$.arg_list = arg_list
  self$.arg_modifiers = arg_modifiers
  self$regenerate = function() {
    do.call(self$.constructor, c(self$.arg_list, self$.arg_modifiers))
  }
  return_object(self, "Args")
}

EditableArgs = function(constructor, arg_list, arg_modifiers) {
  self = Args(constructor, arg_list, arg_modifiers)
  self$edit = function(...) {
    new_args = valid$named_list$assert(list(...))
    new_list_args = new_args[!names(new_args) %in% names(self$.arg_modifiers)]
    new_modifier_args = new_args[names(new_args) %in% names(self$.arg_modifiers)]
    for (nm in names(new_list_args)) {
      self$.arg_list[[nm]] = new_list_args[[nm]]
    }
    for (nm in names(new_modifier_args)) {
      self$.arg_modifiers[[nm]] = unique(c(
        self$.arg_modifiers[[nm]],
        new_modifier_args[[nm]]
      ))
    }
  }
  self$remove_from = list()
  self$add_to = list()
  self$replace = list()
  for (nm in names(self$.arg_modifiers)) {
    mod_nm = substring(nm, 2)
    self$remove_from[[mod_nm]] = MakeRemove(self, nm)
    self$add_to[[mod_nm]] = MakeAdd(self, nm)
    self$replace[[mod_nm]] = MakeReplace(self, nm)
  }
  return_object(self, "EditableArgs")
}
