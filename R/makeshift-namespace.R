prefix_internal_names = function(internal_names
    , external_names
    , internal_prefix = "...Internal..."
  ) {
  bad = startsWith(external_names, internal_prefix)
  if (any(bad)) {
    macpan2:::msg_colon(
        macpan2:::msg(
            "You are not allowed to use names for variables"
          , "that start with", internal_prefix
          , "but the following variables are used"
        )
      , macpan2:::msg_indent(external_names[bad])
    ) |> stop()
  }
  sprintf("%s%s", internal_prefix, internal_names) |> make.unique(sep = "_")
}

#prefix_internal_names("hello", c("...Internal...hello"))
