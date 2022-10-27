
#' @export
ModelVars = function(...) {
  self = Base()
  valid_model_labels = ValidityMessager(
    All(
      MappedAllTest(is.character),

      ## label vectors cannot have names for their elements
      TestPipeline(MappedSummarizer(names), MappedAllTest(is.null)),

      ## bound the range of the number of variables
      ## (question: should we allow one variable or not?)
      TestPipeline(MappedSummarizer(length), MappedAllTest(Not(TestRange(0L, 0L)))),

      ## bound the number of characters that is allowed in labels
      TestPipeline(MappedSummarizer(nchar), MappedAllTest(Not(TestRange(0L, 0L)))),

      TestPipeline(MappedSummarizer(length), TestHomo()),
      TestPipeline(Summarizer(names, is.null), TestFalse()),
      TestPipeline(Summarizer(names, duplicated, any), TestFalse())
    ),
    "\nInvalid model labels passed to ModelVars.",
    "Valid labels have the following characteristics:",
    "  - all label vectors are character vectors",
    "  - all label vectors have the same positive length",
    "  - all label vectors have a unique name",
    "  - but no label vectors have names for their elements",
    "  - no elements in label vectors can be blank strings",
    "  - but they can be missing with NA"
  )
  valid_variable_names = ValidityMessager(
    TestPipeline(Summarizer(duplicated, any), TestFalse()),
    "\nInvalid variable names.",
    "Please supply labels that uniquely identify each variable."
  )
  self$labels = valid_model_labels$assert(list(...))
  self$n_variables = function() length(self$labels[[1L]])
  self$n_labels = function() length(self$labels)
  self$filter = function(...) {
    l = list(...)
    filter_condition_matrix = vapply(names(l), function(label_nm) {
        self$labels[[label_nm]] %in% l[[label_nm]]
      }, logical(self$n_variables()))
    keepers = apply(filter_condition_matrix, 1L, all)
    filtered_variables = sapply(self$labels, `[`, keepers, simplify = FALSE)
    do.call(ModelVars, filtered_variables)
  }
  self$filter_as_product = function(edge_factor, vertex_factor, vertex) {
    product_model_subset = self$split(vertex_factor)[[vertex]]
    setNames(
      as.list(product_model_subset$var_names()),
      product_model_subset$labels[[edge_factor]]
    )
  }
  self$data_frame = function() as.data.frame(self$labels)
  self$var_names = function() Reduce(LabelMultiply()$direct, self$labels)
  self$subset_var_names = function(label_names) {
    Reduce(LabelMultiply()$direct, self$labels[label_names])
  }
  self$split = memoise::memoise(function(labels_to_split_on) {
    lapply(
      split(
        self$data_frame(),
        self$subset_var_names(labels_to_split_on)
      ),
      do.call,
      what = ModelVars
    )
  })
  valid_variable_names$check(self$var_names())
  return_object(self, "ModelVars")
}
dim.ModelVars = function(x) {
  c(x$n_variables(), x$n_labels())
}

#' @export
VarsFactor = function(model_vars) {
  ## alternative constructor
  factor_label = LabelMultiply()$prod(names(model_vars$labels))
  do.call(ModelVars, setNames(list(model_vars$var_names()), factor_label))
}

#' @export
VarsProduct = function(model_vars_1, model_vars_2) {
  ## alternative constructor
  l1 = model_vars_1$labels
  l2 = model_vars_2$labels
  n1 = model_vars_1$n_variables()
  n2 = model_vars_2$n_variables()
  do.call(ModelVars, c(
    lapply(l1, rep, times = n2),
    lapply(l2, rep, each = n1)
  ))
}

#' @export
EdgeFactor = function(from, to, rate) {
  self = Base()
  self$from = valid$char1$assert(from)
  self$to = valid$char1$assert(to)
  self$rate = MathExpressionFromFunc(rate)
  self$data_frame = function() {
    data.frame(
      from = self$from,
      to = self$to,
      rate = self$rate$string
    )
  }
  return_object(self, "EdgeFactor")
}

#' @export
EdgeProduct = function(edge
      , vertices
      , edge_label
      , vertex_label
      , product_model_vars
  ) {
  self = Base()
  self$edge = edge
  self$vertices = valid$char$assert(vertices)
  self$edge_label = valid$char1$assert(edge_label)
  self$vertex_label = valid$char1$assert(vertex_label)
  self$product_model_vars = product_model_vars
  self$rate_per_vertex = function(vertex) {

    ## compute the map that takes factor model variables into
    ## the product model variables (i.e. a filter function)
    product_to_factor_map = self$product_model_vars$filter_as_product(
      self$edge_label,
      self$vertex_label,
      vertex
    )

    ## apply the map that takes factor model variables into
    ## the specific product model variables that are required
    ## as arguments to the rate function
    rate_arguments = product_to_factor_map[self$edge$rate$arguments]

    ## symbolically evaluate the rate function using the
    ## product model variables
    do.call(self$edge$rate$symbolic_function, rate_arguments)
  }
  self$rates = function() {
    vapply(self$vertices, self$rate_per_vertex, character(1L))
  }
  self$rate = function() {
    sum = LabelMultiply(add = " + ")$sum
    wrap = LabelWrapper(open = "(", close = ")")$wrap
    wrap(sum(self$rates()))
  }
  return_object(self, "EdgeProduct")
}

#' @export
EdgeMatcher = function(from, to, labels_to_match) {
  self = Base()
  df_from = from$data_frame()
  df_to = to$data_frame()
  from_nms = names(df_from)[!names(df_from) %in% labels_to_match]
  to_nms = names(df_from)[!names(df_to) %in% labels_to_match]
  n = length(labels_to_match)
  common_indices = seq_along(labels_to_match)
  from_indices = c(common_indices, seq_len(ncol(df_from) - n) + n)
  to_indices = c(common_indices, seq_len(ncol(df_to) - n) + ncol(from))
  df_edge = merge(
    df_from, df_to,
    by = labels_to_match
  )
  self$from = do.call(
    ModelVars,
    setNames(df_edge[, from_indices, drop = FALSE], c(labels_to_match, from_nms))[names(df_from)]
  )
  self$to = do.call(
    ModelVars,
    setNames(df_edge[, to_indices, drop = FALSE], c(labels_to_match, to_nms))[names(df_to)]
  )
  return_object(self, "ModelEdge")
}

if (FALSE) {
sir_vars = ModelVars(sir = c("S", "I", "R", "beta", "gamma"))
vax_vars = ModelVars(vax = c("vax", "unvax", "vax_rate", "wane_rate"))
sir_vax_vars = VarsProduct(sir_vars, vax_vars)
sir_vax_factor_vars = VarsFactor(sir_vax_vars)
sir_vars$data_frame()
vax_vars$data_frame()
sir_vax_vars$data_frame()
sir_vax_factor_vars$data_frame()
foi = EdgeFactor(
  from = "S",
  to = "I",
  rate = function(S, I, R, beta) beta * I / (S + I + R)
)
foi_vax = EdgeProduct(
  edge = foi,
  vertices = c("unvax", "vax"),
  edge_label = "sir",
  vertex_label = "vax",
  product_model_vars = sir_vax_vars
)
foi_vax$rate_per_vertex("vax")
foi_vax$rate_per_vertex("unvax")
foi_vax$rates()
foi_vax$rate()


sir_vax_vars$filter_as_product(edge_factor = "sir", vertex_factor = "vax", vertex = "unvax")


}
#
#
# epi = ModelVars(epi = c("S", "I", "I", "R", "beta", "gamma"), symp = c(NA, "m", "s", NA, NA, NA))
#
# epi_vax_vars = label_product(epi, vax)
# epi_vax_state = epi_vax$filter(epi = c("S", "I", "R"), vax = c("unvax", "vax"))
#
# xx = ModelEdges(
#   epi_vax_state$filter(epi = "I"),
#   epi_vax_state$filter(epi = "R"),
#   "vax"
# )

#flow = MathExpressionFromFunc(function(from, rates) from * sum(rates))

# EdgeModel = function(from, to, rate) {
#   self = Base()
#   self$from = valid$char1$assert(from)
#   self$to = valid$char1$assert(to)
#   self$rate = valid$math$assert(rate)
#   self$flow = MathExpressionFromStrings(
#     unique(c(from, rate$arguments)),
#     flow$symbolic_function(self$from, rate$string)
#   )
#   return_object(self, "EdgeModel")
# }
# EdgeModel("S", "I", MathExpressionFromFunc(
#   function(S, I, R, beta) I * beta / (S + I + R)
# ))
#flow = math$from_function(function(from, rate) from * rate)
# infection = EdgeModel("S", "I", math$from_function(
#   function(S, I, R, beta) I * beta / (S + I + R)
# ))

# ProductEdgeByVertices = function(edge, vertices) {
#   self = Base()
#   self$edge = edge
#   self$vertices = valid$char$assert(vertices)
#   self$edge = valid$edge$assert(edge)
#   return_object(self, "ProductEdgeByVertices")
# }

# xx = ProductEdgeByVertices(infection, c("unvax", "vax"))
# label_mult = LabelMultiply("_")
# label_mult$outer(label_mult$outer(label_mult$outer(c("a", "b"), xx$vertices), c("A", "B", "C")), c("Z", "GE", "GH"))

# AbstractModel = function(
#     vertices,
#     parameters,
#     edges
#   ) {
#   self = Base()
#   self$vertices = vertices
#   self$edges = edges
#   self$parameters = parameters
#   self$required_variables = function(edge) {
#     flow_function = self$abstract_model$edges[[edge]]$flow
#     flow_function_arguments = formals(flow_function)
#     names(flow_function_arguments)
#   }
#   self$from = function() {
#     vapply(self$edges, getElement, character(1L), "from")
#   }
#   self$to = function() {
#     vapply(self$edges, getElement, character(1L), "to")
#   }
#   self$flow = function() {
#     lapply(self$edges, getElement, "flow")
#   }
#   self$edge_table = function() {
#     data.frame(
#       from = self$from(),
#       to = self$to(),
#       flow = self$flow()
#     )
#   }
#   return_object(self, "AbstractModel")
# }
#
# InstantiatedModel = function(
#     vertices,
#     parameters,
#     abstract_model
# ) {
#   self = Base()
#   self$abstract_model = abstract_model
#   self$vertices = vertices
#   self$parameters = parameters
#   self$compute_flow = function(edge) {
#     variables = c(self$vertices, self$parameters)
#     required_variables = variables[self$required_variables(edge)]
#     flow_function_inputs = as.list(required_variables)
#     flow_function = self$abstract_model$edges[[edge]]$flow
#     do.call(flow_function, flow_function_inputs)
#   }
#   return_object(self, "NumericalModel")
# }
#
#
#
#
# sir = AbstractModel(
#   vertices = c("S", "I", "R"),
#   parameters = c("beta", "gamma"),
#   edges = list(
#     infection = list(
#       from = "S", to = "I",
#       flow = function(S, I, R, beta) {beta * S * I / (S + I + R)}
#     ),
#     recovery = list(
#       from = "I", to = "R",
#       flow = function(I, gamma) {I * gamma}
#     )
#   )
# )
#
# vax = AbstractModel(
#   vertices = c("unvax", "vax"),
#   parameters = c("vax_rate", "wane_rate"),
#   edges = list(
#     vaccination = list(
#       from = "unvax", to = "vax",
#       flow = function(unvax, vax_rate) {unvax * vax_rate}
#     ),
#     wanning = list(
#       from = "vax", to = "unvax",
#       flow = function(vax, wane_rate) {vax * wane_rate}
#     )
#   )
# )

# sir$edge_table()
#
# vapply(sir$edges, getElement, "character", "from")
# vapply(sir$edges, getElement, "character", "to")
# vapply(vax$edges, getElement, "character", "from")
# vapply(vax$edges, getElement, "character", "to")

# ## Naive
# list(
#   from = "S_unvax", to = "I_unvax",
#   flow = function(S_unvax, I_unvax, R_unvax, beta) {
#     beta * S_unvax * I_unvax / (S_unvax + I_unvax + R_unvax)
#   }
# )
#
# ## Modified
# list(
#   from = "S_unvax", to = "I_unvax",
#   flow = function(S_unvax, I_unvax, R_unvax, beta_unvax, beta_vax) {
#     beta_unvax * S_unvax * I_unvax / (S_unvax + I_unvax + R_unvax) +
#       beta_vax * S_unvax * I_vax / (S_vax + I_vax + R_vax)
#   }
# )
#
# sir$edges$infection$flow(S_unvax, I_unvax, R_unvax, beta_unvax) +
#   sir$edges$infection$flow(S_unvax, I_vax, R_vax, beta_vax)
#
#
#
# list(
#   sir = c("S", "I", "R"),
#   vax = c("unvax", "vax")
# )
#
#
#
# VertexProduct = function() {
#   self = Base()
#   self$evaluate = function(model_1, model_2) {
#     v1 = model_1$vertices
#     v2 = model_2$vertices
#     paste(
#       rep(v1, length(v2)),
#       rep(v2, each = length(v1)),
#       sep = "_"
#     )
#   }
#   return_object(self, "VertexProduct")
# }
# pp = VertexProduct()
# pp$evaluate(sir, vax)
#
# ee = EdgeProduct()
# ee$evaluate(sir, vax)
#
# EdgeProduct = function() {
#   self = Base()
#   self$evaluate = function(model_1, model_2) {
#     v1 = model_1$vertices
#     v2 = model_2$vertices
#     e1 = model_1$edges
#     e2 = model_2$edges
#     e12 = list()
#     e21 = list()
#     paster = function(x, y) {
#       paste(y, x, sep = "_")
#     }
#
#     ## construct the edges for mechanisms in model_1
#     for (i in seq_along(e1)) {
#       e12[[i]] = list()
#       e12[[i]]$from = lapply(v2, paster, e1[[i]]$from)
#       e12[[i]]$to = lapply(v2, paster, e1[[i]]$to)
#     }
#
#     for (i in seq_along(e2)) {
#       e21[[i]] = list()
#       e21[[i]]$from = paste(e2[[i]]$from, v1, sep = "_")
#       e21[[i]]$to = paste(e2[[i]]$to, v1, sep = "_")
#     }
#
#     c(e12, e21)
#     ## construct the edges for mechanisms in model_2
#   }
#   return_object(self, "EdgeProduct")
# }
#
#
#
# sir_instantiated = InstantiatedModel(
#   vertices = c(S = 0.9, I = 0.1, R = 0),
#   parameters = c(beta = 0.2, gamma = 0.15),
#   abstract_model = sir
# )
#
# sir_instantiated$required_variables("infection")
# sir_instantiated$required_variables("recovery")
# sir_instantiated$compute_flow("infection")
# sir_instantiated$compute_flow("recovery")
