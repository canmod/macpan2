labelled_partitions_validity_message = paste(
  "Valid partitions have the following characteristics.",
  "   All partition vectors are character vectors.",
  "   All partition vectors have the same positive length.",
  "   All partition vectors have a unique name.",
  "   But no partition vectors have names for their elements.",
  "   No elements in partition vectors can be blank strings.",
  "   But they can be missing with NA.",
  sep = "\n"
)

LabelledPartitions = function(...) {
  valid_labelled_partitions = ValidityMessager(
    All(
      MappedAllTest(is.character),

      ## label vectors cannot have names for their elements
      TestPipeline(MappedSummarizer(names), MappedAllTest(is.null)),

      ## bound the range of the number of variables
      ## (question: should we allow one variable or not?)
      TestPipeline(MappedSummarizer(length), MappedAllTest(Not(TestRange(0L, 0L)))),

      ## bound the number of characters that is allowed in partitions
      TestPipeline(MappedSummarizer(nchar), MappedAllTest(Not(TestRange(0L, 0L)))),

      TestPipeline(MappedSummarizer(length), TestHomo()),
      TestPipeline(Summarizer(names, is.null), TestFalse()),
      TestPipeline(Summarizer(names, duplicated, any), TestFalse())
    ),
    "\nInvalid labelled partitions passed to ModelVars.",
    labelled_partitions_validity_message
  )
  self = Base()
  self$partitions = valid_labelled_partitions$assert(list(...))
  return_object(self, "LabelledPartitions")
}

#' Model Variables
#'
#' Construct an object for containing and operating on lists
#' of model variables. These lists are represented by a set
#' of `n` character vectors that are all the same length. The `i`th
#' element of the `j`th character vector provides one of `n` partitions
#' for identifying the `i`th variable.
#'
#' Often the character vectors relate to a specific sub-model (or
#' factor model in the terminology of product models). For example,
#' the following set of model variables is a product of two factor
#' models, `si` and `vax`:
#'
#' ```{r}
#' ModelVars(
#'   si = c("S", "I", "S", "I"),
#'   vax = c("n", "n", "y", "y")
#' )$data_frame()
#' ```
#'
#' The first two states are the `S` and `I` compartments for
#' unvaccinated individuals and the last two are for those who
#' are vaccinated. Constructing variable lists for product models
#' like this is easier with the \code{\link{VarsProduct}} function.
#'
#' ```{r}
#' VarsProduct(
#'   ModelVars(si = c("S", "I")),
#'   ModelVars(vax = c("n", "y"))
#' )$data_frame
#' ```
#'
#' @param ... Vectors giving partitions that describe a set of model
#' variables. The elements of each vector provides a label for
#' each variable. r labelled_partitions_validity_message.
#'
#' @return Object of class \code{ModelVars}.
#'
#' ## Methods
#'
#' * `$data_frame()`: Return a data frame representation of the model
#' variable partitions with one row for each variable and one column for
#' each label.
#' * `$filter(...)`: Return a new `ModelVars` object containing the
#' subset of variables expressed by the filter in `...` (note: the
#' format of the filtering arguments are not finalized)
#' * `$factor_to_product_map(factor_label, filter_partitions, vertex)`:
#' Return a named list of strings with names giving the partitions
#' for variables associated with the factor_label ...
#'
#' ## Method Arguments
#'
#' TODO maybe
#'
#' @export
ModelVars = function(...) {
  self = LabelledPartitions(...)
  self$n_variables = function() length(self$partitions[[1L]])
  self$n_partitions = function() length(self$partitions)
  self$filter = function(...) {
    l = list(...)
    filter_condition_matrix = vapply(names(l), function(partition_nm) {
        self$partitions[[partition_nm]] %in% l[[partition_nm]]
      }, logical(self$n_variables()))
    keepers = apply(filter_condition_matrix, 1L, all)
    filtered_variables = sapply(self$partitions, `[`, keepers, simplify = FALSE)
    do.call(ModelVars, filtered_variables)
  }
  self$factor_to_product_map = function(factor_partition, ...) {
    filter_partitions = names(list(...))
    filter_var_name = valid$char1$assert(ModelVars(...)$var_names())

    ## seems wasteful but self$split is memoised, so subsequent calls
    ## just look in the cache of memos.
    product_model_subset = self$split(filter_partitions)[[filter_var_name]]

    setNames(
      as.list(product_model_subset$var_names()),
      product_model_subset$partitions[[factor_partition]]
    )
  }
  self$data_frame = function() as.data.frame(self$partitions)
  self$var_names = function() Reduce(LabelMultiply()$direct, self$partitions)
  self$subset_var_names = function(partition_names) {
    Reduce(LabelMultiply()$direct, self$partitions[partition_names])
  }
  self$split = memoise::memoise(function(partitions_to_split_on) {
    lapply(
      split(
        self$data_frame(),
        self$subset_var_names(partitions_to_split_on)
      ),
      do.call,
      what = ModelVars
    )
  })
  valid_variable_names = ValidityMessager(
    TestPipeline(Summarizer(duplicated, any), TestFalse()),
    "\nInvalid variable names.",
    "Please supply partitions that uniquely identify each variable."
  )
  valid_variable_names$check(self$var_names())
  return_object(self, "ModelVars")
}
dim.ModelVars = function(x) {
  c(x$n_variables(), x$n_partitions())
}

#' @export
VarsFactor = function(model_vars) {
  ## alternative constructor
  factor_partition = LabelMultiply()$prod(names(model_vars$partitions))
  do.call(ModelVars, setNames(list(model_vars$var_names()), factor_partition))
}

#' @export
VarsProduct = function(model_vars_1, model_vars_2) {
  ## alternative constructor
  l1 = model_vars_1$partitions
  l2 = model_vars_2$partitions
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
      , edge_partition
      , vertex_partition
      , product_model_vars
  ) {
  self = Base()
  self$edge = edge
  self$vertices = valid$char$assert(vertices)
  self$edge_partition = valid$char1$assert(edge_partition)
  self$vertex_partition = valid$char1$assert(vertex_partition)
  self$product_model_vars = product_model_vars
  self$rate_per_vertex = function(vertex) {

    map_arguments = c(
      list(factor_partitions = self$edge_partition),
      setNames(self$vertex_partition, vertex)
    )

    ## compute the map that takes factor model variables into
    ## the product model variables (i.e. a filter function)
    product_to_factor_map = do.call(
      self$product_model_vars$factor_to_product_map,
      map_arguments
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
EdgeMatcher = function(from, to, partitions_to_match) {
  self = Base()
  df_from = from$data_frame()
  df_to = to$data_frame()
  from_nms = names(df_from)[!names(df_from) %in% partitions_to_match]
  to_nms = names(df_from)[!names(df_to) %in% partitions_to_match]
  n = length(partitions_to_match)
  common_indices = seq_along(partitions_to_match)
  from_indices = c(common_indices, seq_len(ncol(df_from) - n) + n)
  to_indices = c(common_indices, seq_len(ncol(df_to) - n) + ncol(from))
  df_edge = merge(
    df_from, df_to,
    by = partitions_to_match
  )
  self$from = do.call(
    ModelVars,
    setNames(df_edge[, from_indices, drop = FALSE], c(partitions_to_match, from_nms))[names(df_from)]
  )
  self$to = do.call(
    ModelVars,
    setNames(df_edge[, to_indices, drop = FALSE], c(partitions_to_match, to_nms))[names(df_to)]
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
foi$rate$symbolic_function("well", "that", "is", "go")
foi_vax = EdgeProduct(
  edge = foi,
  vertices = c("unvax", "vax"),
  edge_partition = "sir",
  vertex_partition = "vax",
  product_model_vars = sir_vax_vars
)
foi_vax$rate_per_vertex("vax")
foi_vax$rate_per_vertex("unvax")
foi_vax$rates()
foi_vax$rate()


sir_vax_vars$factor_to_product_map(factor_partition = "sir", filter_partitions = "vax", vertex = "unvax")


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
