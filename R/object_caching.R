
ObjectCache = function(generator) {
  self = Base()
  self$generator = generator
  self$wrap = function(method, cache) {
    wrapped_fn = function() {
      field = sprintf(".%s", method)
      if (is.null(cache[[field]])) {
        cache[[field]] = cache$generator[[method]]()
      }
      return(cache[[field]])
    }
    e = environment(wrapped_fn)
    e$method = method
    e$cache = cache
    return(wrapped_fn)
  }
  self$method_names = (generator
    |> eapply(is.function) 
    |> Filter(f = isTRUE) 
    |> names()
  )
  for (method in self$method_names) {
    self[[method]] = self$wrap(method, self)
  }
  self$show_code = function(method) {
    (self$generator[[method]]
      |> body()
      |> getElement(2L) ## remove the `{` from the function body
      |> deparse()
      |> cat(sep = "\n", fill = FALSE)
    )
  }
  return_object(self, "ObjectCache")
}

SIExample = function() {
  self = Base()
  
  self$specification = function() {
    mp_tmb_library("starter_models"
      , "sir"
      , package = "macpan2"
    )
  }
  self$simulator = function(specification = self$specification()) {
    mp_simulator(specification, 50, "infection")
  }
  self$data = function(simulator = self$simulator()) {
    mp_trajectory(simulator)
  }
  self$calibrator = function(
        specification = self$specification()
      , data = self$data()
    ) {
    mp_tmb_calibrator(specification
      , data
      , traj = "infection"
      , par = c("beta", "gamma")
      , default = list(beta = 0.25, gamma = 0.25)
    )
  }
  self$optimized_calibrator = function(
        specification = self$specification()
      , data = self$data()
    ) {
    mp_tmb_calibrator(specification
      , data
      , traj = "infection"
      , par = c("beta", "gamma")
      , default = list(beta = 0.25, gamma = 0.25)
      , optimize = TRUE
    )
  }
  return_object(self, "SIExample")
}

si_example = ObjectCache(SIExample())

#' SI Example
#' 
#' The \code{si_example_object} function efficiently generates objects
#' associated with the 
#' [example SI model](https://github.com/canmod/macpan2/blob/main/inst/starter_models/si/README.md). 
#' It either creates these objects or extracts them from a pre-computed cache if 
#' they have already been created in the current session. This function is 
#' predominantly used to simplify examples in the package documentation -- it 
#' allows package developers to efficiently get these basic example objects to 
#' illustrate many different concepts, without having to spend time computing 
#' them over and over again when generating the documentation. The 
#' \code{si_example_code} function displays code 
#' that could be used to generate these objects, and clarifies what these 
#' functions do.
#' 
#' @param object Type of object associated with the example SI model. Can be
#' one of the following: 
#' `"specification"`, `"simulator"`, `"data"`, `"calibrator"`, 
#' `"optimized_calibrator"`.
#' The functions used to produce each of these objects, respectively, are
#' \code{\link{mp_tmb_library}}, 
#' \code{\link{mp_simulator}}, 
#' \code{\link{mp_trajectory}},
#' \code{\link{mp_tmb_calibrator}}, and \code{\link{mp_tmb_calibrator}}.
#' 
#' @examples
#' 
#' # Get SI calibrator in two different states
#' cal1 = si_example_object("calibrator")
#' cal2 = si_example_object("optimized_calibrator")
#' 
#' # Optimize cal1 so that it is equal to cal2
#' mp_optimize(cal1)
#' mp_tmb_coef(cal1)
#' mp_tmb_coef(cal2)
#' 
#' # Inspect the code used to produce all objects in this example SI model.
#' si_example_code("specification")
#' si_example_code("simulator")
#' si_example_code("data")
#' si_example_code("calibrator")
#' si_example_code("optimized_calibrator")
#' 
#' @name si_example
NULL

#' @describeIn si_example Return an object associated with the example
#' SI model.
#' @export
si_example_object = function(object = c(
      "specification"
    , "simulator"
    , "data"
    , "calibrator"
    , "optimized_calibrator"
)) {
  object = match.arg(object)
  si_example[[object]]()
}

#' @describeIn si_example Show code that could be used to generate an object
#' associated with the example SI model.
#' @export
si_example_code = function(object = c(
      "specification"
    , "simulator"
    , "data"
    , "calibrator"
    , "optimized_calibrator"
)) {
  object = match.arg(object)
  si_example$show_code(object)
  invisible(NULL)
}
