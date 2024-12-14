library(oor)

TimeVarBaseline = function() {
  self = Base()
  self$not_for_first_window = function() FALSE
  self$calc = function(baseline, eta_last, link, link_last, par_nm) sprintf("%s ~ 0", baseline)
  return_object(self, "BaselineFunction")
}
TimeVarBaselineLast = function() {
  self = TimeVarBaseline()
  self$not_for_first_window = function() TRUE
  self$calc = function(baseline, eta_last, link, link_last, par_nm) {
    sprintf("%s ~ last(%s)", baseline, eta_last)
  }
  return_object(self, "TimeVarBaselineLast")
}
TimeVarBaselineParameter = function() {
  self = TimeVarBaseline()
  self$calc = function(baseline, eta_last, link, link_last, par_nm) {
    sprintf("%s ~ %s", baseline, link$ref(par_nm))
  }
  return_object(self, "TimeVarBaselineParameter")
}
TimeVarBaselineModelVar = function(model_var, link = mp_identity) {
  self = TimeVarBaseline()
  self$model_var = model_var
  self$link = link ## make NULL to use link function for the window
  self$calc = function(baseline, eta_last, link, link_last, par_nm) {
    if (!is.null(self$link)) link = self$link
    sprintf("%s ~ %s", baseline, link$ref(self$model_var))
  }
  return_object(self, "TimeVarBaselineModelVar")
}
TimeVarBaselineNumeric = function(value) {
  self = TimeVarBaseline()
  self$value = value
  self$calc = function(baseline, eta_last, link, link_last, par_nm) {
    sprintf("%s ~ %s", baseline, link$ref(self$value))
  }
  return_object(self, "TimeVarBaselineNumeric")
}

LinearTimeVar = function(variable_name, matrix_list, time_index_list) {
  
  variable_name = c(as.character(variable_name), "")[1L]
  
  self = Base()
  self$variable_name = variable_name
  self$matrix_list = matrix_list
  self$time_index_list = time_index_list
  
  ## time_index_list elements must all be increasing
  ## integer vectors without any duplicates.
  
  self$nr = function() vapply(self$matrix_list, nrow, integer(1L))
  self$nc = function() vapply(self$matrix_list, ncol, integer(1L))
  self$ln = function() vapply(self$time_index_list, length, integer(1L))
  self$min_time = function() vapply(self$time_index_list, min, integer(1L))
  self$max_time = function() vapply(self$time_index_list, max, integer(1L))
  
  self$check = function() {
    if (!identical(self$nr(), self$ln())) {
      stop("Matrix list and time-index list are not the compatible.")
    }
    NULL
  }
  self$matrix = function() {
    nr = self$nr()
    nc = self$nc()
    min_time = self$min_time()
    max_time = self$max_time()
    time_index = min(min_time):max(max_time)
    M = list()
    for (i in seq_along(self$matrix_list)) {
      M[[i]] = matrix(0, length(time_index), nc[i])
      ii = match(self$time_index_list[[i]], time_index)
      M[[i]][ii, ] = self$matrix_list[[i]]
    }
    M = Reduce(cbind, M)
    return(M)
  }
  return_object(self, "TimeVar")
}

mats = list(rbf(10, 4), rbf(20, 7), rbf(4, 2))
inds = list(
    sort(sample(30, 10, replace = FALSE))
  , sort(sample(30, 20, replace = FALSE))
  , sort(sample(30, 4 , replace = FALSE))
)
xx = LinearTimeVar("beta", mats, inds)
xx$matrix()

TimeVar = function(par_orig_nm) {
  self = Base()
  self$par_orig_nm = par_orig_nm
  
  self$update_
  
  self$par_input_nm = sprintf()
  self$par_output_nm = character()
  
  
  
  self$before = function() list()
  self$during = function() list()
  
  self$integer = function() list()
  self$default = function() list()
  
  return_object(self, "TimeVar")
}

TimeConst = function(var_nm) {
  self = TimeVar(var_nm)
  return_object(self, "TimeConst")
}

TimePiecewise = function(var_nm, change_values, change_points) {
  self = TimeVar(var_nm)
  self$change_values = change_values
  self$change_points = change_points
  self$par_input_nm = function() sprintf("time_var_%s", self$par_orig_nm())
  self$during = function() list()
}

