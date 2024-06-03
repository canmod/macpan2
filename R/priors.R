


# 
# 
# Distribution = function(...) {
#   self = Base()
#   self$parameters = list(...)
#   self$density_prototype = ~ 0
#   self$simulation_prototype = ~ 0
#   self$density = function(variable) {
#     update_formula(self$density_prototype
#       , c(
#           two_sided("variable", variable)
#         , self$parameters  ## whatever this is??
#       )
#     )
#   }
#   self$simulation = function(variable) self$simulation_prototype 
#   return_object(self, "Distribution")
# }
# 
# NormalPrior = function(variable, ) {
#   self = Distribution(variables, hyperparameters)
#   self$density_prototype = ~ dnorm(obs, mean, sd)
#   self$simulation_prototype = ~ rnorm(mean, sd)
#   return_object(self, "NormalPrior")
# }
# Normal("log_beta", sd = testing + 1)$x()
# 
# 
# 
# Normal = function(variable, hyperparameters = list()) {
#   self = Density(variable, hyperparameters)
# }
