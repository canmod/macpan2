library(macpan2)
library(oor)
source("../../../R/simulation_methods.R")
source("../../../R/alt_model_constructors.R")
source("../../../R/model_products.R")

epi_dir = "Epi_model"
age_dir = "Age_model"

epi_files = ModelFiles("Epi_model")
age_files = ModelFiles("Age_model")

epi_model = CompartmentalAlt(epi_dir)
age_model = CompartmentalAlt(age_dir)


epi_model$expr_list()$print_exprs()
age_model$expr_list()$print_exprs()

#Initial_state, rk1, rk2, rk3, and rk4 must all be initialized with empty_matrix for the RK4 method to work 
epi_simulator = SimulatorConstructor(epi_model,
                                     time_steps = 25L,
                                     state = c(S = 999, E = 1, I = 0, R = 0),
                                     flow = c(total_foi = NA, progression = 0.1, recovery = 0.05),
                                     N = empty_matrix,
                                     transmissability = 0.75,
                                     per_capita_transmission = empty_matrix,
                                     initial_state = empty_matrix,
                                     rk1 = empty_matrix,
                                     rk2 = empty_matrix,
                                     rk3 = empty_matrix,
                                     rk4 = empty_matrix,
                                     .mats_to_return = c("state")
                                     )
epi_simulator$print$expressions()
epi_simulator$report()

# The commented out code below is not comparable to the output of epi_simulator$report() since it is missing 
# the expression that updates total_foi. This Expression is included automatically when SimulatorConstructor is 
# called but not when Compartmental is called.
# epi_model_2 = Compartmental(epi_dir)
# epi_simulator_2 = epi_model_2$simulators$tmb(
#                                             time_steps = 25L,
#                                             state = c(S = 999, E = 1, I = 0, R = 0),
#                                             flow = c(total_foi = NA, progression = 0.1, recovery = 0.05),
#                                             N = empty_matrix,
#                                             transmissability = 0.75,
#                                             per_capita_transmission = empty_matrix,
#                                             .mats_to_return = c("state")
#                                             )
# epi_simulator_2$print$expressions()
# epi_simulator_2$report()

#Initial_state, rk1, rk2, rk3, and rk4 must all be initialized with empty_matrix for the RK4 method to work 
age_simulator = SimulatorConstructor(age_model,
                                     time_steps = 25L,
                                     state = c(young = 333, medium = 333, old = 333),
                                     flow = c(ageing_rate = 0.03, birth_rate = 5, death_rate = 0.01),
                                     per_capita_transmission = empty_matrix,
                                     initial_state = empty_matrix,
                                     rk1 = empty_matrix,
                                     rk2 = empty_matrix,
                                     rk3 = empty_matrix,
                                     rk4 = empty_matrix,
                                     .mats_to_return = c("state")
                                     )

age_simulator$print$expressions()
age_simulator$report()


age_model_2 = Compartmental(age_dir)
age_simulator_2 = age_model_2$simulators$tmb(
                                            time_steps = 25L,
                                            state = c(young = 333, medium = 333, old = 333),
                                            flow = c(ageing_rate = 0.03, birth_rate = 5, death_rate = 0.01),
                                            per_capita_transmission = empty_matrix,
                                            .mats_to_return = c("state")
                                            )
age_simulator_2$print$expressions()
age_simulator_2$report()



var_prd = VariablesProduct(epi_model, age_model)
var_prd$frame()

flw_prd = FlowsProduct(epi_model, age_model)
flw_prd

stg_prd = SettingsProduct(epi_model, age_model)
stg_prd
