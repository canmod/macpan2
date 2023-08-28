library(macpan2)
library(oor)
# source("../../../R/simulation_methods.R")
# source("../../../R/alt_model_constructors.R")
# source("../../../R/model_products.R")

epi_dir = "Epi_model"
age_dir = "Age_model"


#Initial_state, rk1, rk2, rk3, and rk4 must all be initialized with empty_matrix for the RK4 method to work 
epi_simulator = SimulatorConstructor(epi_dir,
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


#Initial_state, rk1, rk2, rk3, and rk4 must all be initialized with empty_matrix for the RK4 method to work 
age_simulator = SimulatorConstructor(age_dir,
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

