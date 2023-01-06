library(McMasterPandemic)
library(macpan2)
library(TMB)

compile('dev.cpp')
dyn.load(dynlib("dev"))

ExprList(

)

%>% add_rate("E", "Ia", ~ (alpha) * (sigma))
    %>% add_rate("E", "Ip", ~ (1 - alpha) * (sigma))
    %>% add_rate("Ia", "R", ~ (gamma_a))
    %>% add_rate("Ip", "Im", ~ (mu) * (gamma_p))
    %>% add_rate("Ip", "Is", ~ (1 - mu) * (gamma_p))
    %>% add_rate("Im", "R", ~ (gamma_m))
    %>% add_rate("Is", "H", ~
                   (1 - nonhosp_mort) * (phi1) * (gamma_s))
    %>% add_rate("Is", "ICUs", ~
                   (1 - nonhosp_mort) * (1 - phi1) * (1 - phi2) * (gamma_s))
    %>% add_rate("Is", "ICUd", ~
                   (1 - nonhosp_mort) * (1 - phi1) * (phi2) * (gamma_s))
    %>% add_rate("Is", "D", ~ (nonhosp_mort) * (gamma_s))
    %>% add_rate("ICUs", "H2", ~ (psi1))
    %>% add_rate("ICUd", "D", ~ (psi2))
    %>% add_rate("H2", "R", ~ (psi3))
    %>% add_rate("H", "R", ~ (rho))
    %>% add_rate("Is", "X", ~ (1 - nonhosp_mort) * (phi1) * (gamma_s))
