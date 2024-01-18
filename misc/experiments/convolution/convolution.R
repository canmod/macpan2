library(macpan2)
library(dplyr)
library(tidyr)
library(ggplot2)

# from mcmasterpandemic -- TODO: add to macpan2
make_delay_kernel <- function(prop, delay_mean, delay_cv, max_len = ceiling(tail_val), tail_crit = 0.95) {
    gamma_shape <- 1 / delay_cv^2
    gamma_scale <- delay_mean / gamma_shape
    tail_val <- qgamma(tail_crit, shape = gamma_shape, scale = gamma_scale)
    if (max_len < tail_val) {
        warning(sprintf(
            "max_len (%d) is less than qgamma(%f, %1.1f, %1.1f)=%1.1f",
            max_len, tail_crit, gamma_shape, gamma_scale, tail_val
        ))
    }
    pp <- diff(pgamma(seq(max_len + 1), shape = gamma_shape, scale = gamma_scale))
    pp <- pp / sum(pp) ## normalize to 1
    v <- prop * pp
    return(v)
}

sir = Compartmental(system.file("starter_models", "sir", package = "macpan2"))$simulators$tmb(
    time_steps = 100
  , state = c(S = 9999, I = 1, R = 0)
  , flow = c(foi = NA_real_, gamma = 0.2)
  , beta = 0.4
  , N = empty_matrix
  , kernel = make_delay_kernel(0.1, 11, 0.25)
  , kernel_length = length(make_delay_kernel(0.1, 11, 0.25))
  , reports = empty_matrix
  , incidence = empty_matrix
  , .mats_to_save = c("state", "incidence", "reports")
  , .mats_to_return = c("state", "incidence", "reports")
)
sir$insert$expressions(
    incidence ~ foi * S
  , reports ~ convolution(incidence, kernel)
  , .at = Inf
  , .phase = "during"
)

(sir$report(.phases = "during")
  %>% filter(matrix %in% c("reports", "incidence"))
  %>% ggplot()
  + geom_line(aes(time, value, colour = matrix))
)
