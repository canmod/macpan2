Wastewater model
================
Jennifer Freeman

-   <a href="#packages-used" id="toc-packages-used">Packages Used</a>
-   <a href="#model-specification" id="toc-model-specification">Model
    Specification</a>
-   <a href="#states" id="toc-states">States</a>
-   <a href="#parameters" id="toc-parameters">Parameters</a>
-   <a href="#dynamics" id="toc-dynamics">Dynamics</a>
-   <a href="#model-specification-1" id="toc-model-specification-1">Model
    Specification</a>
-   <a href="#references" id="toc-references">References</a>

The McMasterPandemic model ([Bolker et al. 2024](#ref-macpan)) modified
to include a wastewater component.

# Packages Used

The code in this article uses the following packages.

``` r
library(macpan2)
library(ggplot2)
library(dplyr)
```

# Model Specification

This model has been specified in the `ww` directory
[here](https://github.com/canmod/macpan2/blob/main/inst/starter_models/ww/tmb.R)
and is accessible from the `macpan2` model library (see [Example
Models](https://canmod.github.io/macpan2/articles/example_models.html)
for details). We can read in the model specification using the
`mp_tmb_library` command.

``` r
spec = mp_tmb_library(
    "starter_models"
  , "ww"
  , package = "macpan2"
)
```

This specification can be used to draw the following flow diagrams using
code found in the [source for this
article](https://github.com/canmod/macpan2/blob/main/inst/starter_models/ww/README.Rmd).
For clarity, we first draw the epidemiological components of the model
first, followed by the wastewater shedding component.

![](./figures/diagram-1.png)<!-- -->![](./figures/diagram-2.png)<!-- -->

# States

| variable | description                                                         |
|----------|---------------------------------------------------------------------|
| $S$      | Number of susceptible individuals                                   |
| $E$      | Number of exposed individuals                                       |
| $I_a$    | Number of asymptomatic infectious individuals                       |
| $I_p$    | Number of pre-symptomatic infectious individuals                    |
| $I_m$    | Number of mildly infectious individuals                             |
| $I_s$    | Number of severely infectious individuals                           |
| $H$      | Number of hospitalized individuals (acute care)                     |
| $ICU_s$  | Number of individuals admitted to the ICU with a survival prognosis |
| $ICU_d$  | Number of individuals admitted to the ICU with a death prognosis    |
| $H_2$    | Number of hospitalized individuals (acute care) after ICU stay      |
| $D$      | Number of dead individuals                                          |
| $R$      | Number of recovered individuals                                     |
| $W$      | Concentration of viral particles in wastewater                      |
| $A$      | Total accumulated concentration of virus in wastewater over time    |

The size of the total population is,
$N = S + E + I_a + I_p + I_m + I_s + H + ICU_s + ICU_d + H_2 + D + R$.

# Parameters

| variable      | description                                                                         |
|---------------|-------------------------------------------------------------------------------------|
| $\beta_0$     | baseline (non-intervention) transmission across categories                          |
| $C_a$         | relative asymptomatic transmission (or contact) proportion                          |
| $C_p$         | relative presymptomatic transmission (or contact) proportion                        |
| $C_m$         | relative mildly transmission (or contact) proportion                                |
| $C_s$         | relative severly transmission (or contact) proportion                               |
| $\alpha$      | fraction of infections that are asymptomatic                                        |
| $\mu$         | fraction of symptomatic infections that are mild                                    |
| $\sigma$      | 1/time in exposed class                                                             |
| $\gamma_a$    | 1/time to recovery for asymptomatic infections                                      |
| $\gamma_p$    | 1/time in pre-symptomatic state                                                     |
| $\gamma_m$    | 1/time to recovery for mildly symptomatic infections                                |
| $\gamma_s$    | 1/time spent in severely symptomatic state before either hospitalization or death   |
| $\rho$        | 1/time in hospital (initial acute care admission)                                   |
| $\delta_{nh}$ | probability of mortality without hospitalization                                    |
| $\phi_1$      | fraction of hospitalized infections that only require acute care (no ICU admission) |
| $\phi_2$      | fraction of ICU infections that are fatal                                           |
| $\psi_1$      | 1/time spent in ICU before returning to acute care                                  |
| $\psi_2$      | 1/time spent in ICU before dying                                                    |
| $\psi_3$      | 1/time in post-ICU acute care before hospital discharge                             |
| $\nu$         | viral shedding rate to wastewater                                                   |
| $\xi$         | rate at which virus is denaturing/removed from wastewater                           |

# Dynamics

<!-- nb. do not remove the spaces in `\delta _ {nh}`. https://github.com/github/markup/issues/1575 -->

$$
\begin{split}
\frac{dS}{dt} &= -\beta_0\left(C_aI_a +C_pI_p + C_mI_m(1-\text{iso}_m) + C_sI_s(1-\text{iso}_s)\right)S/N \\
\frac{dE}{dt} &= \beta_0\left(C_aI_a +C_pI_p + C_mI_m(1-\text{iso}_m) + C_sI_s(1-\text{iso}_s)\right)S/N - \sigma E \\
\frac{dI_a}{dt} &= \alpha\sigma E- \gamma_a I_a \\
\frac{dI_p}{dt} &= (1-\alpha)\sigma E- \gamma_p I_p \\
\frac{dI_m}{dt} &= \mu\gamma_pI_p- \gamma_m I_m \\
\frac{dI_s}{dt} &= (1-\mu)\gamma_pI_p- (1-\delta _ {nh})\gamma_s I_s \\
\frac{dH}{dt} &= (1-\delta _ {nh})\phi_1\gamma_s I_s - \rho H  \\
\frac{dICU_s}{dt} &= (1-\delta _ {nh})(1-\phi_1)(1-\phi_2)\gamma_s I_s - \psi_1 ICU_s \\
\frac{dICU_d}{dt} &= (1-\delta _ {nh})(1-\phi_1)\phi_2\gamma_s I_s - \psi_2 ICU_d \\
\frac{dH_2}{dt} &= \psi_1 ICU_s - \psi_3 H_2  \\
\frac{dR}{dt} &= \gamma_a I_a + \gamma_m I_m + \rho H + \psi_3 H_2 \\
\frac{dD}{dt} &=  \psi_2 ICU_d \\
\frac{dW}{dt} &= \nu I_a + \nu I_p + \nu I_m + \nu I_s - \xi W \\
\frac{dA}{dt} &= \xi W
\end{split}
$$

# Model Specification

This model has been specified in the `ww` directory
[here](https://github.com/canmod/macpan2/blob/main/inst/starter_models/ww/tmb.R)
and is accessible from the `macpan2` model library (see [Example
Models](https://canmod.github.io/macpan2/articles/example_models.html)
for details).

# References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-macpan" class="csl-entry">

Bolker, Ben, David Earn, Morgan Kain, Mike Li, and Jonathan Dushoff.
2024. *McMasterPandemic: Pandemic Model*.
<https://github.com/bbolker/McMasterPandemic>.

</div>

</div>
