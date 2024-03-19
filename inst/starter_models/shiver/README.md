---
title: "SHIVER = SEIR + H + V"
index_entry: "SEIR with vaccination and hospitalization"
author: Jennifer Freeman
---

This model builds on the basic SEIR model, with two additional compartments for vaccination and hospitalizations. 

Vaccines are typically subject to resource constraints and distribution strategies might prioritize vaccinations for specific subpopulations, such as immunocompromised people, to reduce bad outcomes. We model this with a flow of susceptibles entering the vaccination class. Following vaccination, most individuals return to the susceptible class when the acquired immune response wears off, called *vaccine waning*.

In reality, vaccinations are only partly effective in developing a protective host immune response. This *vaccine failure* includes both vaccine-related inadequacies and host-related reasons like health status and genetic factors (Wiedermann et al., 2016). Vaccine failure is modelled as flow from vaccination to the exposed class.

The SHIVER model assumes all individuals can be exposed to the disease, but transmission rates depend on vaccination status. Additionally, individuals with severe infections are hospitalized and assumed to be isolated, before recovering from the disease. Hospital isolation means this portion of infectious individuals no longer contribute to the transmission dynamics. 

# States

| variable | description                        |
| -------- | ---------------------------------- |
| S        | Number of susceptible individuals  |
| H        | Number of hospitalized individuals |
| I        | Number of infectious individuals   |
| V        | Number of vaccinated individuals   |
| E        | Number of exposed individuals      |
| R        | Number of recovered individuals    |

The size of the total population is,  $N = S + H + I  + V + E + R$, and the disease spreads through homogenous mixing of the subpopulation $N_{\text{mix}}=N -H$.

# Parameters

| variable   | description                                                                                         |
| ---------- | --------------------------------------------------------------------------------------------------- |
| $\phi$     | per capita vaccination rate of susceptibles                                                         |
| $\rho$     | per capita vaccine waning rate                                                                      |
| $\beta_S$  | per capita transmission rate for susceptibles (in $N_{\text{mix}}$ population)                      |
| $\beta_V$  | per capita transmission rate for vaccinated individuals (in $N_{\text{mix}}$ population)            |
| $\alpha$   | per capita infection rate (average time spent in compartment $E$ is $1/\alpha$)                     |  |
| $\gamma_I$ | per capita recovery rate for infected  individuals                                                  |
| $\gamma_H$ | per capita recovery rate for hospitalized individuals                                               |
| $\sigma$   | per capita rate at which infected individuals develop severe infections and require hospitalization |

# Dynamics 

$$
\begin{align*}
\frac{dS}{dt} &= -\beta_{S} S\frac{I}{N_{\text{mix}}} - \phi S + \rho V\\
\frac{dV}{dt} &=  \phi S - \rho V - \beta_{V} V\frac{I}{N_{\text{mix}}} \\
\frac{dE}{dt} &= (\beta_{S} S + \beta_{V} V) \frac{I}{N_{\text{mix}}} - \alpha E \\
\frac{dI}{dt} &= \alpha E - \gamma_I I - \sigma I\\
\frac{dH}{dt} &= \sigma I- \gamma_H H \\
\frac{dR}{dt} &= \gamma_I I + \gamma_H H
\end{align*}
$$

This model could be optionally parameterized with $\beta_{S} = \beta$, and $\beta_{V} = a \beta$ with $a \in (0, 1)$ to explicitly show that the rate at which vaccinated individuals acquire infection and transmit the disease is reduced when compared to unvaccinated individuals (Brauer & Castillo-Chavez, 2012).

# Model Specification

This model has been specified in the `shiver` directory [here](https://github.com/canmod/macpan2/blob/main/inst/starter_models/shiver/tmb.R) and is accessible from the `macpan2` model library (see [Example Models](https://canmod.github.io/macpan2/articles/example_models.html) for details). 

# References

Brauer, F., & Castillo-Chavez, C. (2012). Mathematical Models in Population Biology and Epidemiology (Vol. 40). Springer New York. https://doi.org/10.1007/978-1-4614-1686-9

Wiedermann, U., Garner-Spitzer, E., & Wagner, A. (2016). Primary vaccine failure to routine vaccines: Why and what to do? Human Vaccines & Immunotherapeutics, 12(1), 239–243. https://doi.org/10.1080/21645515.2015.1093263



