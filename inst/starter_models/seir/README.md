---
title: "basic SEIR"
index_entry: "vanilla epidemic model with an exposed class"
author: Steve Walker
---

We introduce the *exposed* compartment, to capture the time period in which individuals have been exposed to the disease but are not able to infect others yet.

# States

| variable | description                       |
| -------- | --------------------------------- |
| S        | Number of susceptible individuals |
| E        | Number of exposed individuals     |
| I        | Number of infectious individuals  |
| R        | Number of recovered individuals   |

The size of the total population is,  $ N = S + E + I + R$.

# Parameters

| variable | description                                                                   |
| -------- | ----------------------------------------------------------------------------- |
| $\beta$  | per capita transmission rate                                                  |
| $\alpha$ | per capita infection rate (average time spent in compartment E is $1/\alpha$) |
| $\gamma$ | per capita recovery rate                                                      |

# Dynamics 

$$
\begin{align*}
\frac{dS}{dt} &= -\beta S\frac{I}{N} \\
\frac{dE}{dt} &= \beta S\frac{I}{N} - \alpha E \\
\frac{dI}{dt} &= \alpha E- \gamma I \\
\frac{dR}{dt} &= \gamma I
\end{align*}
$$

# Model Specification

This model has been specified in the `seir` directory [here](https://github.com/canmod/macpan2/blob/main/inst/starter_models/seir/tmb.R) and is accessible from the `macpan2` model library (see [Example Models](https://canmod.github.io/macpan2/articles/example_models.html) for details). 
