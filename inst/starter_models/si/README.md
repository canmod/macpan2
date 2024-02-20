---
title: "basic SI"
index_entry: "a very simple epidemic model"
author: Jennifer Freeman
---

This is the simplest possible epidemic model.

# States

| variable | description                       |
| -------- | --------------------------------- |
| S        | Number of susceptible individuals |
| I        | Number of infectious individuals  |

The size of the total population is, $N = S + I$.

# Parameters

| variable | description                  |
| -------- | ---------------------------- |
| $\beta$  | per capita transmission rate |

# Dynamics 

$$
\begin{align*}
\frac{dS}{dt} &= -\beta S\frac{I}{N} \\
\frac{dI}{dt} &= \beta S\frac{I}{N}\\
\end{align*}
$$

# Model Specification

This model has been specified in the `si` directory [here](https://github.com/canmod/macpan2/blob/main/inst/starter_models/si/tmb.R) and is accessible from the `macpan2` model library (see [Example Models](https://canmod.github.io/macpan2/articles/example_models.html) for details). 

# References

Earn, D.J.D. (2008). A Light Introduction to Modelling Recurrent Epidemics. In: Brauer, F., van den Driessche, P., Wu, J. (eds) Mathematical Epidemiology. Lecture Notes in Mathematics, vol 1945. Springer, Berlin, Heidelberg. https://doi.org/10.1007/978-3-540-78911-6_1
