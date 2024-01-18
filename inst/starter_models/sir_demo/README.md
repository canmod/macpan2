---
title: "SIR with demography"
index_entry: "An SIR model with birth and death"
---

We assume new individuals (births) join the susceptible compartment, and individuals can leave the population (die) from any compartment.

# States

| variable | description                       |
| -------- | --------------------------------- |
| S        | Number of susceptible individuals |
| I        | Number of infectious individuals  |
| R        | Number of recovered individuals   |

The size of the total population is,  $ N = S + I + R$.

# Parameters

| variable | description                  |
| -------- | ---------------------------- |
| $\beta$  | per capita transmission rate |
| $\gamma$ | per capita recovery rate     |
| $\nu$    | per capita birth rate        |
| $\mu$    | per capita mortality rate    |

The SIR model with demography often assumes that the time scale of epidemic changes is much shorter than demographic changes (Earn, 2008). This translates to a constant population size $N$ over time, with $\nu = \mu$. We parameterize birth and mortality rates separately to allow for the general case in which epidemic and demographic dynamics occur on similar time scales. 

# Dynamics 

$$
\begin{align*}
\frac{dS}{dt} &= \nu N -\beta SI - \mu S \\
\frac{dI}{dt} &= \beta SI - \gamma I - \mu I \\
\frac{dR}{dt} &= \gamma I - \mu R
\end{align*}
$$

# References

Earn, D.J.D. (2008). A Light Introduction to Modelling Recurrent Epidemics. In: Brauer, F., van den Driessche, P., Wu, J. (eds) Mathematical Epidemiology. Lecture Notes in Mathematics, vol 1945. Springer, Berlin, Heidelberg. https://doi.org/10.1007/978-3-540-78911-6_1
