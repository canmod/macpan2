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

The size of the total population is,  $N = S + I$.

# Parameters

| variable | description                  |
| -------- | ---------------------------- |
| $\beta$  | per capita transmission rate |

# Dynamics 

$$
\begin{align*}
\frac{dS}{dt} &= -\beta SI \\
\frac{dI}{dt} &= \beta SI \\
\end{align*}
$$

# References

Earn, D.J.D. (2008). A Light Introduction to Modelling Recurrent Epidemics. In: Brauer, F., van den Driessche, P., Wu, J. (eds) Mathematical Epidemiology. Lecture Notes in Mathematics, vol 1945. Springer, Berlin, Heidelberg. https://doi.org/10.1007/978-3-540-78911-6_1
