---
title: "Lotka-Volterra"
index_entry: "simple two-species competition model"
author: Jennifer Freeman
---

The simplest Lotka-Volterra competition model with two competing species.

# Species

| variable | description                          |
| -------- | ------------------------------------ |
| $X$      | number of individuals in species $x$ |
| $Y$      | number of individuals in species $y$ |

# Parameters

| variable      | description                                                                                        |
| ------------- | -------------------------------------------------------------------------------------------------- |
| $r_i$         | growth rate of species $i$                                                                         |
| $a_{ij}$      | intra/inter-specific density dependence, ``effect of species $j$ on species $i$'' (Hastings, 1997) |
| $K_{i}$       | carrying capacity of species $i$                                                                   |
| $\alpha_{ij}$ | relative effect of species $j$ on species $i$ (Hastings, 1997)                                     |

# Dynamics
$$
\begin{align*}
\frac{dX}{dt} &= r_x X (1 - a_{xx}X - a_{xy}Y) \\
\frac{dY}{dt} &= r_y Y (1 - a_{yy}Y - a_{yx}X)
\end{align*}
$$

This model can also be expressed in an equivalent form using the carrying capacity of each species.

$$
\begin{align*}
\frac{dX}{dt} &= \frac{r_x X}{K_x} (K_x - X - \alpha_{xy}Y) \\
\frac{dY}{dt} &= \frac{r_y Y}{K_y} (K_y - Y - \alpha_{yx}X)
\end{align*}
$$


# References
Hastings, A. (1997). Competition. In: *Population Biology*. Springer, New York, NY. https://doi.org/10.1007/978-1-4757-2731-9_7
