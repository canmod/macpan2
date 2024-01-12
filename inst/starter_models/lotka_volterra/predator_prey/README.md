---
title: "Lotka-Volterra"
index_entry: "simple predator-prey model"
author: Jennifer Freeman
---

The simplest Lotka-Volterra predator-prey model with two small modifications to include logistic prey growth and non-linear functional responses.

# Species

| variable | description         |
| -------- | ------------------- |
| $X$      | number of prey      |
| $Y$      | number of predators |

# Parameters

| variable | description                                                                                                              |
| -------- | ------------------------------------------------------------------------------------------------------------------------ |
| $\alpha$ | per capita growth rate of prey, in the absence of predators (Hastings, 1997)                                             |
| $\gamma$ | per capita rate of predator loss, in the absence of prey                                                                 |
| $\beta$  | per capita mortality rate of prey fom predation                                                                          |
| $\delta$ | per capita growth rate of predators from predation                                                                       |
| $K$      | prey carrying capacity                                                                                                   |
| $f(X)$   | per predator predation rate as a function of the number of prey, also called [functional response](#functional-response) |





# Dynamics

## Simple Predator-Prey

In the simple predator-prey Lotka-Volterra model, we assume exponential growth and decay for prey and predators respectively.

$$
\begin{align*}
\frac{dX}{dt} &= \alpha X - \beta X Y \\
\frac{dY}{dt} &= \delta XY - \gamma Y
\end{align*}
$$

## Logistic Prey Growth

We modify the simple model to include logisitic prey growth in the absence of predators with a prey carrying capacity of $K$.

$$
\begin{align*}
\frac{dX}{dt} &= \alpha X \left(1 - \frac{X}{K}\right)- \beta X Y \\
\frac{dY}{dt} &= \delta XY - \gamma Y
\end{align*}
$$

## Functional Response

The functional response $f(X)$ describes the predation rate as a function of prey density (Hastings, 1997). In the simplest case, we assume a linear function of prey, $f(X) = aX$ also called a [type I Holling](#holling-type-i) response. The simple predator-prey model includes a type I Holling response. Increasing functions that approach horizontal asymptotes can be used to represent more ecologically realistic predation rates to communicate that predation does not indefinitely increase when prey are abundant. The [type II Holling](#holling-type-ii) response is parameterized by the predator *attack rate* $a$, and the time elapsed by the predator capturing and consuming prey, called the *handling time* $h$ (Bolker, 2008). A [Holling type III](#holling-type-iii) response is defined with higher powers of prey density.

The general predator-prey dynamics incorporating the functional response $f(X)$ is given below.

$$
\begin{align*}
\frac{dX}{dt} &= \alpha X\left(1-\frac{X}{K}\right) - f(X) Y \\
\frac{dY}{dt} &= \delta f(X)Y - \gamma Y
\end{align*}
$$

Note if we parameterize the general dynamics by the inverse of the carrying capacity $K$, we can recover the exponential prey growth model by setting $K^{-1}=0$ and using the Holling type I response with $\beta$ as the attack rate. 

### Holling type I

$$ f(X) = a X \:, a>0$$

### Holling type II

$$ f(X) = \frac{a X}{1+ ahX} \:,  a > 0  \:, h > 0$$

### Holling type III

$$ f(X) = \frac{a X^k}{1 + ah X^k} \:, k > 1$$




# References
Bolker, B. (2008). *Ecological Models and Data in R*. Princeton: Princeton University Press. https://doi.org/10.1515/9781400840908
Hastings, A. (1997). Predator-Prey Interactions. In: *Population Biology*. Springer, New York, NY. https://doi.org/10.1007/978-1-4757-2731-9_8