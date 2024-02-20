---
title: "SIR with waning immunity"
index_entry: "a basic SIR model with a flow from R back to S"
author: Steve Walker
---

Endemic pathogens can sometimes be modelled by sending R back to S, thereby controlling susceptible depletion such that new infections keep arising indefinitely.

# States

| variable | description                       |
| -------- | --------------------------------- |
| S        | Number of susceptible individuals |
| I        | Number of infectious individuals  |
| R        | Number of recovered individuals   |

The size of the total population is, $N = S + I + R$.

# Parameters

| variable | description                     |
| -------- | ------------------------------- |
| $\beta$  | per capita transmission rate    |
| $\gamma$ | per capita recovery rate        |
| $\phi$   | per capita waning immunity rate |

# Dynamics 

$$
\begin{align*}
\frac{dS}{dt} &= -\beta S\frac{I}{N} + \phi R\\
\frac{dI}{dt} &= \beta S\frac{I}{N} - \gamma I \\
\frac{dR}{dt} &= \gamma I  - \phi R
\end{align*}
$$

# Model Specification

This model has been specified in the `sir_waning` directory [here](https://github.com/canmod/macpan2/blob/main/inst/starter_models/sir_waning/tmb.R) and is accessible from the `macpan2` model library (see [Example Models](https://canmod.github.io/macpan2/articles/example_models.html) for details). 

