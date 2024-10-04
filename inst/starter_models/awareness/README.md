---
title: "awareness models"
index_entry: "behaviour modifications in response to death"
author: Steve Walker
---

SEIR-type model with awareness-driven behaviour, inspired by [this](https://doi.org/10.1073/pnas.2009911117) and [this](https://doi.org/10.1016/j.jtbi.2022.111378).

# States

| variable | description  |
| -------- | ------------ |
| S        | Susceptible  |
| E        | Exposed      |
| I        | Infectious   |
| R        | Recovered    |
| D        | Dead         |
| H        | Hospitalized |
|          |              |


# Parameters

| variable               | description                                                                   |
| ---------------------- | ----------------------------------------------------------------------------- |
| $\beta$                | per capita transmission rate                                                  |
| $\mu$                  | per capita infection rate (average time spent in compartment E is $1/\alpha$) |
| $\gamma$               | per capita recovery/death rate                                                |
| `importation_prob`     | probability that one more infectious individual will arrive on any day.       |
| `memory_length`        | how long do people 'care about' deaths (days)                                 |
| $1/\gamma_h$           | mean time between isolation and death (days)                                  |
| $\delta_c$             | half-saturation constant for death awareness.                                 |
| $\delta_{\text{long}}$ | half-saturation constant for longer-term death awareness.                     |
| $f_D$                  | fraction of $\gamma$ that contributes to death rather than recovery           |
| $k$                    | exponent for half-saturation power law                                        |

# Base Awareness Model

$$
\begin{align*}
\frac{dS}{dt} &= -S\frac{\beta I}{N(1 + (I \gamma f_D / \delta_c)^k)} \\
\frac{dE}{dt} &= S\frac{\beta I}{N(1 + (I \gamma f_D / \delta_c)^k)} - \mu E \\
\frac{dI}{dt} &= \mu E- \gamma (1 - f_D) I \\
\frac{dR}{dt} &= \gamma (1 - f_D) I \\
\frac{dD}{dt} &= I \gamma f_D \\
\end{align*}
$$

# Delayed Death Awareness Model

$$
\begin{align*}
\frac{dS}{dt} &= -S\frac{\beta I}{N(1 + (I \gamma f_D / \delta_c)^k)} \\
\frac{dE}{dt} &= S\frac{\beta I}{N(1 + (I \gamma f_D / \delta_c)^k)} - \mu E \\
\frac{dI}{dt} &= \mu E- \gamma (1 - f_D) I \\
\frac{dR}{dt} &= \gamma (1 - f_D) I \\
\frac{dH}{dt} &= I \gamma f_D - H \gamma_h\\
\frac{dD}{dt} &= H \gamma_h \\
\end{align*}
$$

# Longer Memory Awareness Model

This model is identical to the [delayed death awareness model](#delayed-death-awareness-model), except that the factor $I \gamma f_D$ in the force of infection is replaced with a temporal convolution that takes a weighted average of past values with more recent values being weighted more heavily. See [here](https://github.com/canmod/macpan2/blob/main/inst/starter_models/awareness/tmb.R) for details.

# Importation Awareness Model

This model is identical to the [longer memory awareness model](#longer-memory-awareness-model) except that it includes random importations. These importations are simulated by drawing a Bernoulli random variable at each time step, adding it to the `I` box, and removing from the `R` box.
