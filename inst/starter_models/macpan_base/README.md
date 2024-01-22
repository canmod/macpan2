---
title: "MacPan base"
index_entry: "re-implementation of the McMaster group's COVID-19 model"
author: Jennifer Freeman
---

The McMasterPandemic model (Bolker, 2022); A modified SEIR model that incorporates additional infectious compartments to reflect the current knowledge of COVID-19 epidemiology. Exposed individuals enter four infectious compartments characterized by the individuals symptom status (asymptomatic, pre-symptomatic, mild, and severe). Severely infected individuals require care through hospital and/or Intensive Care Unit (ICU) compartments and either recover or die.

# States

| variable | description                                                         |
| -------- | ------------------------------------------------------------------- |
| S        | Number of susceptible individuals                                   |
| E        | Number of exposed individuals                                       |
| I~a~     | Number of asymptomatic infectious individuals                       |
| I~p~     | Number of pre-symptomatic infectious individuals                    |
| I~m~     | Number of mildly infectious individuals                             |
| I~s~     | Number of severely infectious individuals                           |
| H        | Number of hospitalized individuals (acute care)                     |
| ICU~s~   | Number of individuals admitted to the ICU with a survival prognosis |
| ICU~d~   | Number of individuals admitted to the ICU with a death prognosis    |
| H~2~     | Number of hospitalized individuals (acute care) after ICU stay      |
| D        | Number of dead individuals                                          |
| R        | Number of recovered individuals                                     |

The size of the total population is,  $N = S + E + I_a + I_p + I_m + I_s + H +  ICU_s + ICU_d + H_2 + D + R$.

# Parameters

| variable       | description                                                                         |
| -------------- | ----------------------------------------------------------------------------------- |
| $\beta_0$      | baseline (non-intervention) transmission across categories                          |
| $C_a$          | relative asymptomatic transmission (or contact) proportion                          |
| $C_p$          | relative presymptomatic transmission (or contact) proportion                        |
| $C_m$          | relative mildly transmission (or contact) proportion                                |
| $C_s$          | relative severly transmission (or contact) proportion                               |
| $\alpha$       | fraction of infections that are asymptomatic                                        |
| $\mu$          | fraction of symptomatic infections that are mild                                    |
| $\sigma$       | 1/time in exposed class                                                             |
| $\gamma_a$     | 1/time to recovery for asymptomatic infections                                      |
| $\gamma_p$     | 1/time in pre-symptomatic state                                                     |
| $\gamma_m$     | 1/time to recovery for mildly symptomatic infections                                |
| $\gamma_s$     | 1/time spent in severely symptomatic state before either hospitalization or death   |
| $\rho$         | 1/time in hospital (initial acute care admission)                                   |
| $\delta$       | fraction of hospitalized infections that are fatal **                               |
| $\delta_{nh}$  | probability of mortality without hospitalization                                    |
| $\text{iso}_m$ | relative self-isolation/distancing of mild cases *                                  |
| $\text{iso}_s$ | relative self-isolation/distancing of severe cases *                                |
| $\phi_1$       | fraction of hospitalized infections that only require acute care (no ICU admission) |
| $\phi_2$       | fraction of ICU infections that are fatal                                           |
| $\psi_1$       | 1/time spent in ICU before returning to acute care                                  |
| $\psi_2$       | 1/time spent in ICU before dying                                                    |
| $\psi_3$       | 1/time in post-ICU acute care before hospital discharge                             |

\* These parameters were not detailed in [Papst](#references) (TBD), but they were included in model definition files. 
\** This parameter does not appear in the model dynamics.

# Dynamics 

<!-- nb. do not remove the spaces in `\delta _ {nh}`. https://github.com/github/markup/issues/1575 -->
$$
\begin{align*}
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
\frac{dD}{dt} &=  \psi_2 ICU_d
\end{align*}
$$

When there were discrepancies with how the model is expressed in Papst (TBD) versus model definition files, the latter was chosen.

# References 

Bolker B, Steve Walker, David Earn, Morgan Kain, Mike Li, Jonathan Dushoff (2022). McMasterPandemic: Pandemic Model. R package version 0.2.0.0, https://github.com/mac-theobio/McMasterPandemic.

Papst, Irena, Mike Li, David Champredon, Aamir Fazil, Jonathan Dushoff, Ben Bolker, David Earn (draft/preprint). Forecasting infectious disease spread as new variants emerge and vaccination rates increase. Accessed 4 Jan 2024.
