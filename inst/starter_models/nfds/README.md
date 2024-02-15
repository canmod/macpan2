---
title: "NFDS and vaccine design"
index_entry: "An ecological model using population genomics to design optimal vaccines as implemented in Colijn et al. (2020)"
author: Jennifer Freeman
---

<!-- population-->
*Streptococcus pneumoniae* (pneumococcus) is a bacteria that can cause *invasive pneumococcal disease* (IPD), such as meningitis and bacterial pnuemonia, in vulnerable populations (ie. young children and elderly) (PHAC, 2023). Pneumococci typically colonize the respiratory tract and its subsequent occupation is called *carriage* (Coughtrie et al. 2019). Carriage pneumococcus is common and individuals are often asymptomatic, becoming *carriers* of the bacterium (PHAC, 2023). Pneumococci populations are further subdivided by their epidemiological characteristics (*serotypes*) and genetic variation (*genotypes* and the *loci* they encode).

<!-- vaccine/motivation -->
*Protein–polysaccharide conjugate vaccines* (PCVs) were introduced to target small subsets of pneumococcal serotypes to reduce IPD burden (Colijn et al. 2020). In a carriage population after PCV introduction, vaccine serotypes are removed through herd immunity (Colijn et al. 2020). Pneumococcal carriage prevalence is known to be relatively stable over time as *serotype replacement* occurs where non-vaccine serotypes compete and replace vaccine types (Corander et al. 2017). The effect of PCVs in a post-vaccine carriage population can be problematic when non-vaccine serotypes that are highly *invasive*, "the rate at which they progress from carriage to cause IPD", or anti-microbial resistant become more prevalent (Colijn et al. 2020). 

<!-- the problem -->
Colijn et al. formulated optimal vaccine designs that minimized IPD burden in post-vaccine carriage populations. Two distinct carriage population datasets were included in model experiments. One from a vaccinated group of children in Massachusetts and the second, an unvaccinated group from the Maela refugee camp in Thailand. In simulations, a hypothetical vaccine was introduced into the population, and a duration of 10 years was selected to evaluate the effect of this vaccine on the carriage community make-up of pneumococcus.

<!-- the model -->
A multi-locus *negative frequency-dependent selection* (NFDS) model was implemented as a first order ODE to simulate the dynamics of genotype prevalence in carriage over time. This model assumes "alleles are most beneficial to genotypes when they are rare" (Corander et al. 2017) and the fitness of each genotype is  reflected in its dynamic reproductive rate. This study ignores genetic *recombination*, assuming the set of unique genotypes in each population is fixed over time (Colijn et al. 2020). Genotype prevalences in Colijn et al. were simulated using an ODE solver in Matlab.

<!-- the optimization -->
IPD burden was computed using serotype invasiveness estimates, derived from a meta-analysis, and the post-vaccine carriage prevalences (the ODE solution). Vaccine design was optimized using Bayesian optimization and a genetic algorithm, with additional constraints on vaccine valency and forced serotype inclusion (Colijn et al. 2020).


# States


*Note:* Input values provided here are for the Massachusetts population ($M=603$, $L=1090$), to get corresponding Maela input values see [write_data.m](https://github.com/canmod/macpan2/blob/b162156ca3d2787de90f5c080827ee9547b14fab/inst/starter_models/nfds/data/write_data.m).
| variable   | description                            | input value                                                                                                                                      |
| ---------- | -------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------ |
| $Y_{i}(t)$ | prevalence of genotype $i$ at time $t$ | [intial conditions (t=0)](https://github.com/canmod/macpan2/blob/b162156ca3d2787de90f5c080827ee9547b14fab/inst/starter_models/nfds/data/ics.csv) |



# Parameters

*Note:* Input values provided here are for the Massachusetts population ($M=603$, $L=1090$), to get corresponding Maela input values see [write_data.m](https://github.com/canmod/macpan2/blob/b162156ca3d2787de90f5c080827ee9547b14fab/inst/starter_models/nfds/data/write_data.m).

## Fixed


| variable  | description                                                                                                                                                               | csv input file                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| --------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| $w_l$     | NFDS weight for locus $l$                                                                                                                                                 | [locusweights](https://github.com/canmod/macpan2/blob/b162156ca3d2787de90f5c080827ee9547b14fab/inst/starter_models/nfds/data/locusweights.csv)                                                                                                                                                                                                                                                                                                                                                                                           |
| $e_l$     | equilibrium frequency for locus $l$                                                                                                                                       | [locusfreq](https://github.com/canmod/macpan2/blob/b162156ca3d2787de90f5c080827ee9547b14fab/inst/starter_models/nfds/data/locusfreq.csv)                                                                                                                                                                                                                                                                                                                                                                                                 |
| $\kappa$  | carriage population carrying capacity                                                                                                                                     | hardcoded, $\kappa = 10,000$                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| $G_{i,l}$ | $G=$ genotype-loci matrix (size $M$ by $L$). $G_{i,l}=1$ if genotype $i$ encodes locus $i$, 0 otherwise.                                                                  | [G](https://github.com/canmod/macpan2/blob/b162156ca3d2787de90f5c080827ee9547b14fab/inst/starter_models/nfds/data/G.csv)                                                                                                                                                                                                                                                                                                                                                                                                                 |
| $\rho$    | constant representing the "overall strength of NFDS" (Colijn et al., 2020), computed as $\log(1 + \sigma)$ where $\sigma$ is taken from Corander et al. (2017).           | [sigma](https://github.com/canmod/macpan2/blob/b162156ca3d2787de90f5c080827ee9547b14fab/inst/starter_models/nfds/data/sigma.csv)                                                                                                                                                                                                                                                                                                                                                                                                         |
| $v$       | vaccine efficacy parameter                                                                                                                                                | [v](https://github.com/canmod/macpan2/blob/b162156ca3d2787de90f5c080827ee9547b14fab/inst/starter_models/nfds/data/v.csv)                                                                                                                                                                                                                                                                                                                                                                                                                 |
| $m$       | uniform migration rate per genotype                                                                                                                                       | [m](https://github.com/canmod/macpan2/blob/b162156ca3d2787de90f5c080827ee9547b14fab/inst/starter_models/nfds/data/m.csv)                                                                                                                                                                                                                                                                                                                                                                                                                 |
| $r_i$     | vaccine strategy, $r_i=r$ "if genotype $i$ encodes an antigen included in the vaccine" (Colijn et al., 2020), 0 otherwise. Computed as $r_i = -\log(1- (v \text{vax}_i))$ | The user specifies a vector of serotypes to be included, excluded, and *varied* (varied means these serotypes will be optimized i.e. optimizer decides if each serotype in this vector will be included/excluded) in the vaccine. This vaccine vector is then mapped to the level of genotypes, creating a binary vector $\text{vax}$. (See an example of user input for the `macpan2` model implementation [here](https://github.com/canmod/macpan2/blob/7c76cfb2f6a2e329e7cc8af4d46229d37692185f/inst/starter_models/nfds/tmb.R#L78)). |



## Time Varying
| variable  | description                                              | calculation                                          |
| --------- | -------------------------------------------------------- | ---------------------------------------------------- |
| $N(t)$    | carriage population of pneumococci organisms at time $t$ | $N(t) = \sum_{i=1}^{M} Y_i$                          |
| $f_{l,t}$ | instantaneous frequency for locus $l$ at time $t$        | $f_{l,t} = \frac{1}{N(t)} \sum_{i=1}^{M} Y_i G_{il}$ |


# Dynamics 

$$\frac{dY_i}{dt} = \left( \log\left( \frac{\kappa}{N(t)}\right) -r_i + \rho\left( \sum_{l=1}^{L} w_l G_{il} (e_l - f_{l,t})\right) \right) Y_i + m, \quad i \in \{1, M\}$$

# References

Colijn, C., Corander, J., & Croucher, N. J. (2020). Designing ecologically optimized pneumococcal vaccines using population genomics. *Nature Microbiology*, 5(3), 473–485. https://doi.org/10.1038/s41564-019-0651-y

Corander, J., Fraser, C., Gutmann, M.U. et al. (2017) Frequency-dependent selection in vaccine-associated pneumococcal population dynamics. *Nature Ecology and Evolution*, 1, 1950–1960. https://doi.org/10.1038/s41559-017-0337-x

Coughtrie, A. L., Jefferies, J. M., Cleary, D. W. et al. (2019). Microbial epidemiology and carriage studies for the evaluation of vaccines. *Journal of Medical Microbiology*, 68(10), 1408-1418. https://doi.org/10.1099/jmm.0.001046

Public Health Agency of Canada [PHAC] (2023, July 10). Invasive Pneumococcal Disease. Retrieved February 8, 2024, from https://www.canada.ca/en/public-health/services/immunization/vaccine-preventable-diseases/invasive-pneumococcal-disease/health-professionals.html