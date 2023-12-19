---
title: "basic SIR"
index_entry: "a very simple epidemic model"
author: Steve Walker
---

This is (nearly) the simplest possible 'vanilla' epidemic model, implemented as an example.

# States

| variable | description |
| -- | -- |
| S | Number of susceptible individuals |
| I | Number of infectious individuals |
| R | Number of recovered individuals | 

The size of the total population is,  $ N = S + I + R$.
# Parameters

| variable | description |
| -- | -- |
| $\beta$ | per capita transmission rate |
| $\gamma$ | per capita recovery rate |


# Dynamics 


```mermaid
%%{
  init: {
    'theme': 'base',
    'themeVariables': {
      'primaryColor': '#FDBF57',
      'primaryTextColor': '#000',
      'primaryBorderColor': '#5E6A71',
      'lineColor': '#7A003C',
      'secondaryColor': '#fff',
      'tertiaryColor': '#fff'
    }
  }
}%%
flowchart LR;
    S(S) -- &beta;I-->  I(I);
    I -- &gamma; --> R(R);
```


$$
\begin{align*}
\frac{dS}{dt} &= -\beta SI \\
\frac{dI}{dt} &= \beta SI - \gamma I \\
\frac{dR}{dt} &= \gamma I
\end{align*}
$$


# References

Earn, D.J.D. (2008). A Light Introduction to Modelling Recurrent Epidemics. In: Brauer, F., van den Driessche, P., Wu, J. (eds) Mathematical Epidemiology. Lecture Notes in Mathematics, vol 1945. Springer, Berlin, Heidelberg. https://doi.org/10.1007/978-3-540-78911-6_1