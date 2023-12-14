---
title: "Lotka-Volterra"
index_entry: "simple two-species competition model"
author: Jennifer Freeman
---

- $X$ - number of individuals in species $X$  
- $Y$ - number of individuals in species $Y$ 
- $r_i$ - growth rate of species $I$
- $a_{ij}$ - intra/inter-specific density dependence, ``effect of species $j$ on species $i$'' (Hastings, 1997)

$$
\begin{align*}
\frac{dX}{dt} &= r_x X (1 - a_{xx}X - a_{xy}Y) \\
\frac{dY}{dt} &= r_y Y (1 - a_{yy}Y - a_{yx}X)
\end{align*}
$$

Hastings, A. (1997). Competition. In: Population Biology. Springer, New York, NY. https://doi.org/10.1007/978-1-4757-2731-9_7