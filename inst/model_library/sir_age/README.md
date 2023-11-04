---
title: "Age-Structured SIR"
index_entry: "Basic SIR Model with Age Groups"
author: Steve Walker
---

This model directory has the following files.

1. `model_structure.R` -- Defines the structure of the model with neither numerical inputs nor numerical outputs.

2. `numerical_inputs.R` -- Assigns numerical values to vectors.

3. `simulator.R` -- Constructs a simulator object that can be used to run simulations.

4. `experimentation.R` -- Example code for using the simulator object.  Currently it just produces a graph.

5. `main.R` -- Runs 1-3 above.


All of these files should be run from the root directory of `macpan2`.

Files 1-4 depend on each other in sequential order.
