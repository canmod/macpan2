# Model Specification Format Challenges

The `variables.csv` file contains two types variables, state and flow, as well as others that do not fit as nicely. For example, what do we do about transmission variables that need to map states to flows?

* So we are tempted to place these items into another kind of file, say `mappings.csv`. But now when resolving `flows.csv` we need to look in both `variables.csv` and `mappings.csv`.
* An alternative is to have lots of blank cells in `variables.csv`. But this how do we name the mappings without lots of dots in the basic state and flow variables?
* An alternative is to break the rule that all labels have the same numbers of dots, but this breaks the long-existing labelling standard.
* An alternative is to add mapping variables to `variables.csv` as vectors (or matrices?), but this causes problems for lining up the elements of these matrices through mechanisms like `flows.csv`.

All this to say that from the user's perspective what we want is really the first suggestion, and bite the bullet as a developer for resolving `flows.csv` in multiple locations.

Or actually, maybe the better thing is to have smarter labelling standards that check to see if labels can be truncated without destroying invertibility?

Well ... maybe we can save the labelling standards by letting the mapping component names get split into the atomic labels first to produce valid labels that can be inverted.

Well ... this doesn't need to happen, because in the mappings file we might be able to keep them split up.

```
flow[infection] ~ groupSums(beta * state[trans_infectious], trans_infection, length(infection))
```
