- intro video + hands on sesh was a good format
- presentation: work a bit bring the big picture together (based on 1:1 discussions with participants afterwards)
- current challenge: how to connect bookkeeping stuff to the dynamics (expressions/how people simulate models), which i understand—-what the vignette tries to start explaining without getting into expressions per se
    - make a *real* quickstart (simulate an SIR)
    - change the name of the current quickstart + add a section at the end explaining the suppressed expressions list, initial vecs, unstructured matrices
    - follow up with a more structured model, calibration, and ideally forecasting (generating ensembles in the future)
- dev focus: making macpan2 friendlier (it's powerful, but is it friendly)
    - making the interface simpler for basic models so people don't *have* to get into the specifics of Euler, RK4, etc, but they can if they want to, maybe this involves writing friendly wrappers that don’t expose *everything* to the user to start
    - connected to coming up with a real quickstart
- ideas for a “friendlier” interface
    - make reproducible chunks (modules) for components of the expression list and/or an expression list shared between models (classify models based on this)
    - modules for transmission, post-processing delays, etc.
    - think about reducing the naming burden (unique column names in ledgers for different expressions)
        - expression list that doesn’t directly reference the ledgers but just expresses the logic… e.g. `lambda = beta*S` vs `rate[force_of_infection] ~ rate[transmission_rate]*state[from_state]`
        - make the expression list look more like deSolve

---

# To do

- merge `main` into `refactorcpp` and clean up techincal debt (make tests pass, get rid of old code/examples except for model examples (later?), etc.)
- merge `refactorcpp` into `main` as v1.0.0 (full API change will be v2.0.0)
- a real quickstart (simulation)
- go back and translate model examples into new interface and use that to inform the development of a "friendlier" `macpan2` API
- developing a friendlier `macpan2` API
- "quickstart: calibration" and "quickstart: product models"
