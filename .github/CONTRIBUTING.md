# Contributing to `macpan2`

Thank you for contributing to `macpan2`.  Pull requests and issues are welcome!

Developers can see [here](https://canmod.github.io/macpan2/articles/index.html#developer) for documentation useful those who will contribute code.

## Developer Installation

Developers and contributors should clone this repository and call `make` at the command-line in the top level directory. The following `make` rules are available for getting more control over the build process.

```
make quick-install     # for changes that only modify R source
make quick-doc-install # for changes that modify R source and roxygen comments
make quick-test        # quick-doc-install + run-examples + run-tests
make run-examples      # help file checks only (without package rebuild)
make run-tests         # run scripts in tests (without package rebuild)
make full-install      # for all changes, including changes to C++ source
make src-update        # push changes to dev.cpp to macpan2.cpp (see below)
make enum-update       # register new C++ engine functions on the R-side
make engine-doc-update # generate roxygen comments from comments in dev.cpp
make doc-update        # roxygenize
make pkg-build         # build the R package
make pkg-install       # install the R package from the build
make pkg-check         # R package checks
```

## `C++` Development

In most R packages with compiled code, developers edit the source files to be compiled in the `src` directory. In `macpan2` there is a single file in that directory called `macpan2`, which is generated automatically from the file `misc/dev/dev.cpp`. This setup allows for quicker C++ development cycles, because developers can edit `misc/dev/dev.cpp` and then use this file in tests without needing to re-install the package with the new source. In particular, the above hello-world example could use `dev.cpp` as follows.

```
library(macpan2)
macpan2:::dev_compile() ## compile dev.cpp
options(macpan2_dll = "dev")
sir = mp_tmb_library("starter_models", "sir", package = "macpan2")
mp_simulator(sir, time_steps = 100, outputs = "I")
```

To update `src/macpan2` to the state of `misc/dev/dev.cpp` one may run `make src-update`.

Running with `misc/dev/dev.cpp` will print out debugging information in a verbose manner, whereas `src/macpan2.cpp` will not. The `src-update` make rule removes the `#define MP_VERBOSE` flag at the top of the file. 
