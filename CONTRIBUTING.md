# Contributing to `macpan2`

Thank you for contributing to `macpan2`.  Pull requests and issues are welcome.

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

We `#include` both `Rcpp.h` and `TMB.hpp`, which increases the possibility of namespace clashes. Our approach to addressing this is with [include-guarding](https://en.wikipedia.org/wiki/Include_guard). We assume that `TMB` takes precedence and so we include `Rcpp` first and then un-define any names in `Rcpp` that we want to use from `TMB` instead.  Here is the example of the `dnorm` function.

```
#include <Rcpp.h>
#ifdef dnorm
#undef dnorm
#endif
#include <TMB.hpp>
```

When you attempt to use functions from `TMB` when adding an engine function, you should be aware that you might need to do some include-guarding. You will find out via compilation errors.

## Developer Installation on Windows

Developers using `make` on Windows, could encounter the following compilation error.

```
Fatal error: can't write xxx bytes to section .text of macpan2.o: 'file too big' as: macpan2.o: too many sections
```

To resolve this, you may need to pass the `-Wa,-mbig-obj` compiler flag to GCC via the `Makeconf` file located in your R installation directory (typically here `C/Program Files/R/[your version of R]/etc/x64`). 

Locate the `CXXFLAGS` macro in the `Makeconf` file. It will look something like the following.
`CXXFLAGS = -O2 -Wall $(DEBUGFLAG) -mfpmath=sse -msse2 -mstackrealign $(LTO)`

Append the `-Wa,-mbig-obj` flag to the end of this line and save the file. You will likely need to make this change using a Windows Administrator role. 
`CXXFLAGS = -O2 -Wall $(DEBUGFLAG) -mfpmath=sse -msse2 -mstackrealign $(LTO) -Wa,-mbig-obj`

You should now be able to use `make` as described. Note this change might increase the compilation time (~2 min) as described [here](https://github.com/google/googletest/issues/1841#issuecomment-422342176). It would be nice to be able to set this flag globally for all Windows developers. An attempt was made to update the [Makefile](https://github.com/canmod/macpan2/blob/main/Makefile) with this additional line, `CXXFLAGS := $(CXXFLAGS) -Wa,-mbig-obj`, as suggested [here](https://stackoverflow.com/questions/7543978/how-to-pass-g3-flag-to-gcc-via-make-command-line), but it was not successful.


## Test Suite

We use the [testthat](https://testthat.r-lib.org/) package. Tests are located [here](https://github.com/canmod/macpan2/tree/main/tests/testthat).

To run tests interactively (e.g., in RStudio), please run the following code once in your R session.
```
source("tests/testthat/setup.R")
```

This will:
* Load packages that are assumed throughout the test suite
* Set `options(macpan2_verbose = FALSE)`
* Generate a cache of objects that can be (and are) reused in different tests



After running the setup, can also get access to a few useful functions for managing the testing cache. Three examples of usage follows.

You can check where the cache got placed by running the following.
```
test_cache_dir()
```

Here is an example of reading in a simulated trajectory of the `infection` variable over five time steps from the library SIR model.
```
test_cache_read("TRAJ-sir_5_infection.rds")
```

To get a list of all objects in the cache.
```
test_cache_list()
```
