# qens: Quantile prediction ensembles

This package is a wrapper around the python implementation of weighted mean and weighted median quantile ensembles in [qenspy](https://github.com/reichlab/qenspy).

The package uses `reticulate` to interact with Python. This means that installing this package is a little more cumbersome than a typical R package. You will need to do the following:

1. Run `remotes::install_github("reichlab/qensr")` in an R session.

2. Ensure that the [reticulate](https://rstudio.github.io/reticulate/) package works. You should be able to load the package in an R session and execute Python commands such as the following:
```
library(reticulate)
reticulate::py_run_string("import sys; print(sys.version)")
```
Note that `qens` functionality has only been tested with Python version 3.9; earlier Python versions may not work reliably.

3. Install the Python dependencies for `qens`:
```
library(qens)
qens::install_py_dependencies()
```

4. Restart your R session, and confirm that things are working correctly. A good way to check this is by working through the package vignette.
