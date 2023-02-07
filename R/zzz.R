# global reference to qenspy (will be initialized in .onLoad)
qenspy <- NULL

.onLoad <- function(libname, pkgname) {
    # use superassignment to update global reference to qenspy
    qenspy <<- reticulate::import("qens", delay_load = TRUE)
}
