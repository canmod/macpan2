.onLoad <- function(lib, pkg) {
}

.onUnload <- function(libpath) {
    library.dynam.unload("macpan2", libpath)
}
