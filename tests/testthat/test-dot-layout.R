library(macpan2)
## debug(macpan2:::dot_layout)
if (require(Rgraphviz, quietly = TRUE)) {
    specs <- mp_tmb_entire_library()
    no_mpflows <- c("lotka_volterra_competition",
                "lotka_volterra_predator_prey",
                "nfds")
    for (s in no_mpflows) {
        expect_error(dot_layout(specs[[s]]), "was spec defined")
    }
    for (s in setdiff(names(specs), no_mpflows)) {
        d <- dot_layout(specs[[s]])
        expect_s4_class(d, "graphAM")
        suppressWarnings(plot(d))
    }
}
