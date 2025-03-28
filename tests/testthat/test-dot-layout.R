test_that("dot layouts produce appropriate errors and output classes", {
  if (require(Rgraphviz, quietly = TRUE)) {
      specs <- mp_tmb_entire_library()
      no_mpflows <- c("lotka_volterra_competition",
                  "lotka_volterra_predator_prey",
                  "nfds")
      for (s in no_mpflows) {
          expect_error(dot_layout(specs[[s]]), "was spec defined")
      }
      plts = list()
      for (s in setdiff(names(specs), no_mpflows)) {
          plts[[s]] <- dot_layout(specs[[s]])
          expect_s4_class(plts[[s]], "graphAM")
      }
      if (interactive()) lapply(plts, plot)
  }
})
