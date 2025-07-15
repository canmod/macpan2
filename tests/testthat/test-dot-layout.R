test_that("dot layouts produce appropriate errors and output classes", {
  if (require(Rgraphviz, quietly = TRUE)) {
      specs <- mp_tmb_entire_library()
      n_flows = (specs
        |> lapply(mp_flow_frame, topological_sort = FALSE) 
        |> vapply(nrow, integer(1L))
      )
      no_mpflows <- which(n_flows == 0L) |> names()
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
