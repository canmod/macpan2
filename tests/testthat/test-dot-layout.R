# test_that("dot layouts produce appropriate errors and output classes", {
#     specs <- mp_tmb_entire_library()
#     n_flows = (specs
#       |> lapply(mp_flow_frame, topological_sort = FALSE) 
#       |> vapply(nrow, integer(1L))
#     )
#     no_mpflows <- which(n_flows == 0L) |> names()
#     for (s in no_mpflows) {
#         expect_error(
#             mp_dot_layout(specs[[s]], action = "layout")
#           , "was spec defined"
#         )
#     }
#     plts = list()
#     for (s in setdiff(names(specs), no_mpflows)) {
#         plts[[s]] <- mp_dot_layout(specs[[s]], action = "layout")
#         expect_s4_class(plts[[s]], "graphAM")
#     }
#     if (interactive()) for (plt in plts) renderGraph(plt)
#     
#     ## copied from https://testthat.r-lib.org/reference/expect_snapshot_file.html
#     save_png <- function(code, width = 400, height = 400) {
#       path <- tempfile(fileext = ".png")
#       png(path, width = width, height = height)
#       on.exit(dev.off())
#       code
#     
#       path
#     }
#     expect_snapshot_plot <- function(name, code) {
#       name <- paste0(name, ".png")
#     
#       # Announce the file before touching `code`. This way, if `code`
#       # unexpectedly fails or skips, testthat will not auto-delete the
#       # corresponding snapshot file.
#       announce_snapshot_file(name = name)
#     
#       path <- save_png(code)
#       expect_snapshot_file(path, name)
#     }
#     
#     macpan_base <- mp_official_library("macpan_base")
#     seir <- mp_official_library("seir")
#     shiver <- mp_official_library("shiver")
#     
#     graph3 = mp_dot_layout(macpan_base, action = "define")
#     graph2 = mp_dot_layout(seir, action = "layout")
#     expect_snapshot_plot("shiver1", {graph1 <- mp_dot_layout(shiver, action = "render")})
#     expect_snapshot_plot("base", {graph3 |> layoutGraph() |> renderGraph()})
#     expect_snapshot_plot("seir", {graph2 |> renderGraph()})
#     expect_snapshot_plot("shiver2", {graph1 |> renderGraph()})
#     
#     expect_s4_class(graph1, "graphAM")
#     expect_s4_class(graph2, "graphAM")
#     expect_s4_class(graph3, "graphAM")
# })
