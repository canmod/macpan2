get_last_best_par = function(ad_fun) {
  best_par = ad_fun$env$last.par.best
  ranef_indices = ad_fun$env$random
  fixef_indices = setdiff(seq_along(best_par), ranef_indices)
  best_par[fixef_indices] |> rep_name("params")
}
