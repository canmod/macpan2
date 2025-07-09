get_last_best_par = function(ad_fun) {
  best_par = ad_fun$env$last.par.best
  ranef_indices = ad_fun$env$random
  fixef_indices = setdiff(seq_along(best_par), ranef_indices)
  best_par[fixef_indices] |> rep_name("params")
}

singular = function(mat, tol = 1e-6) {
  # initial idea: base::qr(mat)$rank < ncol(mat)
  
  ## from MASS::mvrnorm, which is what we use covariance matrices for
  eS <- eigen(mat, symmetric = TRUE)
  ev <- eS$values
  is_singular = !all(ev >= -tol * abs(ev[1L]))
  return(is_singular)
}
