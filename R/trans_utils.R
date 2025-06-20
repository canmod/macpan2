backtrans = function(tab, vars, matrix_name, sd_name, value_name) {
  # regex matching log or logit transformed model coefficient/parameter names 
  # including time varying parameters and distributional parameters
  transformed_coef_name = "(^time_var_|^distr_params_|^)(log(it)?_)"
  
  cap_grp_names = data.frame(automatic_prefix = character() # might not need this group now
    , transform = character()
    , unnecessary_grp = character() # should remove this group
  )
  cap_grp = strcapture(transformed_coef_name, tab[[matrix_name]], cap_grp_names, perl = TRUE)
  cap_grp[is.na(cap_grp)] <- ""
  prefix = cap_grp[["transform"]]
  tab <- split(tab, prefix)
  for (ptype in setdiff(names(tab), "")) {
    link <- make.link(gsub("_", "", ptype))
    if (!is.null(sd_name)) {
      tab[[ptype]][[sd_name]] = 
        link$mu.eta(tab[[ptype]][[value_name]]) * tab[[ptype]][[sd_name]]
    }
    tab[[ptype]][vars] = lapply(tab[[ptype]][vars],link$linkinv)
    # restore original coefficient name
    orig_coef_name = gsub(ptype, "", tab[[ptype]][[matrix_name]])
    tab[[ptype]][[matrix_name]] = gsub(ptype, "", tab[[ptype]][[matrix_name]])
  }
  return(bind_rows(tab))
}
