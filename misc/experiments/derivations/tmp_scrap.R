self$vctr_pointer = function(vrbl, vctr_nm = "state"){
  vctr = self$def$settings()[[paste0(vctr_nm, "_variables")]]
  return(as.numeric(which(vrbl == vctr))-1)# -1 because c++ side uses zero based indexing.
}
self$from_states = function(){
  from_flows = self$flows_expanded()$from
  return(lapply(from_flows, self$state_pointer))
}
self$to_states = function(){
  to_flows = self$flows_expanded()$to
  return(lapply(to_flows, self$state_pointer))
}
# self$init_vals = function(){
#   #TODO: obtain all required values.
# }
# self$init_mats = function(){
#   #TODO: initiate all required variables. Including:
#   #1) state, flow, rate
#   #2) from, to, from_lngth, to_lngth
#   #3) flow variables
# }
self$standard_expressions = function(){
  flw_frml = MathExpressionFromStrings("state[from]*rate", list("state", "from", "rate"))
  symblc_flw_frml = do.call(flw_frml$symbolic$evaluate, list("state", "from", "rate"))
  
  stt_frml = MathExpressionFromStrings("state - group_sums(state, from, from_lngth)+group_sums(state, to, to_lngth)", list("state", "from", "from_lngth", "to", "to_lngth"))
  symblc_stt_frml = do.call(stt_frml$symbolic$evaluate, list("state", "from", "from_lngth", "to", "to_lngth"))
  
  return(list(list("flow", symblc_flw_frml), list("state", symblc_stt_frml)))
}
self$vctr_replacer = function(symbol, vrbls, vctr_nm = "state") {
  if(any(symbol == vrbls)){
    return(paste0(paste0(paste0(vctr_nm,"["), self$vctr_pointer(symbol, vctr_nm)), "]"))
  }
  else return(symbol)
}







nmbr_of_drvtns = length(derivation_list)
before = list()
during_pre_update = list()
during_post_update = list()
after = list()
for ( i in 1:nmbr_of_drvtns){
  # if(!is.null(derivation_list[[i]]$filter_partition)){
  #   vrbls = self$variables()$filter(derivation_list[[i]]$filter_names, .wrt = derivation_list[[i]]$filter_partition)
  # } 
  # else vrbls = self$variables()
  # if(!is.null(derivation_list[[i]]$group_partition)){
  #   nmbr_of_grps = length(derivation_list[[i]]$group_names)
  #   grp_vrbls = lapply(derivation_list[[i]]$group_names, vrbls$filter, .wrt = derivation_list[[i]]$group_partition, .comparison_function = all_consistent)
  # }
  # else {
  #   nmbr_of_grps = 1
  #   grp_vrbls = list(vrbls)
  # }
  # if(!is.null(derivation_list[[i]]$output_partition)){
  #   grp_outputs = lapply(derivation_list[[i]]$output_names, vrbls$filter, .wrt = derivation_list[[i]]$output_partition)
  # }
  # else {
  #   grp_outputs = lapply(derivation_list[[i]]$output_names, vrbls$filter, .wrt = self$def$settings()$required_partitions)
  # }
  # if(!is.null(derivation_list[[i]]$input_partition)){
  #   grp_inputs = derivation_list[[i]]$input_partition
  # }
  # else{
  #   grp_inputs = self$def$settings()$required_partitions
  # }
  
  nondots_flag = !is.null(derivation_list[[i]]$arguments) #Does the derivation have regular (i.e. not related to dots) arguments
  dots_flag = !is.null(derivation_list[[i]]$argument_dots) #Does the derivation have arguments to go in place of dots
  
  if(nondots_flag){
    fltrd_grp_vrbls = list()
    for(j in 1:nmbr_of_grps){
      fltrd_grp_vrbls = c(fltrd_grp_vrbls, grp_vrbls[[j]]$filter(derivation_list[[i]]$arguments, .wrt = grp_inputs))
    }
  }
  if(dots_flag){
    #dots_flag = TRUE
    fltrd_grp_vrbls_dts = list()
    for(j in 1:nmbr_of_grps){
      fltrd_grp_vrbls_dts = c(fltrd_grp_vrbls_dts, grp_vrbls[[j]]$filter(derivation_list[[i]]$argument_dots, .wrt = grp_inputs))
    }
  }
  grp_exprs_list = list()
  if(nondots_flag & dots_flag){
    frml = MathExpressionFromStrings(derivation_list[[i]]$expression, derivation_list[[i]]$arguments, include_dots = TRUE)
    for(j in 1:nmbr_of_grps){
      ordrd_grp_vrbls = fltrd_grp_vrbls[[j]]$filter_ordered(derivation_list[[i]]$arguments, .wrt = grp_inputs)
      symblc_input = c(ordrd_grp_vrbls$labels(), flted_grp_vrbls_dts[[j]]$labels())
      symblc_input = lapply(symblc_input, self$vctr_replacer, stt_vrbls)
      symblc_input = lapply(symblc_input, self$vctr_replacer, rt_vrbls, vctr_nm = "rate")
      symblc_frml = do.call(frml$symbolic$evaluate, symblc_input)
      symblc_output = grp_outputs[[j]]$labels()
      symblc_output = lapply(symblc_output, self$vctr_replacer, stt_vrbls)
      symblc_output = lapply(symblc_output, self$vctr_replacer, rt_vrbls, vctr_nm = "rate")
      grp_exprs_list = append(grp_exprs_list, list(list(symblc_output, symblc_frml)))
    }
  }
  else if(nondots_flag){
    frml = MathExpressionFromStrings(derivation_list[[i]]$expression, derivation_list[[i]]$arguments)
    for(j in 1:nmbr_of_grps){
      ordrd_grp_vrbls = fltrd_grp_vrbls[[j]]$filter_ordered(derivation_list[[i]]$arguments, .wrt = grp_inputs)
      symblc_input = ordrd_grp_vrbls$labels()
      symblc_input = lapply(symblc_input, self$vctr_replacer, stt_vrbls)
      symblc_input = lapply(symblc_input, self$vctr_replacer, rt_vrbls, vctr_nm = "rate")
      symblc_frml = do.call(frml$symbolic$evaluate, symblc_input)
      symblc_output = grp_outputs[[j]]$labels()
      symblc_output = lapply(symblc_output, self$vctr_replacer, stt_vrbls)
      symblc_output = lapply(symblc_output, self$vctr_replacer, rt_vrbls, vctr_nm = "rate")
      grp_exprs_list = append(grp_exprs_list, list(list(symblc_output, symblc_frml)))
    }
  }
  else if(dots_flag){
    frml = MathExpressionFromStrings(derivation_list[[i]]$expression, include_dots = TRUE)
    for(j in 1:nmbr_of_grps){
      symblc_input = fltrd_grp_vrbls_dts[[j]]$labels()
      symblc_input = lapply(symblc_input, self$vctr_replacer, stt_vrbls)
      symblc_input = lapply(symblc_input, self$vctr_replacer, rt_vrbls, vctr_nm = "rate")
      symblc_frml = do.call(frml$symbolic$evaluate, symblc_input)
      symblc_output = grp_outputs[[j]]$labels()
      symblc_output = lapply(symblc_output, self$vctr_replacer, stt_vrbls)
      symblc_output = lapply(symblc_output, self$vctr_replacer, rt_vrbls, vctr_nm = "rate")
      grp_exprs_list = append(grp_exprs_list, list(list(symblc_output, symblc_frml)))
    }
  }
  else{
    stop("Invalid derivations file? No arguments or argument dots.")
  }
  if(derivation_list[[i]]$simulation_phase == "before") before = append(before, list(grp_exprs_list))
  else if(derivation_list[[i]]$simulation_phase == "during_pre_update") during_pre_update = append(during_pre_update, list(grp_exprs_list))
  else if(derivation_list[[i]]$simulation_phase == "during_post_update") during_post_update = append(during_post_update, list(grp_exprs_list))
  else if(derivation_list[[i]]$simulation_phase == "after") after = append(after, list(grp_exprs_list))
  else stop("Unrecognized simulation phase")
}
usr_exprs = list(before, during_pre_update, during_post_update, after)
names(usr_exprs) = c("before", "during_pre_update", "during_post_update", "after")
return(usr_exprs)

# self$prep_expresions = function(){
#   #TODO: collect all expressions together into single list
#   #      Order is: 1. "before" expressions
#   #                2. "during_pre_update" expressions
#   #                3. assign variable rate expressions
#   #                4. standard expressions
#   #                5. "during_post_update" expressions
#   #                6. "after" expressions
#   # Note: 2 - 5 should be collected together as "during" expressions
# }
