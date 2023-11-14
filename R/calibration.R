TMBOptimizer = function(simulator) {
  self = Base()
  self$simulator = simulator
  arg_updater = function(opt_method_nm, opt_func, arg_nms_from_object) {
    args = formals(opt_func)
    formals(self[[opt_method_nm]]) = args[!(names(args) %in% arg_nms_from_object)]
  }
  wrap = function(opt_method_nm, opt_func, ...) {
    arg_mappings = list(...)
    force(arg_mappings)
    force(opt_func)
    force(opt_method_nm)

    self[[opt_method_nm]] = function() {

      ## TMB doesn't compute hessians for models with random effects
      random_effects_in_model = !is.null(self$simulator$tmb_model$random_arg())
      hessian_in_args = !is.null(arg_mappings[["he"]])
      if (hessian_in_args & random_effects_in_model) {
        arg_mappings = arg_mappings[!names(arg_mappings) %in% "he"]
      }

      ad_fun = self$simulator$ad_fun()
      ad_fun$fn()  ## work out certain starting value issues -- is this necessary?
      args = list()
      mc = match.call()
      if (length(mc) != 1L) {
        args = setNames(
          lapply(2L:length(mc), function(i) mc[[i]]),
          names(mc)[-1L]
        )
      }
      args_from_object = setNames(
        ad_fun[names(arg_mappings)],
        unname(arg_mappings)
      )
      opt_obj = do.call(opt_func, c(args_from_object, args))
      self$simulator$optimization_history$save(opt_obj)
      self$simulator$cache$sdreport$invalidate() ## now that we have optimized again, we need to invalidate the now out-of-date sdreport
      ad_fun$fn(opt_obj$par) ## probably this should be last.par.best
      opt_obj
    }
    arg_updater(opt_method_nm, opt_func, unname(arg_mappings))
  }
  wrap(
      "optim", stats::optim
    , par = "par", fn = "fn", gr = "gr"
  )
  wrap(
      "nlminb", stats::nlminb
    , par = "start", fn = "objective", gr = "gradient", he = "hessian"
  )

  return_object(self, "TMBOptimizer")
}

TMBCurrentParams = function(simulator) { ## TMBSimulator
  self = Base()
  self$simulator = simulator

  self$n_params = function() {
    self$simulator$tmb_model$params$data_frame() |> nrow()
  }
  self$n_random = function() {
    self$simulator$tmb_model$random$data_frame() |> nrow()
  }
  self$params_vector = function() {
    if (self$n_params() == 0L) return(numeric())
    self$simulator$ad_fun()$env$parList()$params
  }
  self$random_vector = function() {
    if (self$n_random() == 0L) return(numeric())
    self$simulator$objective(self$params_vector())
    self$simulator$ad_fun()$env$parList()$random
  }
  self$params_frame = function() {
    self$simulator$tmb_model$params$data_frame(current = self$params_vector())
  }
  self$random_frame = function() {
    self$simulator$tmb_model$random$data_frame(current = self$random_vector())
  }
  return_object(self, "OptimizedParams")
}

TMBOptimizationHistory = function(simulator) {
  self = Base()
  self$simulator = simulator
  self$.history = list()
  self$get = function() self$.history
  self$save = function(opt_obj) self$.history = append(self$.history, list(opt_obj))
  return_object(self, "TMBOptimizationHistory")
}


add_slot <- function(sim, x, value = empty_matrix, save_x = FALSE, return_x = FALSE) {
    args <- list()
    argstr <- list()
    if (save_x) {
        args <- c(args, list(.mats_to_save = x))
        argstr <- append(argstr, sprintf(".mats_to_save = %s", x))
    }
    if (return_x) {
        args <- c(args, list(.mats_to_return = x))
        argstr <- append(argstr, sprintf(".mats_to_return = %s", x))
    }
    args <- c(list(value), args)
    argstr <- append(argstr, sprintf("%s = %s", x, deparse(substitute(value))))
    names(args)[1] <- x
    do.call(sim$add$matrices, args)
    argstr <- sprintf("sim$add$matrices(%s)",
                      do.call(paste, c(list(unlist(argstr)), list(collapse = ", "))))
    return(invisible(argstr))
}

## Note: example is failing with bf1de7f99
## with: Error in valid$consistency_params_mats$check(self$model) :
## optimization parameters are not consistent with matrices
## but validity could not be checked because:
## Error in if (any(!valid_pars)) { : missing value where TRUE/FALSE needed

##' Add calibration information to a simulator
##'
##' ## To do/FIXME
##'  * see hacks for getting simulation variables, state variables
##' * modularize?
##' * switch for enabling a differenced/incidence class (add a flow/accumulator var to model; add a differencing step)?
##' * allow setting clamp tolerance? allow specified list of variables to clamp rather than all or nothing
##' * rename and move into macpan2
##' * document that 'log-likelihood' means -1*(loss function) (e.g. for SSQ, chi-squared fits)
##' @param sim A \code{macpan2} simulator (i.e., a \code{TMBSimulator} object).
##' @param params a list of parameters with default/starting values.
##' @param transforms TODO.
##' @param data A data frame containing data to add (i.e., observed variables that will be compared with simulations). If the data frame contains a column called "time" or "date" (any capitalization), it will be used.
##' @param start_time A time or date, overriding first time in data; set to 1 otherwise.
##' @param end_time A time or date, overriding last time in data; set to number of time steps otherwise.
##' @param exprs A list of expressions to add.
##' @param debug (logical) Print debugging information?
##' @param clamp_vars (logical) Force state variables to be positive in likelihood expression?
##' @return This function modifies the simulator object **in place**. It also returns (invisibly) a character vector of the lower-level operations it performs.
##' @export
#' @examples
#' ## it's convenient to have a function that sets up a fresh simulation
#' ## (since adding already-existing components to a simulation object throws an error)
#' library(dplyr)
#' setup_sim <- function() {
#'   m <- Compartmental(system.file("starter_models", "sir", package = "macpan2"))
#'   sim <- m$simulators$tmb(
#'    time_steps = 100,
#'    state = c(S = 99, I = 1, R = 0),
#'    flow = c(foi = NA, gamma = 0.1),
#'    beta = 0.2,
#'    N = empty_matrix
#'  )
#' }
#' sim <- setup_sim()
#' if (require(outbreaks)) {
#'      I_obs <- influenza_england_1978_school[["in_bed"]]
#' } else {
#'   set.seed(101)
#'   I_obs <- (sim$report(.phases = "during")
#'      |> filter(row == "I")
#'     |> mutate(obs = rnbinom(100, mu = value, size = 2))
#'     |> pull(obs)
#'   )
#' }
#' m1 <- mk_calibrate(sim,
#'     data = data.frame(I_obs),
#'     params = list(beta = 0.2, I_sd = 1),
#'     transforms = list(beta = "log", I_sd = "log"),
#'     exprs = list(log_lik ~ dnorm(I_obs, I, I_sd)),
#' )
#' cat(m1, sep = "\n")
#' sim$optimize$nlminb()
#' sim <- setup_sim()  ## refresh
#' mk_calibrate(sim,
#'     data = data.frame(I_obs),
#'     params = list(beta = 0.2, gamma = 0.05),
#'     transforms = list(beta = "log", gamma = "log"),
#'     exprs = list(log_lik ~ dpois(I_obs, I))
#' )
#' sim$optimize$nlminb()
#' ## warning about NA/NaN function evaluation is probably harmless ...
mk_calibrate <- function(sim,
                         params = list(),
                         transforms = list(),
                         data = NULL,
                         start_time = NULL,
                         end_time = NULL,
                         exprs = list(),
                         debug = FALSE,
                         clamp_vars = FALSE) {
    ## how do I get these programmatically from sim?
    ## is there a better/easier way to get state names??
    ##  these are present in 'Compartmental' objects;
    ## easy to get with sim$labels$state().  Should they be carried along
    ## somehow?
    data_sub <- deparse(substitute(data))
    desc <- list()

    ## if only one data frame is passed, assume that it is
    ## referencing state variables to be fitted. in the future
    ## we will allow a list of data frames, with one per vector.
    if (inherits(data, "data.frame")) data = list(state = data)

    logit <- plogis  ## ugh; better way to handle transformations?

    ## for testing!

    cap <- function(s) paste0(toupper(substring(s, 1, 1)), substring(s, 2))

    ## add log-likelihood slot
    ## add comments ???
    desc <- append(desc, "# add log_lik matrix (empty)")

    desc <- append(desc, add_slot(sim, "log_lik"))
    ## added_vars <- character(0)

    ## add data
    if (!is.null(data)) {
        for (vctr in names(data)) {

          if (!is.data.frame(data[[vctr]])) stop("'data' argument must be a data frame or list of data frames")
          timecol <- grep("([Tt]ime|[Dd]ate)", names(data[[vctr]]))
          if (length(timecol) > 1) stop("multiple time/date columns detected")
          if (length(timecol) == 0 && (!is.null(start_time) || !is.null(end_time))) {
              stop("if start_time or end_time are specified, data must include a time/date column")
          }
          if (length(timecol) > 0) {
              timevec <- data[[vctr]][[timecol]]
              if (length(unique(timevec)) < length(timevec)) stop("time steps in data must be unique")
              if (any(diff(timevec) < 0)) stop("data must be sorted by time")
              if (any(diff(timevec) < 1)) stop("time steps must be equal to 1")
              if (is.null(start_time)) start_time <- timevec[1]
              if (is.null(end_time)) end_time <- tail(timevec, 1)
              ts <- as.numeric(end_time - start_time) + 1 ## check on +1/edge effect issues?
              ## generate time index that matches up with data frame
              timevec <- as.integer(timevec - start_time + 1)
              data[[vctr]] <- data[[vctr]][, -timecol, drop = FALSE]
          } else {
              ts <- nrow(data[[vctr]])
              timevec <- seq(ts)
          }
          irreg_time <- any(is.na(data[[vctr]])) || any(diff(timevec) > 1)
          cur_ts <-  sim$tmb_model$time_steps$time_steps
          if (ts != cur_ts) {
              if (debug) cat(sprintf("resetting number of time steps (%d -> %d)\n",
                                     cur_ts, nrow(data[[vctr]])))
              sim$replace$time_steps(nrow(data[[vctr]]))
              desc <- append(desc, sprintf("sim$replace$time_steps(%d)", nrow(data[[vctr]])))
          }
          for (nm in names(data[[vctr]])) {
              if (debug) cat(sprintf("add data matrix: %s\n", nm))
              do.call(sim$add$matrices, data[[vctr]][nm])
              desc <- append(desc, sprintf("sim$add$matrices(%s[['%s']]", data_sub, nm))
          }
        }
    }

    ## ?? needs to go before expressions get added?
    for (p in setdiff(names(params), sim$matrix_names())) {
        ## add params if not already in model (e.g. dispersion parameter)
        if (debug) cat("add param (scalar placeholder value): ", p, "\n")
        desc <- append(desc, add_slot(sim, p, 1.0))
    }

    ## add _sim analogues for state variables referred to in expressions;
    ## substitute rbind_time(*_sim) in expressions
    for (i in seq_along(exprs)) {
        ee <- exprs[[i]]
        if (debug) cat("process expression: ", deparse(ee), "\n")
        all_vars <- all.vars(ee)
        ## create a placeholder
        ## FIXME: change logic to allow time indices for derived variables as well as state variables?
        ## should have a separate loop that checks for 'specials' (dnorm, dpois, etc.) and uses those arguments
        ## to match sim vs obs for time-index-matching
        ## right now this *won't* work for the combination of derived variables (e.g. sum of hospital classes, incidences) + irregular time
        for (vctr in names(data)) {
          for (v in intersect(all_vars, labels(sim)[[vctr]])) {
              ph <- paste0(v, "_sim")
              if (debug) cat("add (empty matrix): ", ph, "\n")
              desc <- append(desc, add_slot(sim, ph, save_x = TRUE))
              newexpr <- reformulate(v, response = ph, env = emptyenv())
              if (debug) cat("add ", deparse(newexpr), "\n")
              sim$insert$expressions(
                             newexpr,
                             .phase = "during",
                             .at = Inf)
              desc <- append(desc, sprintf("sim$insert$expressions(%s, .phase = 'during', .at = Inf", deparse(newexpr)))
              if (irreg_time) {
                  ## could skip indices for any data elements that are regular?
                  ## need to match observed var with sim var explicitly
                  ## for now let's hope there's a one-to-one match between expressions in the state vector
                  ## and observed data ...
                  data_var <- intersect(all_vars, names(data[[vctr]]))
                  t_ind <- timevec[!is.na(data[[vctr]][[data_var]])]
                  t_indnm <- paste0(v, "_tind")
                  do.call(sim$add$matrices, setNames(list(t_ind), t_indnm))
                  if (debug) cat(sprintf("add data matrix: %s", t_indnm))
                  desc <- append(desc, sprintf("%s <- %s", t_indnm, deparse(t_ind)))
                  desc <- append(desc, sprintf("sim$add$matrices(%s)", t_indnm))
                  bind_var <- sprintf("rbind_time(%s, %s)", ph, t_indnm)
              } else {
                  bind_var <- sprintf("rbind_time(%s)", ph)
              }
              ## convert to parsed expression, then get rid of expression()
              newsym <- parse(text = bind_var)[[1]]
              exprs[[i]] <- do.call(substitute,
                                    list(ee, setNames(list(newsym), v)))
              ## substitute clamp(*_sim) [INSIDE] rbind_time()
              if (clamp_vars) {
                  clamp_var <- sprintf("clamp(%s)", ph)
                  newsym <- parse(text = clamp_var)[[1]]
                  exprs[[i]] <- do.call(substitute,
                                        list(exprs[[i]], setNames(list(newsym), ph)))
              }
          }
        }
        if (debug) cat("add ", deparse(exprs[[i]]), "\n")
        sim$insert$expressions(exprs[[i]], .phase = "after")
        desc <- append(desc, sprintf("sim$insert$expressions(%s, .phase = 'after', .at = Inf", deparse(exprs[[i]])))
    }

    ## modify names for transforms, apply transform to specified values
    pp = function(...) {
      l = list(...)
      l = l[!vapply(l, is.null, logical(1L))]
      if (length(l) == 1L) return(l[[1L]])
      if (length(l) == 0L) return("")
      do.call(paste, c(l, sep = "_"))
    }
    trans_params = character()
    for (m in names(params)) {
      tr_params = names(transforms[[m]])
      if (!is.null(tr_params)) {
        trans_params = append(trans_params,
          paste(
            unname(transforms[[m]]),
            names(params[[m]][tr_params]),
            sep = "_"
          )
        )
      } else {
        trans_params = append(trans_params,
          paste(transforms[[m]], names(params[m]), sep = "_")
        )
      }
    }

    # trpars <- transforms != ""
    # trp <- params[trpars]
    # names(trp) <- paste(transforms[trpars], names(trp), sep = "_")
    ## trp <- Map(function(x, tr) get(tolower(tr))(x), trp, transforms[trpars])
    pframe <- data.frame(mat = trans_params, row = 0, col = 0, default = unlist(trp))
    rownames(pframe) <- NULL ## cosmetic

    desc <- append(desc, sprintf("pframe <- data.frame(mat = %s, row = 0, col = 0, default = %s)",
                         deparse(names(trp)), deparse(unname(unlist(trp)))))
    if (debug) {
        cat("param_frame:\n")
        print(pframe)
    }

    ## FIXME: handle no transformation case
    if (debug) cat("adding transformations\n")
    add_trans <- function(tr, nm) {
        if (debug) cat("add transformation: ", cap(tr)," ", nm, "\n")
        sim$add$transformations(get(cap(tr))(nm))
        ## does package checking complain about <<- ? could use assign(..., parent.frame())
        desc <<- append(desc, sprintf("sim$add$transformations(%s(\"%s\"))", cap(tr), nm))
    }

    ## add transformations
    Map(add_trans, transforms[trpars], names(params)[trpars])

    ## now add param frame (does order matter??)
    ##
    ## FIXME: refresh_param not found
    sim$replace$params_frame(pframe)
    desc <- append(desc, sprintf("sim$replace$params_frame(pframe)"))

    if (debug) cat("set obj_fn to -sum(log_lik)\n")
    sim$replace$obj_fn(~ -sum(log_lik))
    desc <- append(desc, "sim$replace$obj_fn(~ -sum(log_lik))")
    ## add transformations
    ## add parameters
    ## for now, assume all parameters are scalar?

    ## everything is done as a side effect (mutating state of sim)
    return(invisible(unlist(desc)))

}


#' Pick out obs/location pairs from terms involving probability distributions
#' find_obs_pairs(~ dnorm(a, b, c) + dpois(d, e))
#' find_obs_pairs(stuff ~ other_stuff + more_stuff)
.known_dist <- c("dnorm", "dpois", "dgamma", "dlnorm", "dnbinom")
find_obs_pairs <- function(form, specials = .known_dist, top = TRUE) {
    if (is.symbol(form) || length(form) == 1) return(NULL)
    if (deparse(form[[1]]) %in% specials) {
        return(list(deparse(form[[2]]), deparse(form[[3]])))
    }
    res <- lapply(form[-1], find_obs_pairs, top = FALSE)
    ## drop NULL elements
    res <- res[!vapply(res, is.null, logical(1))]
    if (!top) res else res[[1]]
}
