
mp_param_id = function(model, mat, row = 0, col = 0, type = c("fixed", "random")) {
  if (length(mat) != 1L) stop("Can only lookup one matrix at a time.")
  type = match.arg(type)
  pization = mp_parameterization(model, type)
  pization = pization[pization$mat == mat, , drop = FALSE]
  if (nrow(pization) == 0L) stop("Cannot find matrix ", mat)
  if (length(row) != 1L) stop("Can only lookup one row at a time.")
  pization = pization[pization$row == row, , drop = FALSE]
  if (length(col) != 1L) stop("Can only lookup one column at a time.")
  pization = pization[pization$col == col, , drop = FALSE]
  if (nrow(pization) == 1L) return(pization$par_id)
  stop("Cannot find parameter for row ", row, " in column ", col, " of matrix ", mat)
}
get_param_id_util = function(model, param) {
  if (is.character(param)) return(mp_param_id(model, param))
  if (length(param) != 1L) stop("Can only retrieve one parameter at a time.")
  if (!is.numeric(param)) stop("Can only retrieve parameters by name or by ID.")
  return(as.integer(round(param)))
}

#' TMB Likelihood Profiling
#' 
#' Use \code{\link{TMB::tmbprofile}} to compute the profile likelihood
#' of a calibrator produced using \code{\link{mp_tmb_calibrator}}.
#' 
#' @param model A TMB model probably produced using
#' \code{\link{mp_tmb_calibrator}}.
#' @param param The name of a fixed effect parameter set through the `par`
#' argument of the call to \code{\link{mp_tmb_calibrator}} used to create
#' `model`.
#' @param ... Arguments to pass to \code{\link{TMB::tmbprofile}}.
#' 
#' @return The output of \code{\link{TMB::tmbprofile}}.
#' @export
mp_tmb_profile = function(model, param, ...) UseMethod("mp_tmb_profile")

#' @export
mp_tmb_profile.TMBCalibrator = function(model, param, ...) {
  mp_tmb_profile(model$simulator, param, ...)
}

#' @export
mp_tmb_profile.TMBSimulator = function(model, param, ...) {
  tmb_profile_args = list(...)
  if (any(c("obj", "name") %in% names(tmb_profile_args))) {
    stop("Cannot directly pass a TMB object or parameter name to TMB::tmb_profile")
  }
  param_id = get_param_id_util(model, param)
  ps = mp_parameterization(model)
  i = which(ps$par_id == param_id)
  TMB::tmbprofile(mp_tmb(model), i, ...)
}
make_liksurf <- function(simulator
      , param_x, param_y
      , param_x_seq, param_y_seq
    ) {
  
  param_x_id = get_param_id_util(simulator, param_x)
  param_y_id = get_param_id_util(simulator, param_y)
  pization = mp_parameterization(simulator)
  x = which(pization$par_id == param_x_id)
  y = which(pization$par_id == param_y_id)
  p = pization$current
  lik_surf = expand.grid(x = param_x_seq, y = param_y_seq)
  p_surf = matrix(p, nrow = nrow(lik_surf), ncol = length(p), byrow = TRUE)
  p_surf[, c(x, y)] = as.matrix(lik_surf)
  lik_surf = as.data.frame(lik_surf)
  gr = apply(p_surf, 1, simulator$gradient) |> t()
  lik_surf$z = apply(p_surf, 1, simulator$objective)
  gr = 0.1 * gr / max(abs(gr))
  lik_surf$gx = gr[, x]
  lik_surf$gy = gr[, y]
  return(lik_surf)
}
plot_liksurf <- function(dd, arrows = TRUE, contours = TRUE,
                    arrow_len = 0.05,
                    arrow_thin = 5) {
    require(ggplot2)
    gg0 <- (ggplot(dd, aes(x, y))
        + geom_tile(aes(fill = z))
        + theme_bw()
        + scale_x_continuous(expand = c(0,0), limits = range(dd$x))
        + scale_y_continuous(expand = c(0,0), limits = range(dd$y))
        + scale_fill_continuous(trans = "log10", type = "viridis")

    )
    if (contours) {
        gg0 <- gg0 + geom_contour(aes(z = z), colour = "grey")
    }
    if (arrows) {
        gg0 <- gg0 + geom_segment(
                         data = dd[seq(nrow(dd)) %% arrow_thin == 0 , ],
                         aes(xend = x - gx, yend = y - gy), 
                         arrow = arrow(length = unit(arrow_len, "inches")), 
                         colour = 'white'
                     )
    }
    gg0 = gg0 + geom_point(
        aes(x, y)
      , data = filter(dd, z == min(z))
      , colour = "red"
    )
    gg0
}

