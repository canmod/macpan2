#' Matrix List
#'
#' Create a list of initial values for matrices used to define a compartmental
#' model in TMB.
#'
#' @param ... Named objects that can be coerced to numerical matrices.
#' @param .mats_to_save Character vector naming matrices to be saved at each
#' set in the simulation so that some calculations can make use of past value
#' (e.g. delayed effects) and/or to be able to retrieved the simulation
#' history after the simulation is complete.
#' @param .mats_to_return Character vector naming matrices to be returned
#' after the simulate is complete.
#' @param .dimnames Named list of \code{\link{dimnames}} for matrices that change
#' their dimensions over the simulation steps. These names correspond to the
#' names of the matrices. The output of the simulations will try their best
#' to honor these names, but if the shape of the matrix is too inconsistent
#' with the \code{\link{dimnames}} then numerical indices will be used instead.
#' For matrices that do not change their dimensions, set \code{\link{dimnames}}
#' by adding \code{\link{dimnames}} to the matrices passed to \code{...}.
#' @param .structure_labels An optional object for obtaining labels of
#' elements of special vectors and matrices. Note that this
#' is an advanced technique.
#'
#' @return Object of class \code{MatsList} with the following methods.
#'
#' ## Methods
#'
#' * `$data_arg()`: Return the following components of the data structure
#' to pass to C++.
#'     * `mats` -- Unnamed list of numeric matrices.
#'     * `mats_save_hist` -- Boolean vector identifying which matrices should
#'     have their history saved.
#'     * `mats_return` -- Boolean vector identifying which matrices should be
#'     returned after a simulation.
#' * `$mat_dims()`: Return a data frame giving the numbers of rows and columns
#' of each matrix in the list.
#' * `$add_mats(...)`: Add matrices to the list and return a new
#' regenerated \code{MatsList} object.
#'
#' @noRd
MatsList = function(...
    , .mats_to_save = character(0L)
    , .mats_to_return = character(0L)
    , .dimnames = list()
    , .structure_labels = NullLabels()
    , .dummy = "dummy"
  ) {
  self = Base()

  ## Args
  ## TODO: these shouldn't be private but we have a problem.
  ## the original sin was putting dots in front of arguments that
  ## correspond to public Arg fields. this will require a breaking
  ## change to fix where we remove dots from all arguments that should
  ## actually be stored publicly. i initially was implicitly thinking
  ## of arguments starting with dots as arguments that aren't really
  ## part of the method signature being depended on, but this is silly
  ## in hindsight ... what does that even mean?
  self$.mats_to_save = .mats_to_save
  self$.mats_to_return = .mats_to_return
  self$.dimnames = .dimnames
  self$.structure_labels = .structure_labels
  self$.dummy = .dummy

  # Static
  l = list(...)

  ## FIXME: still required because int_vecs and not literals are required on
  ## the lhs so we still need the dummy ~ assign... construction for those
  ## cases
  if (!is.null(.dummy)) {
    if (!.dummy %in% names(l)) {
      l = c(l, setNames(list(empty_matrix), .dummy))
    }
  }

  self$.initial_mats = lapply(l, as.matrix)
  self$all_matrices = function() self$.initial_mats

  if (length(self$.initial_mats) == 0L) names(self$.initial_mats) = character()

  self$mats_save_hist = function() names(self$.initial_mats) %in% self$.mats_to_save
  self$mats_return = function() names(self$.initial_mats) %in% self$.mats_to_return

  ## Standard methods
  self$get = function(variable_name) {
    i = which(self$.names() == valid$char1$assert(variable_name))
    if (length(i) == 1L) {
      return(self$.mats()[[i]])
    } else {
      i = which(variable_name == self$.structure_labels$state())
      if (length(i) == 1L) {
        return(self$get("state")[i])
      }
      i = which(variable_name == self$.structure_labels$flow())
      if (length(i) == 1L) {
        return(self$get("flow")[i])
      }
    }
    stop(
      "\nNo variable called ", variable_name, " in the list:\n",
      paste0(
        c(
          self$.names(),
          self$.structure_labels$state(),
          self$.structure_labels$flow()
        ),
        collapse = "; "
      )
    )
  }
  self$.names = function() names(self$.initial_mats)
  self$.mats = function() unname(self$.initial_mats)
  dimnames_handle_nulls = function(x) {
    if (is.null(dimnames(x))) return(NULL)
    if (is.null(rownames(x))) rownames(x) = rep("", nrow(x))
    if (is.null(colnames(x))) colnames(x) = rep("", ncol(x))
    dimnames(x)
  }
  not_null = function(x) !is.null(x)
  dn = lapply(self$.initial_mats, dimnames_handle_nulls)
  for (mat_nm in names(.dimnames)) dn[[mat_nm]] = .dimnames[[mat_nm]]
  self$.dimnames = Filter(not_null, dn)

  self$.dim = setNames(
    lapply(self$.mats(), dim),
    self$.names()
  )
  self$.nrow = vapply(self$.mats(), nrow, integer(1L), USE.NAMES = FALSE)
  self$.ncol = vapply(self$.mats(), ncol, integer(1L), USE.NAMES = FALSE)
  self$mat_dims = function() {
    data.frame(mat = self$.names(), nrow = self$.nrow, ncol = self$.ncol)
  }

  self$dimnames = function() self$.dimnames
  self$rownames = function() lapply(self$dimnames(), getElement, 1L)
  self$colnames = function() lapply(self$dimnames(), getElement, 2L)
  self$data_arg = function() {
    r = list(
      mats = self$.mats(),
      mats_save_hist = self$mats_save_hist(),
      mats_return = self$mats_return()
    )
    valid$mats_arg$assert(r)
  }

  ## add _new_ matrices -- error if a matrix with the
  ## same name already exists
  self$add_mats = function(...
    , .mats_to_save = character(0L)
    , .mats_to_return = character(0L)
    , .dimnames = list()
  ) {
    args = c(self$.initial_mats, list(...))
    dups = duplicated(names(args))
    if (any(dups)) {
      stop(
        "\nThe following matrices were added, but already existed:\n",
        paste0(names(args)[dups], collapse = ", ")
        ## TODO: fill in what to do about it
      )
    }
    args$.mats_to_save = union(self$.mats_to_save, .mats_to_save)
    args$.mats_to_return = union(self$.mats_to_return, .mats_to_return)
    args$.dimnames = c(self$.dimnames, .dimnames)
    args$.structure_labels = self$.structure_labels
    do.call(MatsList, args)
  }

  ## add new matrices or update existing matrices as the
  ## case may be -- no error or warning if you happen to
  ## overwrite an existing matrix
  self$update_mats = function(...
    , .mats_to_save = character(0L)
    , .mats_to_return = character(0L)
    , .dimnames = list()
  ) {
    args = self$.initial_mats
    new_args = list(...)
    args[names(new_args)] = new_args
    args$.mats_to_save = union(self$.mats_to_save, .mats_to_save)
    args$.mats_to_return = union(self$.mats_to_return, .mats_to_return)
    args$.dimnames = self$.dimnames
    args$.dimnames[names(.dimnames)] = .dimnames
    args$.structure_labels = self$.structure_labels
    do.call(MatsList, args)
  }
  return_object(self, "MatsList")
}

#' @export
names.MatsList = function(x) x$.names()
