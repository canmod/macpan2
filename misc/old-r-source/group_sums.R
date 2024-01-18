## R-side versions of non-standard functions

# Group Sums
#
# Column vector containing the
# sums of groups of elements in `x`. The groups are
# determined by the integers in `f` and the order of
# the sums in the output is determined by these
# integers.
#
# @param x A numeric column vector.
# @param f A column vector the same length as `x`.
# containing integers between `0` and `n-1`.
# @param n Length of the output column vector.
#

group_sums = function(x, f, n) {
  ## Column vector containing the
  ## sums of groups of elements in `x`. The groups are
  ## determined by the integers in `f` and the order of
  ## the sums in the output is determined by these
  ## integers

  if (length(x) != length(f)) stop("invalid groups")
  y = numeric(n)
  y[] = 0.0
  f = as.integer(f)
  groups = sort(unique(f))
  if (any(groups > (n - 1))) stop("invalid groups")
  for (group in groups) {
    y[group + 1L] = sum(x[f == group])
  }
  y
}

