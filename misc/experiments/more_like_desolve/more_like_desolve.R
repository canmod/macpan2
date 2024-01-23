library(macpan2)
library(ggplot2)
library(dplyr)
library(tidyr)

## start with an indexed state vector
state = mp_cartesian(
  mp_index(
    Epi = c("S", "I"   , "I"    , "R"),
    Symp = c("", "mild", "severe", "")
  ),
  mp_index(Loc = c("east", "west")),
  mp_index(Age = c("young", "old"))
)

beta = mp_symmetric(
  mp_lookup(state, "I") |> mp_lookup("Symp.Age"),
  c("SympS", "AgeS")
) |> mp_cartesian(mp_lookup(state, "Loc"))

mp_lookup(beta, "mild")

mp_subset_list(state, young_before_recovery = list(Epi = c("S", "I"), Age = "young"))
mp_custom_slices(state, "Epi", Epi.Age = "I.young", unpack = "yes")



permutations(3)
lapply(increasing_int_seq(5, 3), apply_permutations)


apply_k_int_permutations(5, 3)
apply_k_incr_int_permutations(5, 3)

matrix(letters[permutations(3)],ncol=3)



.



ModelStructure = function() {}




method_parser = macpan2:::method_parser






Mutate = function(expr) {
  deparsed_expr = deparse(substitute(expr))
  expr = try(force(expr), silent = TRUE)
  if (inherits(expr, "formula")) {
    expr = macpan2:::rhs_expr(expr)
  } else if (is.character(expr) & !inherits(expr, "try-error")) {
    expr = macpan2:::rhs_expr(macpan2:::one_sided(expr))
  } else {
    expr = deparsed_expr
  }
  self = Base()
  self$expr = expr
  return_object(self, "Mutate")
}

Transition = function(from, to, rate) {
  deparsed_rate = deparse(substitute(rate))
  rate = try(force(rate), silent = TRUE)
  if (inherits(rate, "formula")) {
    rate = macpan2:::rhs_expr(rate)
  } else if (is.character(rate) & !inherits(rate, "try-error")) {
    rate = macpan2:::rhs_expr(macpan2:::one_sided(rate))
  } else {
    rate = deparsed_rate
  }
  self = Base()
  self$type = macpan2:::lhs_char(rate)
  self$from = from
  self$to = to
  self$rate = rate
  self$ledger = function(indexes, by, update_position_vector_names = FALSE) {
    from = "from"
    to = "to"
    if (update_position_vector_names) {
      indexes = setNames(indexes, sprintf("%s_%s", names(indexes), self$type))
      by_names = strsplit(names(by), ".", fixed = TRUE)
      all_by_names = unlist(by_names, use.names = FALSE) |> unique()
      all_by_names = setNames(
        sprintf("%s_%s", all_by_names, self$type),
        all_by_names
      )
      #all_by_names["from"] = "from"
      #all_by_names["to"] = "to"
      for (i in seq_along(by_names)) {
        by_names[[i]] = all_by_names[by_names[[i]]] |> paste0(collapse = ".")
      }
      names(by) = by_names
      from = all_by_names["from"]
      to = all_by_names["to"]
    }
    args = (
         setNames(list(self$from, self$to), c(from, to))
      |> c(indexes)
      |> c(list(by = by))
    )
    do.call(mp_join, args)
  }
  self$expr = function(indexed_vars = character(0L), state_vector_name = "state") {
    ## this is nowhere near done -- it needs to know the name of the
    ## reference index/vector used in each case
    from_to = c("from", "to")
    from_to = sprintf("%s ~ state[%s_%s]", from_to, from_to, self$type)
    updaters = ("%s ~ %s[%s_%s]"
      |> sprintf(indexed_vars, indexed_vars, indexed_vars, self$type)
      |> c(from_to)
      |> lapply(as.formula)
    )
    (self$type
      |> macpan2:::two_sided(self$rate)
      |> macpan2:::update_formula(updaters)
    )
  }
  return_object(self, "Transition")
}
mp_transition = Transition

ff = function(formula_list) {
  input = character()
  derived = character()

  for (f in formula_list) {
    left = macpan2:::formula_components(f, "left")$variables
    right = macpan2:::formula_components(f, "right")$variables

    input = append(input, right[!right %in% derived]) |> unique()
    derived = append(derived, left[!left %in% input]) |> unique()
  }
  nlist(input, derived)
}


find_signs = function(expr) {
  parse_table = (expr
   |> macpan2:::rhs()
   |> method_parser()
   |> within({
    sign = ""
    sign[n == 0L] <- "to"
    sign[i[(x == "-") & (n == 1)] + 1L] <- "from"
    sign[i[(x == "-") & (n == 2)] + 2L] <- "from"
    sign[i[(x == "+") & (n == 1)] + 1L] <- "to"
    sign[i[(x == "+") & (n == 2)] + 2L] <- "to"
   })
  )
  setNames(
    parse_table[parse_table$n == 0L, c("x", "sign"), drop = FALSE],
    c("transition", "sign")
  )
}

make_transition_objects = function(transition_exprs, state_change_exprs) {
  state_change_indexes = (state_change_exprs
    |> vapply(macpan2:::lhs_char, character(1L))
    |> sub(pattern = "^d_", replacement = "")
  )
  transition_names = (transition_exprs
    |> vapply(macpan2:::lhs_char, character(1L))
  )
  names(transition_exprs) = transition_names
  f = (state_change_exprs
    |> lapply(find_signs)
    |> setNames(state_change_indexes)
    |> macpan2:::bind_rows(.id = "change_index")
  )
  rate_objects = list()
  for (transition in names(transition_exprs)) {
    rate_objects[[transition]] = mp_transition(
        type = transition
      , from = f$change_index[(f$transition == transition) & (f$sign == "from")]
      , to = f$change_index[(f$transition == transition) & (f$sign == "to")]
      , rate = transition_exprs[[transition]] |> macpan2:::rhs_char()
    )
  }
  rate_objects
}


## TODO: write a function to take two expression lists, (1) transition rates
## and (2) state changes, and return a list of Transition objects. this will set
## up the ability to write a variety of updated state change expressions
## (e.g. Euler, hazard, RK4, Gillespie)
##
## note: hazard correction is most easily done on the per-capita transition
## rates, but we have done away with these for the sake of transparency. this
## will require inefficient divisions to get the per-capita rates, after the
## absolute rates were computed. perhaps we can do this symbolically?



transition_exprs = list(
    infection ~ beta * S * I / N
  , recovery ~ gamma * I
  #, movement ~ orig * tau
)
state_change_exprs = list(
    d_S ~ - infection
  , d_I ~   infection - recovery
  , d_R ~               recovery
  #, d_orig ~ -movement
  #, d_dest ~  movement
)

transitions = make_transition_objects(transition_exprs, state_change_exprs)

state = mp_index(
    Epi  = c("S", "I",    "I",     "R")
  , Symp = c("",  "mild", "severe", "")
)
mp_slices(state, unpack = "no")
mp_factors(state, unpack = "no")
beta = Symp
gamma = Symp


state = mp_cartesian(
  mp_index(Epi = c("S", "I", "R")),
  mp_index(Loc = c("a", "b"))
)
mp_slices(state, unpack = "yes") ## adds indexes for S, I, R, a, and b to the global environment
mp_factors(state, unpack = "yes") ## adds indexes for Epi and Loc to the global environment
tau = mp_symmetric(Loc, "Dest") ## index of movement paths between locations
aggregation = mp_aggregate(state, "Loc") ## grouping factor for summing populations over strata
N = mp_reference(aggregation, "group") ## index for stratum-level population totals

mp_agg(N ~ group_sums(state, aggregation, N))

~ beta * from * I / N

list(
  infection = mp_flow(~beta * I / N, from = S    , to = I),
  recovery  = mp_flow(~gamma,        from = I    , to = R),
  movement  = mp_flow(~tau,          from = state, to = state)
)

N = mp_aggregate(~sum(state))

infection$ledger(
  indexes = list(beta = Loc, infectious_vector = I),
  by = list(
      from.to = "Loc"
    , I.beta = "Loc"
    , from.I = "Loc"
  )
)
movement$ledger(
  indexes = list(state = state, tau = tau),
  by = list(
      tau.from = "Loc",
      tau.to = "Dest" ~ "Loc",
      from.to = "Epi"
  )
)

state = mp_cartesian(
  mp_index(Epi = c("S", "I", "R")),
  mp_index(Stratum = c("a", "b"))
)
rates = mp_cartesian(
  mp_index(Epi = c("beta", "gamma")),
  mp_index(Stratum = c("a", "b"))
)
mp_slices(rates, 'yes')



s = TMBModel(
  init_mats = MatsList(
      state = c(S.a = 99, S.b = 99, I.a = 1, I.b = 1, R.a = 0, R.b = 0)
    , beta = 0.25
    , gamma = 0.1
    , tau = 0.05
    , infection = empty_matrix
    , recovery = empty_matrix
    , movement = empty_matrix
    , .mats_to_save = "state"
    , .mats_to_return = "state"
  ),
  expr_list = ExprList(
    during = list(
        infection ~ beta * S * I / 100
      , recovery ~ gamma * I
      , movement ~ tau * a
      , state[S_i] ~ S - infection
      , state[I_i] ~ I + infection - recovery
      , state[R_i] ~ R             + recovery
      , state[a_i] ~ a                        - movement
      , state[b_i] ~ b                        + movement
    )
  ),
  engine_methods = macpan2:::EngineMethods(
    exprs = list(
        S = ~ state[S_i]
      , I = ~ state[I_i]
      , R = ~ state[R_i]
      , a = ~ state[a_i]
      , b = ~ state[b_i]
    ),
    int_vecs = macpan2:::IntVecs(
      S_i = 0:1, I_i = 2:3, R_i = 4:5,
      a_i = seq(0, 4, by = 2),
      b_i = seq(1, 5, by = 2)
    )
  ),
  time_steps = Time(100L)
)

(s$simulator()$report()
  |> separate(row, c("status", "location"))
  |> ggplot()
  + geom_line(aes(time, value, colour = status, linetype = location))
)


m = mp_dynamic_model(
  expr_list = ExprList(
    during = list(fun_times = y ~ y + x, funner_times = y ~ y / x, fun_times = y ~ y + 1)
  ),
  unstruc_mats = list(x = 0, y = 0)
)
get_elements_by_names(m$expr_list$during, "fun_times")

s = mp_dynamic_simulator(m, 10L, mats_to_return = "y", params = macpan2:::OptParamsList(0, par_id = 0L, mat = "x", row_id = 0L, col_id = 0L))
mp_trajectory(s, parameter_vector = c(x = 10))
get_elements_by_names(m$expr_list$during, "funner_times")
m$expr_list$during$fun_times

## indexes
geog = mp_index(Loc = c("mon", "tor", "ham"))
symp = mp_index(Symp = c("mild", "severe"))
full = mp_cartesian(geog, symp)
move = mp_linear(geog, "Dest")

## ledgers
movement = mp_join(
  orig = geog,
  dest = geog,
  move = move,
  by = list(
    orig.move = "Loc",
    dest.move = "Loc" ~ "Dest"
  )
) |> mp_ledgers()
grouping = mp_aggregate(
  full,
  by = "Loc",
  ledger_column = "full2geog"
) |> mp_ledgers()
lineup = mp_join(
  full = full,
  geog2full = geog,
  symp2full = symp,
  by = list(
    full.geog2full = "Loc",
    full.symp2full = "Symp"
  )
) |> mp_ledgers()

sir = mp_dynamic_model(
  expr_list = ExprList(
    before = list(
        N ~ S + group_sums(I, full2geog, S) + R
    ),
    during = list(
        infection ~ S * group_sums(I, full2geog, S) * beta / N
      , recovery ~ gamma[symp2full] * I
      , movement[move] ~ tau[move] * S[orig]
      , S ~ S + (
          - infection
          + group_sums(movement, dest, S)
          - group_sums(movement, orig, S)
        )
      , I ~ I + infection[geog2full] - recovery
      , R ~ R + group_sums(recovery, full2geog, R)
    )
  ),
  init_vecs = list(
      S = geog, I = full, R = geog
    , beta = geog, gamma = symp
    , tau = move
    , movement = move
    , recovery = full
    , infection = geog
  ),
  ledgers = list(grouping, lineup, movement),
  unstruc_mats = list()
)

pop_each_city = 49999
(sir
  |> mp_dynamic_simulator(time_steps = 100L
    , vectors = list(
        S = c(mon = pop_each_city, tor = pop_each_city, ham = pop_each_city)
      , I = c(mon.mild = 1, tor.mild = 1, ham.mild = 1,
              mon.severe = 0, tor.severe = 0, ham.severe = 0)
      , R = c(mon = 0, tor = 0, ham = 0)
      , beta = c(mon = 0.15, tor = 0.25, ham = 0.3)
      , gamma = c(mild = 0.3, severe = 0.1)
      , tau = c(mon.tor = 0.01, tor.mon = 0.01, tor.ham = 0.01, ham.tor = 0.01)
      , movement = c(mon.tor = 0, tor.mon = 0, tor.ham = 0, ham.tor = 0)
      , infection = c(mon = 0, tor = 0, ham = 0)
      , recovery = c(mon.mild = 0, tor.mild = 0, ham.mild = 0,
                     mon.severe = 0, tor.severe = 0, ham.severe = 0)
    )
    , mats_to_return = c("I")
  )
  |> mp_trajectory()
  |> arrange(time)
  |> separate(row, c("city", "symptoms"))
  #|> View()
  |> ggplot()
  + geom_line(aes(time, value, colour = city, linetype = symptoms))
  #+ ylim(0, 50)
)
