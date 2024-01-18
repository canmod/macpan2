library(macpan2)

Epi = mp_index(Epi = c("S", "I", "R"))
Age = mp_index(Age = c("young", "old"))

state = mp_cartesian(Epi, Age)
flow = mp_union(
    mp_cartesian(mp_index(Epi = "foi"), Age)
  , mp_index(Epi = "gamma")
)

N = mp_aggregate(state, by = "Age")
mp_group(state, by = "Age")  ## probably this is out-dated in favour of mp_aggregate??

infection = mp_join(
    from = mp_subset(state, Epi = "S")
  , to = mp_subset(state, Epi = "I")
  , rate = mp_subset(flow, Epi = "foi")
  , by = list(
      from.to = "Age"
    , from.rate = "Age"
  )
)

recovery = mp_join(
    from = mp_subset(state, Epi = "I")
  , to = mp_subset(state, Epi = "R")
  , rate = mp_subset(flow, Epi = "gamma")
  , by = list(from.to = "Age")
)

A = mp_square(state)
weird_join_test = mp_join(
    a = mp_subset(A, EpiA = "R")
  , b = mp_subset(A, EpiA = "I")
  , c = mp_subset(A, EpiA = "I")
  , by = list(c.a = c("AgeA", "AgeB"), a.b = c("AgeA", "AgeB"), b.c = "AgeA")
)
weird_join_test
