# Flow Diagram Grid Layout (experimental)

Layout the flow diagram of a model specification so that each row is one
of the paths through the model (ignoring loops).

## Usage

``` r
mp_layout_paths(
  spec,
  sort_paths = TRUE,
  combine_columns = TRUE,
  deduplicate_edges = TRUE,
  loops = "^$",
  ignore = "^$",
  x_gap = 0.3,
  y_gap = 0.3,
  north_south_sep = 0,
  east_west_sep = 0,
  trim_states = character()
)
```

## Arguments

- spec:

  A model specification made with
  [`mp_tmb_model_spec`](https://canmod.github.io/macpan2/reference/mp_tmb_model_spec.md)
  or related function.

- sort_paths:

  Should the paths/rows be sorted to minimize the number times an edge
  must go through a node that it is not connected with?

- combine_columns:

  Should each state/node get its own column in the layout (`FALSE`) or
  should the algorithm try to place branching states in the same column
  (`TRUE`, default).

- deduplicate_edges:

  Should each row have all of the edges in the path or should duplicate
  edges be removed?

- loops:

  Regular expression for matching the names of flows that cause loops in
  the flow model, and so should be ignored when building the layout.

- ignore:

  Regular expression for matching the names of flows that should be
  removed from the layout analysis entirely. These will be isolated in a
  data frame for custom drawing of 'difficult' edges.

- x_gap:

  Size of the gap to the left and right of the 1-by-1 space provided for
  a node.

- y_gap:

  Size of the gap above and below the 1-by-1 space provided for a node.

- north_south_sep:

  Horizontal separation between north and south flow arrows.

- east_west_sep:

  Vertical separation between east and west flow arrows.

- trim_states:

  List of states to remove from the diagram
