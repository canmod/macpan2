# Flow Diagram Grid Layout (experimental)

Create a grid on which to layout the flow diagram of a model
specification.

## Usage

``` r
mp_layout_grid(
  spec,
  east = "",
  south = "^$",
  north = "^$",
  west = "^$",
  loops = north,
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

- east:

  Regular expression for matching the names of flows that will be
  connected eastward in the layout.

- south:

  Regular expression for matching the names of flows that will be
  connected southward in the layout.

- north:

  Regular expression for matching the names of flows that will be
  connected northward in the layout.

- west:

  Regular expression for matching the names of flows that will be
  connected westward in the layout.

- loops:

  Regular expression for matching the names of flows that cause loops in
  the flow model, and so should be ignored when building the layout.

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
