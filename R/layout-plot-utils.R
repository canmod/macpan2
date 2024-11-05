
LayoutMatrixUtils = function() {
  self = Base()
  self$states = function() {
    x = self$matrix()
    dim(x) = NULL
    x = unique(x)
    x = x[x != ""]
    return(x)
  }
  self$lattice = function() {
    states = self$states()
    layout = self$matrix()
    lr = row(layout)
    lc = col(layout)
    list(
        rows = lapply(states, \(state) lr[layout == state]) |> setNames(states)
      , cols = lapply(states, \(state) lc[layout == state]) |> setNames(states)
      , nr = nrow(layout)
      , nc = ncol(layout)
    )
  }
  self$nodes = function() {
    lattice = self$lattice()
    states = names(lattice$rows)
    pos = data.frame(state = states
      , ymax = lattice$nr - vapply(lattice$rows, min, integer(1L)) + 1.5 - self$y_gap
      , ymin = lattice$nr - vapply(lattice$rows, max, integer(1L)) + 0.5 + self$y_gap
      , xmin = vapply(lattice$cols, min, integer(1L)) - 0.5 + self$x_gap
      , xmax = vapply(lattice$cols, max, integer(1L)) + 0.5 - self$x_gap
    )
    pos$x = 0.5 * (pos$xmin + pos$xmax)
    pos$y = 0.5 * (pos$ymin + pos$ymax)
    pos
  }
  self$edges_flows = function() {
    self$merge(self$flows(), self$nodes()) |> self$edge_dimensions()
  }
  self$edges_flows_ignored = function() {
    self$merge(self$flows_ignored(), self$nodes())
  }
  self$edges_inflows = function() {
    (self$inflows()
      |> merge(self$nodes(), by.x = "to", by.y = "state")
      |> self$inflow_dimensions()
    )
  }
  self$edges_conversions = function() {
    nodes = self$nodes()
    merged_data = (self$inflows()
      |> merge(nodes, by.x = "to", by.y = "state")
      |> merge(nodes, by.x = "from", by.y = "state", suffixes = c("_to", "_from"))
      |> self$edge_dimensions()
    )
    merged_data
  }
  self$edges_outflows = function() {
    (self$outflows()
      |> merge(self$nodes(), by.x = "from", by.y = "state")
      |> self$outflow_dimensions()
    )
  }
  self$merge = function(edges, nodes) {
    #edges = edges[edges$type == "flow", , drop = FALSE]
    merged_data = (edges
      |> merge(nodes, by.x = "to", by.y = "state")
      |> merge(nodes, by.x = "from", by.y = "state", suffixes = c("_to", "_from"))
    )
    merged_data
  }
  self$outflow_dimensions = function(edges_frame) {
    e = edges_frame
    e$xend = e$xmin - self$x_gap
    e$yend = e$ymax + self$x_gap
    e$xlab = 0.5 * (e$xmin + e$xend)
    e$ylab = 0.5 * (e$ymax + e$yend)
    return(e)
  }
  self$inflow_dimensions = function(edges_frame) {
    e = edges_frame
    e$xstart = e$xmin - 2 * self$x_gap
    e$xlab = e$xmin - self$x_gap
    return(e)
  }
  self$edge_dimensions = function(edges_frame) {
    e = edges_frame
    lattice = self$lattice()
    shared_rows = mapply(intersect
      , lattice$rows[e$from]
      , lattice$rows[e$to]
      , SIMPLIFY = FALSE
      , USE.NAMES = FALSE
    )
    shared_cols = mapply(intersect
      , lattice$cols[e$from]
      , lattice$cols[e$to]
      , SIMPLIFY = FALSE
      , USE.NAMES = FALSE
    )
    e$hori = vapply(shared_rows, length, integer(1L)) != 0L
    e$vert = vapply(shared_cols, length, integer(1L)) != 0L
    
    e$dir = 0
    e$x = 0
    e$x[e$vert] <- vapply(shared_cols[e$vert], min, integer(1L))
    e$x[e$hori] <- 0.5 * (e$x_from[e$hori] + e$x_to[e$hori])
    #if (is.null(self$.path_ids)) {
      e$y = 0
      e$dir[e$vert] <- e$y_to[e$vert] - e$y_from[e$vert]
      e$y[e$hori] <- 1L + lattice$nr - vapply(shared_rows[e$hori], min, integer(1L))
      e$y[e$vert] <- 0.5 * (e$y_from[e$vert] + e$y_to[e$vert])
    # } else {
    #   path_ids = self$.path_ids
    #   path_ids$y = path_ids$path_id
    #   path_ids$path_id = NULL
    #   e$y = NULL
    #   e = merge(e, path_ids, by = c("from", "to"), all.x = TRUE, all.y = FALSE, sort = FALSE)
    #   e$y_from = e$y
    #   e$y_to = e$y
    # }
    x_gap = self$x_gap
    y_gap = self$y_gap
    e = within(e, {
      dir[hori] <- x_to[hori] - x_from[hori]
      
      north <- vert & dir > 0L
      south <- vert & dir < 0L
      east <- hori & dir > 0L
      west <- hori & dir < 0L
      
      ## where to plot edge labels
      x[north] <- x[north] + 0.5 * self$north_south_sep
      x[south] <- x[south] - 0.5 * self$north_south_sep
      y[east] <- y[east] + 0.5 * self$east_west_sep
      y[west] <- y[west] - 0.5 * self$east_west_sep
      
      ## where to plot arrow heads and tails
      x_from[east] <- xmax_from[east]
      x_to[east] <- xmin_to[east]
      x_from[west] <- xmin_from[west]
      x_to[west] <- xmax_to[west]
      
      y_from[north] <- ymax_from[north]
      y_to[north] <- ymin_to[north]
      y_from[south] <- ymin_from[south]
      y_to[south] <- ymax_to[south]
      
      y_from[hori] <- y[hori]
      y_to[hori] <- y[hori]
      x_from[vert] <- x[vert]
      x_to[vert] <- x[vert]
    })
    e = self$path_id_update(e)
    return(e)
  }
  self$empty_flow_frame = data.frame(
      from = character()
    , to = character()
    , name = character()
    , rate = character()
    , type = character()
    , from_name = character()
    , to_name = character()
  )
  return_object(self, "LayoutMatrixUtils")
}

LayoutMatrix = function() {
  self = LayoutMatrixUtils()
  
  ## the most important function here. it is a character matrix
  ## with names of state variables or blank strings. the locations of
  ## the state variables gives where they will be displayed on a flow
  ## diagram. each state can be in more than one element but these
  ## elements should (must??) cluster together into a rectangle.
  self$matrix = function() matrix(character(), 0L, 0L)
  
  ## flows used to determine the layout
  self$flows = function() self$empty_flow_frame
  
  ## flows not use to determine the layout
  self$flows_ignored = function() self$empty_flow_frame
  
  ## inflows and outflows (neither used to determine the layout)
  self$inflows = function() self$empty_flow_frame
  self$outflows = function() self$empty_flow_frame
  
  self$path_id_update = function(e) return(e)
  
  return_object(self, "LayoutMatrix")
}

#' @export
print.LayoutMatrix = function(x, ...) {
  print(x$matrix())
  invisible(x)
}

#' Flow Diagram Grid Layout
#' 
#' Create a grid on which to layout the flow diagram of a model specification.
#' 
#' @param spec A model specification made with \code{\link{mp_tmb_model_spec}}
#' or related function.
#' @param east Regular expression for matching the names of flows that will
#' be connected eastward in the layout.
#' @param south Regular expression for matching the names of flows that will
#' be connected southward in the layout.
#' @param north Regular expression for matching the names of flows that will
#' be connected northward in the layout.
#' @param west Regular expression for matching the names of flows that will
#' be connected westward in the layout.
#' @param loops Regular expression for matching the names of flows that cause
#' loops in the flow model, and so should be ignored when building the layout.
#' @param x_gap Size of the gap to the left and right of the 1-by-1 space 
#' provided for a node.
#' @param y_gap Size of the gap above and below the 1-by-1 space 
#' provided for a node.
#' @param north_south_sep Horizontal separation between north and south 
#' flow arrows.
#' @param east_west_sep Vertical separation between east and west flow arrows.
#' @param trim_states List of states to remove from the diagram
#' 
#' @export
mp_layout_grid = function(spec
    , east = ""
    , south = "^$"
    , north = "^$"
    , west = "^$"
    , loops = north ## often reasonable to have loops go north 
    , x_gap = 0.3
    , y_gap = 0.3
    , north_south_sep = 0
    , east_west_sep = 0
    , trim_states = character()
) LayoutMatrixGrid(spec, east, south, north, west, loops, x_gap, y_gap, north_south_sep, east_west_sep, trim_states)

#' Flow Diagram Grid Layout
#' 
#' Layout the flow diagram of a model specification so that each row is 
#' one of the paths through the model (ignoring loops).
#' 
#' @param spec A model specification made with \code{\link{mp_tmb_model_spec}}
#' or related function.
#' @param sort_paths Should the paths/rows be sorted to minimize the 
#' number times an edge must go through a node that it is not connected with?
#' @param combine_columns Should each state/node get its own column in the
#' layout (`FALSE`) or should the algorithm try to place branching states 
#' in the same column (`TRUE`, default).
#' @param deduplicate_edges Should each row have all of the edges in the path
#' or should duplicate edges be removed?
#' @param ignore Regular expression for matching the names of flows that should
#' be removed from the layout analysis entirely. These will be isolated in
#' a data frame for custom drawing of 'difficult' edges.
#' @inheritParams mp_layout_grid
#' 
#' @export
mp_layout_paths = function(spec
    , sort_paths = TRUE
    , combine_columns = TRUE
    , deduplicate_edges = TRUE
    , loops = "^$"
    , ignore = "^$"
    , x_gap = 0.3
    , y_gap = 0.3
    , north_south_sep = 0
    , east_west_sep = 0
    , trim_states = character()
  ) LayoutMatrixPaths(spec, sort_paths, combine_columns, deduplicate_edges, loops, ignore, x_gap, y_gap, north_south_sep, east_west_sep, trim_states)

LayoutMatrixGrid = function(spec
    , east = ""
    , south = "^$"
    , north = "^$"
    , west = "^$"
    , loops = north ## often reasonable to have loops go north 
    , x_gap = 0.3
    , y_gap = 0.3
    , north_south_sep = 0
    , east_west_sep = 0
    , trim_states = character()
) {
  self = LayoutMatrix()
  self$spec = spec
  self$loops = loops
  self$east = east
  self$north = north
  self$west = west
  self$south = south
  self$x_gap = x_gap
  self$y_gap = y_gap
  self$north_south_sep = north_south_sep
  self$east_west_sep = east_west_sep
  self$trim_states = trim_states
  
  flows = mp_flow_frame(spec, loops = loops)
  flows = flows[!flows$from %in% self$trim_states, , drop = FALSE]
  flows = flows[!flows$to %in% self$trim_states, , drop = FALSE]
  flows_inside = flows[flows$has_from_state & flows$has_to_state, , drop = FALSE]
  flows_outside = flows[!flows$has_from_state | !flows$has_to_state, , drop = FALSE]
  self$.east = grepl(east, flows_inside$name)
  self$.west = grepl(west, flows_inside$name)
  self$.north = grepl(north, flows_inside$name)
  self$.south = grepl(south, flows_inside$name)
  self$.include = self$.east | self$.north | self$.south | self$.west
  
  ## all flows that haven't been trimmed somehow
  self$.flows = flows
  
  ## all flows that point from one state to another, even if the from state is 
  ## not losing anything (e.g., viral shedding) or the to state is not gaining 
  ## anything (e.g., ... can't actually think of an example right now ...)
  self$.flows_inside = flows_inside
  
  ## all flows that either go from outside the system to a state (e.g., birth, 
  ## importation) or go from a state to outside the system (e.g., death if you
  ## do not have an accumulator for total deaths).
  self$.flows_outside = flows_outside
  
  ## all included flows
  self$flows = function() {
    self$.flows_inside[self$.include, , drop = FALSE]
  }
  
  ## all excluded flows
  self$flows_ignored = function() {
    self$.flows_inside[!self$.include, , drop = FALSE]
  }
  
  ## all inflows from outside the system
  self$inflows = function() {
    self$.flows_outside[self$.flows_outside$type == "inflow", , drop = FALSE]
  }
  
  ## all outflows to the outside of the system
  self$outflows = function() {
    self$.flows_outside[self$.flows_outside$type == "outflow", , drop = FALSE]
  }
  
  self$matrix = function() {
    flows = self$flows()
    states = topological_sort_general(flows, loops = self$loops)
    layout = matrix("", 2L * length(states) + 1L, 2L * length(states) + 1L)
    layout[length(states), length(states)] = states[1L]
    
    links = list(
        E = flows[self$.east, , drop = FALSE] |> neighbour_list()
      , N = flows[self$.north, , drop = FALSE] |> neighbour_list()
      , W = flows[self$.west, , drop = FALSE] |> neighbour_list()
      , S = flows[self$.south, , drop = FALSE] |> neighbour_list()
    )
    
    lr = row(layout)
    lc = col(layout)
    for (state in states) {
      i = layout == state
      state_rows = lr[i]
      state_cols = lc[i]
      neighbour = links$N[[state]]
      if (length(neighbour) == 1L) layout[min(state_rows) - 1L, min(state_cols)] = neighbour
      neighbour = links$S[[state]]
      if (length(neighbour) == 1L) layout[max(state_rows) + 1L, min(state_cols)] = neighbour
      neighbour = links$E[[state]]
      if (length(neighbour) == 1L) layout[min(state_rows), max(state_cols) + 1L] = neighbour
      neighbour = links$W[[state]]
      if (length(neighbour) == 1L) layout[min(state_rows), min(state_cols) - 1L] = neighbour
    }
    i = layout != ""
    return(layout[rowSums(i) > 0L, colSums(i) > 0L])
  }
  
  return_object(self, "LayoutMatrixFactors")
}

LayoutMatrixPaths = function(spec
    , sort_paths = TRUE
    , combine_columns = TRUE
    , deduplicate_edges = TRUE
    , loops = "^$"
    , ignore = "^$"
    , x_gap = 0.3
    , y_gap = 0.3
    , north_south_sep = 0
    , east_west_sep = 0
    , trim_states = character()
  ) {
  self = LayoutMatrix()
  self$spec = spec
  self$loops = loops
  self$sort_paths = sort_paths
  self$combine_columns = combine_columns
  self$deduplicate_edges = deduplicate_edges
  self$x_gap = x_gap
  self$y_gap = y_gap
  self$north_south_sep = north_south_sep
  self$east_west_sep = east_west_sep
  self$trim_states = trim_states
  
  self$.flows = mp_flow_frame(spec, loops = loops)
  self$.flows = self$.flows[!self$.flows$from %in% self$trim_states, , drop = FALSE]
  self$.flows = self$.flows[!self$.flows$to %in% self$trim_states, , drop = FALSE]
  
  self$.flows_inside = self$.flows[self$.flows$has_from_state & self$.flows$has_to_state, , drop = FALSE]
  self$.flows_outside = self$.flows[!self$.flows$has_from_state | !self$.flows$has_to_state, , drop = FALSE]
  
  self$.include = !grepl(ignore, self$.flows_inside$name)
  
  states = topological_sort_general(self$.flows_inside, loops = loops)
  self$.paths <- find_all_paths(self$.flows_inside, states[1L])
  
  if (length(self$.paths) == 0L) stop("model has no paths!")
  if (length(self$.paths) == 1L) self$sort_paths = FALSE
  
  
  ## all included flows
  self$flows = function() {
    self$.flows_inside[self$.include, , drop = FALSE]
  }
  
  ## all excluded flows
  self$flows_ignored = function() {
    self$.flows_inside[!self$.include, , drop = FALSE]
  }
  
  ## all inflows from outside the system
  self$inflows = function() {
    self$.flows_outside[self$.flows_outside$type == "inflow", , drop = FALSE]
  }
  
  ## all outflows to the outside of the system
  self$outflows = function() {
    self$.flows_outside[self$.flows_outside$type == "outflow", , drop = FALSE]
  }
  
  self$matrix = function() {
    flows = self$flows()
    
    states = topological_sort_general(flows, self$loops)
    
    # Pad each path by aligning nodes to their respective positions
    padded_paths <- lapply(self$.paths, function(path) {
      padded <- rep("", length(states))  # Create an empty vector of the same length as all_nodes
      for (i in seq_along(path)) {
        state <- path[i]
        padded[which(states == state)] <- state  # Place each node in its correct position
      }
      return(padded)
    })
    
    # Convert the list of padded paths to a matrix
    padded_paths_matrix <- do.call(rbind, padded_paths)
    
    # Remove blank rows and columns (might be unnecessary)
    blank_els = padded_paths_matrix == ""
    rr = !apply(blank_els, 1L, all)
    cc = !apply(blank_els, 2L, all)
    padded_paths_matrix = padded_paths_matrix[rr, cc]
    
    if (self$sort_paths) {
      i = order_corresp(padded_paths_matrix)
      padded_paths_matrix = padded_paths_matrix[i, , drop = FALSE]
      self$.paths = self$.paths[i]
    }
    
    
    ## only need one edge per node-pair
    if (self$combine_columns) {
      padded_paths_matrix = combine_adjacent_columns(padded_paths_matrix)
    }
    path_to_edges = function(path) data.frame(from = path[-length(path)], to = path[-1])
    path_ids = lapply(self$.paths, path_to_edges) |> bind_rows(.id = "path_id")
    path_ids$path_id = as.numeric(path_ids$path_id)
    if (self$deduplicate_edges) {
      i = duplicated(path_ids[, c("to", "from")])
      edges_to_keep = path_ids[!i, , drop = FALSE]
      for (state in states) {
        paths_required_for_state = c(
            edges_to_keep$path_id[edges_to_keep$from == state]
          , edges_to_keep$path_id[edges_to_keep$to == state]
        )
        state_rows = setdiff(seq_along(self$.paths), paths_required_for_state)
        k = (padded_paths_matrix == state) & (row(padded_paths_matrix) %in% state_rows)
        padded_paths_matrix[k] = ""
      }
      self$.path_ids = edges_to_keep
    } else {
      self$.path_ids = path_ids
    }
    
    return(padded_paths_matrix)
  }
  .trash = self$matrix()
  
  self$path_id_update = function(e) {
    pp = self$.path_ids
    pp$y = pp$y_from = pp$y_to = 1L + max(pp$path_id) - pp$path_id
    pp$path_id = NULL
    e$y = e$y_from = e$y_to = NULL
    e = merge(e, pp, by = c("from", "to"), all.x = TRUE, sort = FALSE)
    e$ymin_to = e$ymin_from = e$y - self$y_gap
    e$ymax_to = e$ymax_from = e$y + self$y_gap
    return(e)
  }
  
  return_object(self, "LayoutMatrixPaths")
}

states_from_layout_matrix = function(layout_matrix) {
  states = layout_matrix
  dim(states) = NULL
  states = unique(states)
  states = states[states != ""]
  return(states)
}

node_lattice = function(layout_matrix) {
  states = states_from_layout_matrix(layout_matrix)
  lr = row(layout_matrix)
  lc = col(layout_matrix)
  list(
      rows = lapply(states, \(state) lr[layout_matrix == state]) |> setNames(states)
    , cols = lapply(states, \(state) lc[layout_matrix == state]) |> setNames(states)
    , nr = nrow(layout_matrix)
    , nc = ncol(layout_matrix)
  )
}

node_dimensions = function(lattice, x_gap = 0.2, y_gap = 0.2) {
  states = names(lattice$rows)
  pos = data.frame(state = states
    , ymax = lattice$nr - vapply(lattice$rows, min, integer(1L)) + 1.5 - y_gap
    , ymin = lattice$nr - vapply(lattice$rows, max, integer(1L)) + 0.5 + y_gap
    , xmin = vapply(lattice$cols, min, integer(1L)) - 0.5     + x_gap
    , xmax = vapply(lattice$cols, max, integer(1L)) + 0.5 - x_gap
  )
  pos$x = 0.5 * (pos$xmin + pos$xmax)
  pos$y = 0.5 * (pos$ymin + pos$ymax)
  pos
}

layout_merge = function(flows, node_data) {
  flows = flows[flows$type == "flow", , drop = FALSE]
  edge_data = (flows
    |> merge(node_data, by.x = "to", by.y = "state")
    |> merge(node_data, by.x = "from", by.y = "state", suffixes = c("_to", "_from"))
  )
  edge_data
}

edge_dimensions = function(flows, node_data, lattice) {
  
  e = layout_merge(flows, node_data)

  shared_rows = mapply(intersect
    , lattice$rows[e$from]
    , lattice$rows[e$to]
    , SIMPLIFY = FALSE
    , USE.NAMES = FALSE
  )
  shared_cols = mapply(intersect
    , lattice$cols[e$from]
    , lattice$cols[e$to]
    , SIMPLIFY = FALSE
    , USE.NAMES = FALSE
  )
  e$hori = vapply(shared_rows, length, integer(1L)) != 0L
  e$vert = vapply(shared_cols, length, integer(1L)) != 0L
  
  e$dir = 0
  e$x = 0
  e$y = 0
  x_gap = 0.3
  y_gap = 0.3
  e = within(e, {
    dir[vert] <- y_to[vert] - y_from[vert]
    dir[hori] <- x_to[hori] - x_from[hori]
    
    north <- vert & dir > 0L
    south <- vert & dir < 0L
    east <- hori & dir > 0L
    west <- hori & dir < 0L
    
    ## where to plot edge labels
    y[hori] <- 1L + lattice$nr - vapply(shared_rows[hori], min, integer(1L))
    x[vert] <- vapply(shared_cols[vert], min, integer(1L))
    y[vert] <- 0.5 * (y_from[vert] + y_to[vert])
    x[hori] <- 0.5 * (x_from[hori] + x_to[hori])
    
    ## where to plot arrow heads and tails
    x_from[east] <- xmax_from[east]
    x_to[east] <- xmin_to[east]
    x_from[west] <- xmin_from[west]
    x_to[west] <- xmax_to[west]
    
    y_from[north] <- ymax_from[north]
    y_to[north] <- ymin_to[north]
    y_from[south] <- ymin_from[south]
    y_to[south] <- ymax_to[south]
    
    y_from[hori] <- y[hori]
    y_to[hori] <- y[hori]
    x_from[vert] <- x[vert]
    x_to[vert] <- x[vert]
  })
  
  
  
  return(e)
}


traverse_graph_on_lattice = function(layout_matrix, state, links, init_row = 1L, init_col = 1L, all_states = character(), visited = character()) {
  if (state %in% visited) return(layout_matrix)
  visited = append(visited, state)
  layout_matrix[init_row, init_col] = all_states[1L]
  lr = row(layout_matrix)
  lc = col(layout_matrix)
  i = layout_matrix == state
  state_rows = lr[i]
  state_cols = lc[i]
  
  neighbour = links$N[[state]]
  if (length(neighbour) == 1L) {
    layout_matrix[state_rows + 1L, state_cols] = neighbour
    layout_matrix = Recall(layout_matrix, neighbour, links, init_row, init_col, all_states, visited)
  }
  neighbour = links$S[[state]]
  if (length(neighbour) == 1L) {
    layout_matrix[state_rows - 1L, state_cols] = neighbour
    layout_matrix = Recall(layout_matrix, neighbour, links, init_row, init_col, all_states, visited)
  }
  neighbour = links$E[[state]]
  if (length(neighbour) == 1L) {
    layout_matrix[state_rows, state_cols + 1L] = neighbour
    layout_matrix = Recall(layout_matrix, neighbour, links, init_row, init_col, all_states, visited)
  }
  neighbour = links$W[[state]]
  if (length(neighbour) == 1L) {
    layout_matrix[state_rows, state_cols - 1L] = neighbour
    layout_matrix = Recall(layout_matrix, neighbour, links, init_row, init_col, all_states, visited)
  }
  
  return(layout_matrix)
}
layout = function(states, links, init_row = 1L, init_col = 1L) {
  layout_matrix = traverse_graph_on_lattice(
      matrix("", init_row + 2 * length(states), init_col + 2 * length(states))
    , states[1L]
    , links
    , init_row
    , init_col
    , states
  )
  i = layout_matrix != ""
  layout_matrix[rowSums(i) > 0L, colSums(i) > 0L]
}

# @param i Index representing one state.
# @param layout List with two vectors, `x` and `y`, giving coordinates of
# each state.
# @param links List with `N`, `S`, `E`, `W` components, each element of which
# gives states to the north, south, east, and west of a focal state.
# @param visited Vector containing indices of states that have been visited,
# with this vector growing as the recursion traverses the graph
# @param all_states character vector in topological order.
# product_layout = function(i, layout, links, all_states = character(), visited = integer()) {
#   err = "This is not a model that can be laid out on a grid"
#   if (i %in% visited) return(layout)
#   visited = append(visited, i)
#   layout$x_min[1] = layout$y_min[1] = layout$x_max[1] = layout$y_max[1] = 1
#   
#   focal_state = links$N[[all_states[i]]]
#   if (length(focal_state) == 1L) {
#     j = match(focal_state, all_states)
#     if (!j %in% visited) {
#       layout$x[j] = layout$x[i]
#       layout$y[j] = layout$y[i] + 1L
#       layout = Recall(j, layout, links, all_states, visited)
#     }
#   } else if (length(focal_state) != 0L) stop(err)
#   
#   focal_state = links$S[[all_states[i]]]
#   if (length(focal_state) == 1L) {
#     j = match(focal_state, all_states)
#     if (!j %in% visited) {
#       layout$x[j] = layout$x[i]
#       layout$y[j] = layout$y[i] - 1L
#       layout = Recall(j, layout, links, all_states, visited)
#     }
#   } else if (length(focal_state) != 0L) stop(err)
#   
#   focal_state = links$E[[all_states[i]]]
#   if (length(focal_state) == 1L) {
#     j = match(focal_state, all_states)
#     if (!j %in% visited) {
#       layout$x[j] = layout$x[i] + 1L
#       layout$y[j] = layout$y[i]
#       layout = Recall(j, layout, links, all_states, visited)
#     }
#   } else if (length(focal_state) != 0L) stop(err)
#   
#   focal_state = links$W[[all_states[i]]]
#   if (length(focal_state) == 1L) {
#     j = match(focal_state, all_states)
#     if (!j %in% visited) {
#       layout$x[j] = layout$x[i] - 1L
#       layout$y[j] = layout$y[i]
#       layout = Recall(j, layout, links, all_states, visited)
#     }
#   } else if (length(focal_state) != 0L) stop(err)
#   
#   return(layout)
# }





neighbour_list = function(flows) {
  if (any(duplicated(flows$from))) stop("more than one neighbour per state")
  setNames(as.list(flows$to), flows$from)
}


# mp_layout_path <- function(
#       edges_df
#     , sort_paths = TRUE
#     , combine_columns = TRUE
#     , deduplicate_edges = TRUE
#     , align_dir_flows = TRUE
#     , resolve_nodes = character()
#     , keep_paths = integer()
#   ) {
#   
#   inflow_df = edges_df[edges_df$type == "inflow", , drop = FALSE]
#   outflow_df = edges_df[edges_df$type == "outflow", , drop = FALSE]
#   edges_df = edges_df[edges_df$type == "flow", , drop = FALSE]
#   
#   all_edges_df = edges_df[edges_df$type == "flow", , drop = FALSE]
#   edges_df = edges_df[!edges_df$to %in% resolve_nodes, , drop = FALSE]
#   removed_edges_df = all_edges_df[all_edges_df$to %in% resolve_nodes, , drop = FALSE]
#   paths <- find_all_paths(edges_df, resolve_nodes)
#   if (length(paths) == 0L) stop("model has no paths!")
#   if (length(paths) == 1L) sort_paths = FALSE
#   if (length(keep_paths) > 0L) paths = paths[keep_paths]
#   if (align_dir_flows) {
#     bidir_edges = find_bidirectional_edges(edges_df)
#     if (length(bidir_edges) > 0) {
#       paths <- split_bidirectional_paths(paths, bidir_edges)
#     }
#   }
#   
#   # Step 1: Identify all unique nodes from the edges and sort them topologically
#   all_nodes <- unique(c(edges_df$from, edges_df$to))
#   resolve_nodes = intersect(all_nodes, resolve_nodes)
#   all_nodes = c(resolve_nodes, setdiff(all_nodes, resolve_nodes))
#   all_nodes = topological_sort_engine(edges_df, all_nodes, warn_not_dag = !align_dir_flows)
#   
#   # Step 2: Pad each path by aligning nodes to their respective positions
#   if (align_dir_flows) {
#     temp_fn = function(edges_df, bidir_edges, paths, nodes) {
#       group_map = find_node_groups(nodes, bidir_edges)
#       groups = unique(group_map)
#       
#       padded_path_matrix = matrix("", nrow = length(paths), ncol = length(groups))
#       paths_with_groups = lapply(paths, \(path) group_map[path])
#       
#       values = unlist(paths)
#       grouped_values = unlist(paths_with_groups, use.names = FALSE)
#       indexes = cbind(
#           rep(seq_along(paths), times = vapply(paths, length, integer(1L)))
#         , match(grouped_values, groups)
#       )
#       padded_path_matrix[indexes] = values
#       padded_path_matrix = padded_path_matrix[!duplicated(padded_path_matrix), , drop = FALSE]
#       return(padded_path_matrix)
#     }
#     padded_paths_matrix = temp_fn(edges_df, bidir_edges, paths, all_nodes)
#   } else {
#     padded_paths <- lapply(paths, function(path) {
#       padded <- rep("", length(all_nodes))  # Create an empty vector of the same length as all_nodes
#       for (i in seq_along(path)) {
#         node <- path[i]
#         padded[which(all_nodes == node)] <- node  # Place each node in its correct position
#       }
#       return(padded)
#     })
#     
#     # Convert the list of padded paths to a matrix for easy visualization
#     padded_paths_matrix <- do.call(rbind, padded_paths)
#   }
#   
#   if (sort_paths) {
#     i = order_corresp(padded_paths_matrix)
#     padded_paths_matrix = padded_paths_matrix[i, , drop = FALSE]
#     paths = paths[i]
#   }
#   
#   box_heights = data.frame(
#       name = unique(padded_paths_matrix[padded_paths_matrix != ""])
#     , min_path_id = vapply(apply(padded_paths_matrix != "", 2L, which, simplify = FALSE), min, integer(1L))
#     , max_path_id = vapply(apply(padded_paths_matrix != "", 2L, which, simplify = FALSE), max, integer(1L))
#   )
#   
#   path_to_edges = function(path) data.frame(from = path[-length(path)], to = path[-1])
#   path_ids = lapply(paths, path_to_edges) |> bind_rows(.id = "path_id")
#   path_ids$path_id = as.numeric(path_ids$path_id)
#   
#   ## only need one edge per node-pair
#   if (deduplicate_edges) {
#     i = duplicated(path_ids[, c("to", "from")])
#     path_ids = path_ids[!i, , drop = FALSE]
#   }
#   if (combine_columns) {
#     padded_paths_matrix = combine_adjacent_columns(padded_paths_matrix)
#   }
#   
#   node_grp_ids = data.frame(
#       name = all_nodes
#     , node_grp_ids = vapply(all_nodes, \(x) which(apply(padded_paths_matrix == x, 2L, any)), integer(1L))
#   )
#   
#   edges_df = merge(edges_df, path_ids, by = c("from", "to")) |> merge(node_grp_ids, by.x = "from", by.y = "name") |> merge(node_grp_ids, by.x = "to", by.y = "name", suffix = c("_from", "_to"))
#   nodes_df = merge(box_heights, node_grp_ids, by = "name")
#   
#   path_twice = c(edges_df$path_id, edges_df$path_id)
#   from_to = c(edges_df$from, edges_df$to)
#   
#   min_paths = tapply(path_twice, from_to, min)
#   max_paths = tapply(path_twice, from_to, max)
#   
#   if (deduplicate_edges) {
#     nodes_df$min_path_id = min_paths[nodes_df$name]
#     nodes_df$max_path_id = max_paths[nodes_df$name]
#   }
#   
#   if (nrow(removed_edges_df) > 0L) {
#     removed_edges_df = merge(removed_edges_df, nodes_df, by.x = "from", by.y = "name") |> merge(nodes_df, by.x = "to", by.y = "name", suffix = c("_from", "_to"))
#   }
#   
#   return(nlist(paths, padded_paths_matrix, edges_df, nodes_df, removed_edges_df, inflow_df, outflow_df))
# }

# mp_plot_layout = function(layout, size = 6) {
#   p = (ggplot()
#     + geom_segment(
#         aes(x = from_east, xend = to_west, y = y, yend = y)
#       , data = layout$edges_df
#       , arrow = mp_flow_arrow
#       , colour = "blue"
#     )
#     + geom_rect(aes(xmin = west, xmax = east, ymin = north, ymax = south)
#       , fill = "lightblue"
#       , data = layout$nodes_df
#     )
#     + geom_text(
#         aes(x, y, label = name)
#       , size = size
#       , data = layout$nodes_df
#       , colour = "blue"
#     )
#     + mp_ggtheme
#   )
#   p
# }

mp_add_ports = function(parsed_paths
  , x_pad = 0.3  ## so you can have space for horizontal arrows
  , y_pad = 0.1  ## so you can tell when one box begins and the other ends
) {
  ndf = parsed_paths$nodes_df
  edf = parsed_paths$edges_df
  rdf = parsed_paths$removed_edges_df
  idf = parsed_paths$inflow_df
  odf = parsed_paths$outflow_df
  
  ndf$x = ndf$node_grp_ids
  ndf$east = ndf$node_grp_ids + x_pad
  ndf$west = ndf$node_grp_ids - x_pad
  ndf$near_east = ndf$node_grp_ids + x_pad/2
  ndf$near_west = ndf$node_grp_ids - x_pad/2
  ndf$y = (ndf$min_path_id + ndf$max_path_id + 1)/2
  ndf$north = ndf$max_path_id + 1 - y_pad
  ndf$south = ndf$min_path_id + y_pad
  
  edf$from_x = edf$node_grp_ids_from
  edf$from_east = edf$node_grp_ids_from + x_pad
  edf$from_west = edf$node_grp_ids_from - x_pad
  edf$from_near_east = edf$node_grp_ids_from + x_pad/2
  edf$from_near_west = edf$node_grp_ids_from - x_pad/2
  
  edf$to_x = edf$node_grp_ids_to
  edf$to_east = edf$node_grp_ids_to + x_pad
  edf$to_west = edf$node_grp_ids_to - x_pad
  edf$to_near_east = edf$node_grp_ids_to + x_pad/2
  edf$to_near_west = edf$node_grp_ids_to - x_pad/2
  
  edf$y = edf$path_id + 0.5
  edf$north = edf$path_id + 1
  edf$south = edf$path_id
  edf$x = (edf$to_x + edf$from_x)/2
  
  if (nrow(rdf) > 0L) {
    rdf$from_x = rdf$node_grp_ids_from
    rdf$from_east = rdf$node_grp_ids_from + x_pad
    rdf$from_west = rdf$node_grp_ids_from - x_pad
    rdf$from_near_east = rdf$node_grp_ids_from + x_pad/2
    rdf$from_near_west = rdf$node_grp_ids_from - x_pad/2
    
    rdf$to_x = rdf$node_grp_ids_to
    rdf$to_east = rdf$node_grp_ids_to + x_pad
    rdf$to_west = rdf$node_grp_ids_to - x_pad
    rdf$to_near_east = rdf$node_grp_ids_to + x_pad/2
    rdf$to_near_west = rdf$node_grp_ids_to - x_pad/2
    
    rdf$to_south = rdf$min_path_id_to + y_pad
    rdf$to_north = rdf$max_path_id_to + 1 - y_pad
    rdf$from_south = rdf$min_path_id_from + y_pad
    rdf$from_north = rdf$max_path_id_from + 1 - y_pad
    rdf$to_y = (rdf$to_south + rdf$to_north)/2
    rdf$from_y = (rdf$from_south + rdf$from_north)/2
    rdf$x = (rdf$to_x + rdf$from_x)/2
  } else {
    rdf$from_x = integer()
    rdf$from_east = integer()
    rdf$from_west = integer()
    rdf$from_near_east = integer()
    rdf$from_near_west = integer()
    
    rdf$to_x = integer()
    rdf$to_east = integer()
    rdf$to_west = integer()
    rdf$to_near_east = integer()
    rdf$to_near_west = integer()
    
    rdf$to_south = integer()
    rdf$to_north = integer()
    rdf$from_south = integer()
    rdf$from_north = integer()
    rdf$to_y = integer()
    rdf$from_y = integer()
    rdf$x = integer()
  }
  
  if (nrow(odf) > 0L) {
    odf = merge(odf, ndf, by.x = "from", by.y = "name")
    parsed_paths$outflow_df = odf
  }
  if (nrow(idf) > 0L) {
    idf = merge(idf, ndf, by.x = "to", by.y = "name")
    parsed_paths$inflow_df = idf
  }
  
  parsed_paths$nodes_df = ndf
  parsed_paths$edges_df = edf
  parsed_paths$removed_edges_df = rdf
  parsed_paths
}


compute_adjacency_matrix <- function(df) {
  # Extract unique nodes from the 'from' and 'to' columns
  nodes <- unique(c(df$from, df$to))
  
  # Create an empty square matrix with dimensions equal to the number of unique nodes
  adj_matrix <- matrix(0, nrow = length(nodes), ncol = length(nodes),
                       dimnames = list(nodes, nodes))
  
  # Fill the matrix: set 1 if there's an edge between 'from' and 'to'
  for (i in seq_len(nrow(df))) {
    adj_matrix[df$from[i], df$to[i]] <- 1
    adj_matrix[df$to[i], df$from[i]] <- 1
  }
  
  return(adj_matrix)
}
