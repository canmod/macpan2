topological_sort_engine = function(flows, state_nms, warn_not_dag = TRUE) {
  has_inflow = function(focal_state, to_states, state_nms) {
    stopifnot(focal_state %in% state_nms)
    focal_state %in% to_states
  }
  if (warn_not_dag) warned_once = FALSE
  if (missing(state_nms)) state_nms = unique(c(flows$from, flows$to))
  state_order = c()
  n = length(state_nms)
  for (i in 1:n) {
    if (length(state_nms) == 0L) break
    remaining_inflow = sapply(
      state_nms,
      has_inflow,
      flows$to,
      state_nms
    )
    state_order = c(state_order, state_nms[!remaining_inflow])
    flows = flows[!flows$to %in% state_order, , drop = FALSE]
    flows = flows[!flows$from %in% state_order, , drop = FALSE]
    state_nms = state_nms[remaining_inflow]
    is_acyclic = (
        isTRUE(all(remaining_inflow))
      & length(state_nms) != 0L
      & length(remaining_inflow) != 0L
    )
    if (is_acyclic) {
      if (warn_not_dag & !warned_once) {
        warning(
         "\nFlow among states contains cycles. This means",
         "\nthat states can be revisited, and therefore",
         "\ncannot be unambiguously sorted to respect the",
         "\ndirection of all flows.",
         "\nDefaulting to original order where necessary."
        )
        warned_once = TRUE
      }
      
      ## assume the next state name is in the right order, but move
      ## on and try to sort the rest
      state_order = c(state_order, state_nms[1L]) |> unique()
      state_nms = state_nms[-1L]
    }
  }
  state_order
}

topological_sort = function(spec, warn_not_dag = TRUE) {
  topological_sort_engine(
      mp_flow_frame(spec)
    , mp_state_vars(spec)
    , warn_not_dag = warn_not_dag
  )
}

#' @param loops States used to ignore any flows that have those
#' states as the `to` compartment, usually to create a DAG so that topological
#' sort is valid.
#' @noRd
topological_sort_general = function(flows, loops = "^$", all_states = NULL) {
  if (is.null(all_states)) all_states = unique(c(flows$from, flows$to))
  flows = flows[flows$type == "flow", , drop = FALSE]
  flows = flows[!grepl(loops, flows$name), , drop = FALSE]
  states = unique(c(flows$from, flows$to))
  missing_states = setdiff(all_states, states)
  states = setdiff(all_states, missing_states)
  sorted_states = topological_sort_engine(flows, states, warn_not_dag = TRUE)
  c(sorted_states, missing_states)
}

## @param states in sorted order
is_cyclic = function(flows, states) {
  from = as.numeric(factor(flows$from, levels = states))
  to = as.numeric(factor(flows$to, levels = states))
  to <= from
}

#' Data Frame Describing Compartmental Model Flows
#' 
#' Get a data frame where each row represents a flow in a model specification.
#' 
#' @param spec A \code{\link{mp_tmb_model_spec}}.
#' @param topological_sort Should the states be topologically sorted to
#' respect the main direction of flow?
#' @param loops Pattern for matching the names of flows that make 
#' the flow model not a DAG, which is a critical assumption when topologically 
#' sorting the order of states and flows in the output. This is only relevant if 
#' `topological_sort` is used.
#' @returns A data frame that gives information provided in calls to
#' \code{\link{mp_per_capita_flow}} and \code{\link{mp_per_capita_inflow}}.
#' 
#' @export
mp_flow_frame = function(spec, topological_sort = TRUE, loops = "^$") {
  cf = spec$change_model$change_frame()
  ff = spec$change_model$flow_frame()
  states = unique(cf$state)
  to = cf[startsWith(cf$change, "+"), , drop = FALSE]
  from = cf[startsWith(cf$change, "-"), , drop = FALSE]
  to$change = sub("^\\+", "", to$change) |> reset_rownames()
  from$change = sub("^\\-", "", from$change) |> reset_rownames()
  to_only = to[to$change %in% setdiff(to$change, from$change), , drop = FALSE]
  from_only = from[from$change %in% setdiff(from$change, to$change), , drop = FALSE]
  flows = merge(
      merge(from, to, by = "change", suffixes = c(".from", ".to"))
    , ff, by = "change"
  )[, c("state.from", "state.to", "change", "rate"), drop = FALSE]
  inflows = merge(to_only, ff, by = "change")[, c("size", "state", "change", "rate"), drop = FALSE]
  outflows = merge(from_only, ff, by = "change")[, c("size", "state", "change", "rate"), drop = FALSE]
  fn = c("from", "to", "name", "rate")
  names(flows) = fn
  names(inflows) = fn
  names(outflows) = fn
  if (nrow(flows) > 0L) {
    flows$type = "flow"
    flows$from_name = flows$from
    flows$to_name = flows$to
  }
  if (nrow(inflows) > 0L) {
    inflows$type = "inflow"
    inflows$from_name = ""
    inflows$to_name = inflows$to
  }
  if (nrow(outflows) > 0L) {
    outflows$type = "outflow"
    outflows$to = outflows$name
    outflows$to_name = ""
    outflows$from_name = outflows$from
  }
  if (topological_sort) {
    topo = topological_sort_general(flows, loops, states)
    flows = flows[order(factor(flows$from, levels = topo)), , drop = FALSE]
  }
  flows = rbind(flows, inflows, outflows)
  flows$has_from_state = flows$from %in% states
  flows$has_to_state = flows$to %in% states
  return(flows)
}

#' Data Frame Describing State Dependent Per-Capita Flow Rates
#' 
#' Data frame giving states that per-capita flow rates directly depend on.
#' This is intended for plotting diagrams and not for mathematical analysis,
#' in that it does not describe indirect dependence for flow rates on state
#' variables.
#' 
#' @param spec Model specification from \code{\link{spec}}.
#' 
#' @export
mp_state_dependence_frame = function(spec) {
  ff = mp_flow_frame(spec, topological_sort = FALSE)
  ss = mp_state_vars(spec)
  (ff$rate
    |> vars_in_char() 
    |> lapply(intersect, ss) 
    |> setNames(ff$name) 
    |> melt_list_of_char_vecs()
    |> setNames(c("state", "flow"))
    |> as.data.frame()
  )
}

#' Dynamic Variable Names
#' 
#' Get the state, flow, and other dynamic variables in a model specification.
#' Dynamic variables are any variable that is updated every time step. 
#' State and flow variables are special dynamic variables involved in 
#' compartmental models that have been explicitly represented using functions 
#' like \code{\link{mp_per_capita_flow}} that define flows among compartments
#' (i.e., states).
#' 
#' State and flow variables will be identical regardless of the state update 
#' method (e.g., \code{\link{mp_rk4}}), but other dynamic variables might
#' appear for one particular state update method that does not appear for 
#' another. For example, the first Runge Kutta step for a state in a model 
#' with an \code{\link{mp_rk4}} updater, will not appear with an
#' \code{\link{mp_euler}} updater.
#' 
#' @param spec Model specification (\code{\link{mp_tmb_model_spec}}).
#' @param topological_sort Should the states and flows be 
#' [topologically sorted](https://en.wikipedia.org/wiki/Topological_sorting) to
#' respect the main direction of flow? The default is no topological sorting,
#' which differs from \code{\link{mp_flow_frame}}.
#' @param loops Pattern for matching the names of flows that make 
#' the flow model not a DAG, which is a critical assumption when topologically 
#' sorting the order of states and flows in the output. This is only relevant if 
#' `topological_sort` is used.
#' @param trans Add a prefix to the names for indicating if a transformed
#' version of the variables is preferred.
#' 
#' @return Character vector of names of all requested variables.
#' 
#' @examples
#' si = mp_tmb_library("starter_models", "si", package = "macpan2")
#' (si
#'   |> mp_simulator(time_steps = 5L, mp_state_vars(si))
#'   |> mp_trajectory()
#' )
#' 
#' @name mp_vars
NULL

#' @describeIn mp_vars Return character vector of all state variables.
#' @export
mp_state_vars = function(spec, topological_sort = FALSE, loops = "^$", trans = "") {
  states = vapply(spec$change_model$update_state(), lhs_char, character(1L))
  flows = mp_flow_frame(spec, topological_sort = FALSE)
  if (topological_sort) states = topological_sort_general(flows, loops, states)
  if (nchar(trans) > 0) states = sprintf("%s_%s", trans, states)
  return(states)
}

#' @describeIn mp_vars Return the names of all variables that contain
#' the absolute flow between compartments.
#' The absolute flow is the magnitude of a flow per time step.
#' @export
mp_flow_vars = function(spec, topological_sort = FALSE, loops = "^$", trans = "") {
  if (topological_sort) {
    flows = mp_flow_frame(spec, topological_sort, loops)
    flow_vars = flows$name
  } else {
    flow_vars = spec$change_model$flow_frame()$change
  }
  if (nchar(trans) > 0) flow_vars = sprintf("%s_%s", trans, flow_vars)
  return(flow_vars)
}

#' @describeIn mp_vars Union of `mp_state_vars()` and `mp_flow_vars()`.
#' @export
mp_state_flow_vars = function(spec, topological_sort = FALSE, loops = "^$", trans = "") {
  c(
      mp_state_vars(spec, topological_sort, loops, trans)
    , mp_flow_vars(spec, topological_sort, loops, trans)
  )
}

#' @describeIn mp_vars All variables that are updated once per time-step.
#' @export
mp_dynamic_vars = function(spec) spec$all_dynamic_vars()

#' @describeIn mp_vars All variables that are updated once per time-step,
#' excluding those that are state and flow variables.
#' @export
mp_other_dynamic_vars = function(spec) {
  setdiff(
      mp_dynamic_vars(spec)
    , mp_state_flow_vars(spec)
  )
}

#' Data Frame Describing Each Change to Each State Variable
#' 
#' Get a data frame with one row for each change made to each state variable 
#' at each time step.
#' 
#' @param spec Model specification (\code{\link{mp_tmb_model_spec}}).
#' 
#' @return Data frame with two columns: `state` and `change`. Each row
#' describes one change.
#' 
#' @examples
#' ("starter_models"
#'   |> mp_tmb_library("sir", package = "macpan2") 
#'   |> mp_change_frame()
#' )
#' 
#' @export
mp_change_frame = function(spec) spec$change_model$change_frame()



#' Find all Paths Through Compartments
#' 
#' Find all paths through a compartmental model.
#' 
#' @param edges_df A data frame with a `from` and a `to` column, which can
#' be extracted from a model specification object using the 
#' \code{\link{mp_flow_frame}}.
#' @param start_node_guesses Optional guesses for nodes that start
#' paths. This is useful for models that are not directed acyclic 
#' graphs (DAGs).
#' 
#' @return List of character vectors of state variable names, each
#' vector describing a path through the model.
#' 
#' @examples
#' spec = mp_tmb_library("starter_models", "macpan_base", package = "macpan2")
#' spec |> mp_flow_frame() |> find_all_paths()
#' 
#' @export
find_all_paths <- function(edges_df, start_node_guesses = character(0L)) {
  # Convert the data frame to an adjacency list
  adj_list <- split(edges_df$to, edges_df$from)
  all_nodes = unique(c(edges_df$from, edges_df$to))
  
  # Function to perform DFS from a given node
  dfs_paths <- function(node, visited, current_path) {
    # Mark the current node as visited
    visited <- c(visited, node)
    current_path <- c(current_path, node)
    
    # If there are no more nodes to explore, return the current path
    if (is.null(adj_list[[node]])) {
      return(list(current_path))
    }
    
    # Recursively explore each neighbor
    all_paths <- list()
    for (neighbor in adj_list[[node]]) {
      if (!(neighbor %in% visited)) {
        paths_from_neighbor <- dfs_paths(neighbor, visited, current_path)
        all_paths <- c(all_paths, paths_from_neighbor)
      }
    }
    if (length(all_paths) == 0L) {
      if (length(current_path) != 0L) {
        all_paths = list(as.character(current_path))
      }
    }
    
    return(all_paths)
  }
  
  # Find all start nodes (nodes without incoming edges)
  start_nodes <- setdiff(unique(edges_df$from), unique(edges_df$to))
  if (length(start_nodes) == 0L) {
    start_nodes = intersect(start_node_guesses, all_nodes)
    if (length(start_nodes) == 0L) stop("no start nodes found. please add to start_node_guesses")
  }
  
  # Traverse from each start node and collect all paths
  all_paths <- list()
  for (start_node in start_nodes) {
    paths_from_node <- dfs_paths(start_node, visited = character(), current_path = character())
    all_paths <- c(all_paths, paths_from_node)
  }
  
  return(all_paths)
}


combine_adjacent_columns <- function(padded_paths_matrix) {
  if (!is.matrix(padded_paths_matrix)) return(matrix(padded_paths_matrix, nrow = 1L))
  if (all(padded_paths_matrix != "")) return(padded_paths_matrix)
  if (nrow(padded_paths_matrix) == 1L) {
    warning("one-row matrix with blanks. this indicates that something went wrong.")
    return(padded_paths_matrix)
  }
  combined_matrix <- padded_paths_matrix
  i <- 1
  
  # Iterate through adjacent columns and check the condition
  while (i < ncol(combined_matrix)) {
    column1 <- combined_matrix[, i]
    column2 <- combined_matrix[, i + 1]
      
    # Check if non-blank in column1 corresponds to blanks in column2 and vice versa
    if (is_good_sort(column1) & is_good_sort(column2) & (all((column1 != "" & column2 == "") | (column1 == "" & column2 != "") | (column1 == "" & column2 == "")))) {
      # Combine the two columns: Keep the non-blank value where it appears
      combined_column <- ifelse(column1 != "", column1, column2)
      
      # Remove the two columns and replace with the combined column
      if (i == 1) {
        combined_matrix <- cbind(
          combined_column,
          combined_matrix[, (i+2):ncol(combined_matrix), drop=FALSE]
        )
      } else if (i + 1 == ncol(combined_matrix)) {
        combined_matrix <- cbind(
          combined_matrix[, 1:(i-1), drop=FALSE],
          combined_column
        )
      } else {
        combined_matrix <- cbind(
          combined_matrix[, 1:(i-1), drop=FALSE],
          combined_column,
          combined_matrix[, (i+2):ncol(combined_matrix), drop=FALSE]
        )
      }
      
      # Stay at the same index because we have just replaced two columns with one
    } else {
      # Move to the next column if they can't be merged
      i <- i + 1
    }
  }
  colnames(combined_matrix) = NULL
  
  return(combined_matrix)
}

is_good_sort <- function(x) {
  # Remove consecutive empty strings
  x_no_empty_run <- x[x != "" | c(TRUE, diff(x != "") != 0)]
  
  # Remove all empty strings from the reduced vector to focus only on non-empty ones
  reduced <- rle(x_no_empty_run)$values
  reduced_non_empty <- reduced[reduced != ""]
  
  # Compare the unique non-empty values of the original vector with the reduced non-empty vector
  identical(unique(x[x != ""]), reduced_non_empty)
}

order_corresp <- function(data_matrix) {
  # Ensure the input is a matrix
  if (!is.matrix(data_matrix)) stop("Input must be a matrix.")
  if (is.character(data_matrix)) data_matrix = data_matrix != ""
  
  # Step 1: Compute the total sum of the matrix
  grand_total <- sum(data_matrix)
  
  # Step 2: Compute row and column sums
  row_sums <- rowSums(data_matrix)
  col_sums <- colSums(data_matrix)
  
  # Step 3: Compute expected frequencies
  expected <- outer(row_sums, col_sums) / grand_total
  
  # Step 4: Standardized residuals
  standardized_residuals <- (data_matrix - expected) / sqrt(expected)
  
  # Step 5: Perform Singular Value Decomposition (SVD)
  svd_result <- svd(standardized_residuals)
  
  # Step 6: Compute row scores (principal coordinates for rows)
  row_scores <- svd_result$u %*% diag(svd_result$d)
  
  return(order(row_scores[, 1L, drop = TRUE]))
}

# Function to extract minimum row and column indices for each node in the matrix
get_node_positions <- function(matrix_input) {
  # Initialize an empty list to store results
  node_positions <- list()
  
  # Loop through the matrix to find the position of each node
  for (i in 1:nrow(matrix_input)) {
    for (j in 1:ncol(matrix_input)) {
      node <- matrix_input[i, j]
      if (node != "") {
        if (!node %in% names(node_positions)) {
          # Store the node with its minimum row and column index
          node_positions[[node]] <- c(min_row = i, min_col = j)
        }
      }
    }
  }
  
  # Convert the list to a data frame
  result_df <- data.frame(
    node_name = names(node_positions),
    min_row_index = as.numeric(sapply(node_positions, function(x) x["min_row"])),
    min_col_index = as.numeric(sapply(node_positions, function(x) x["min_col"])),
    row.names = NULL
  )
  
  return(result_df)
}


find_bidirectional_edges <- function(edges) {
  bidirectional_pairs <- list()
  for (i in 1:nrow(edges)) {
    from_node <- edges$from[i]
    to_node <- edges$to[i]
    if (any(edges$from == to_node & edges$to == from_node)) {
      bidirectional_pairs[[length(bidirectional_pairs) + 1]] <- c(from_node, to_node)
    }
  }
  return(unique(bidirectional_pairs))
}
split_bidirectional_paths <- function(paths, bidirectional_pairs) {
  # Convert bidirectional pairs to a list of sets for easier checking
  bidirectional_sets <- lapply(bidirectional_pairs, function(x) setNames(x, NULL))
  
  # Function to check if two nodes form a bidirectional pair
  is_bidirectional_pair <- function(node1, node2, bidirectional_sets) {
    any(sapply(bidirectional_sets, function(set) all(c(node1, node2) %in% set)))
  }
  
  # Initialize the result list
  result_paths <- list()
  
  # Iterate through each path
  for (path in paths) {
    current_path <- character() # The current path we're building
    new_paths <- list()         # Store additional split paths if needed
    i <- 1
    
    while (i < length(path)) {
      current_path <- c(current_path, path[i])
      
      # Check if the current node and the next node form a bidirectional pair
      if (is_bidirectional_pair(path[i], path[i+1], bidirectional_sets)) {
        # If they do, split the path
        new_paths[[length(new_paths) + 1]] <- path[(i+1):length(path)]
        break
      }
      
      i <- i + 1
    }
    
    # Add the current path and any new paths to the result
    result_paths[[length(result_paths) + 1]] <- current_path
    if (length(new_paths) > 0) {
      result_paths <- c(result_paths, new_paths)
    }
  }
  
  return(result_paths)
}

find_node_groups = function(nodes, bidir_edges) {
  grp_map = setNames(nodes, nodes)
  for (node in nodes) {
    for (edge in bidir_edges) {
      if (node %in% edge) {
        grp_map[[node]] = paste(edge, collapse = "<-->")
        break
      }
    }
  }
  grp_map
}

pad_with_bidir = function(edges_df) {
  bidir_edges = find_bidirectional_edges(edges_df)
  paths <- split_bidirectional_paths(
      find_all_paths(edges_df)
    , bidir_edges
  )
  nodes = topological_sort_engine(edges_df, warn_not_dag = FALSE)
  group_map = find_node_groups(nodes, bidir_edges)
  groups = unique(group_map)
  
  padded_path_matrix = matrix("", nrow = length(paths), ncol = length(groups))
  paths_with_groups = lapply(paths, \(path) group_map[path])
  
  values = unlist(paths)
  grouped_values = unlist(paths_with_groups, use.names = FALSE)
  indexes = cbind(
      rep(seq_along(paths), times = vapply(paths, length, integer(1L)))
    , match(grouped_values, groups)
  )
  padded_path_matrix[indexes] = values
  
  return(padded_path_matrix)
}

edges_list = function(edges_df) {
  apply(
      edges_df[, c("from", "to"), drop = FALSE]
    , 1L
    , unname
    , simplify = FALSE
  ) |> unname()
}
