topological_sort_engine = function(flows, state_nms, warn_not_dag = TRUE) {
  has_inflow = function(focal_state, to_states, state_nms) {
    stopifnot(focal_state %in% state_nms)
    focal_state %in% to_states
  }
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
      if (warn_not_dag) {
        warning(
         "\nState network is not acyclic (i think),",
         "\nand therefore cannot be topologically sorted.",
         "\nDefaulting to original order where necessary."
        )
      }
      state_order = c(state_order, state_nms) |> unique()
      state_nms = character(0L)
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

#' Flow Frame (experimental)
#' 
#' Get a data frame representing the flows in a model specification.
#' 
#' @param spec A \code{\link{mp_tmb_model_spec}}.
#' @param topological_sort Should the states be topologically sorted to
#' respect the main direction of flow?
#' @param warn_not_dag Should a warning be thrown if the flow model is  
#' not a directed acyclic graph. This is only relevant if `topological_sort`
#' is used.
#' @returns A data frame that gives information provided in calls to
#' \code{\link{mp_per_capita_flow}} and \code{\link{mp_per_capita_inflow}}.
#' 
#' @export
mp_flow_frame = function(spec, topological_sort = TRUE, warn_not_dag = TRUE) {
  cf = spec$change_model$change_frame()
  ff = spec$change_model$flow_frame()
  sv = unique(cf$state)
  to = cf[startsWith(cf$change, "+"), , drop = FALSE]
  from = cf[startsWith(cf$change, "-"), , drop = FALSE]
  to$change = sub("^\\+", "", to$change) |> macpan2:::reset_rownames()
  from$change = sub("^\\-", "", from$change) |> macpan2:::reset_rownames()
  to_only = to[to$change %in% setdiff(to$change, from$change), , drop = FALSE]
  from_only = from[from$change %in% setdiff(from$change, to$change), , drop = FALSE]
  flows = merge(
      merge(from, to, by = "change", suffixes = c(".from", ".to"))
    , ff, by = "change"
  )[, c("state.from", "state.to", "change", "rate"), drop = FALSE]
  inflows = merge(to_only, ff, by = "change")[, c("size", "state", "change", "rate"), drop = FALSE]
  outflows = merge(from_only, ff, by = "change")[, c("size", "state", "change", "rate"), drop = FALSE]
  names(flows) = c("from", "to", "name", "rate")
  names(inflows) = c("from", "to", "name", "rate")
  names(outflows) = c("from", "to", "name", "rate")
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
    topo = topological_sort_engine(flows, sv, warn_not_dag)
    flows = flows[order(factor(flows$from, levels = topo)), , drop = FALSE]
  }
  flows = rbind(flows, inflows, outflows)
  return(flows)
}

#' State Variables
#' 
#' Get the state variables in a model specification.
#' 
#' @param spec Model specification (\code{\link{mp_tmb_model_spec}}).
#' 
#' @return Character vector of names of all state variables that have been
#' explicitly represented in the model using functions like
#' \code{\link{mp_per_capita_flow}}.
#' 
#' @export
mp_state_vars = function(spec) {
  vapply(spec$change_model$update_state(), lhs_char, character(1L))
}


#' Find all Paths
#' 
#' Find all paths through a compartmental model.
#' 
#' @param edges_df A data frame with a `from` and a `to` column.
#' @param start_node_guesses Optional guesses for nodes that start
#' paths. This is useful for models that are not directed acyclic 
#' graphs (DAGs).
#' 
#' @return List of character vectors of state variable names, each
#' vector describing a path through the model.
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
