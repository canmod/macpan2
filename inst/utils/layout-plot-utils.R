mp_layout_path <- function(
      edges_df
    , sort_paths = TRUE
    , combine_columns = TRUE
    , deduplicate_edges = TRUE
    , align_dir_flows = TRUE
    , resolve_nodes = character()
    , keep_paths = integer()
  ) {
  
  inflow_df = edges_df[edges_df$type == "inflow", , drop = FALSE]
  outflow_df = edges_df[edges_df$type == "outflow", , drop = FALSE]
  edges_df = edges_df[edges_df$type == "flow", , drop = FALSE]
  
  all_edges_df = edges_df[edges_df$type == "flow", , drop = FALSE]
  edges_df = edges_df[!edges_df$to %in% resolve_nodes, , drop = FALSE]
  removed_edges_df = all_edges_df[all_edges_df$to %in% resolve_nodes, , drop = FALSE]
  paths <- find_all_paths(edges_df, resolve_nodes)
  if (length(paths) == 0L) stop("model has no paths!")
  if (length(paths) == 1L) sort_paths = FALSE
  if (length(keep_paths) > 0L) paths = paths[keep_paths]
  if (align_dir_flows) {
    bidir_edges = find_bidirectional_edges(edges_df)
    if (length(bidir_edges) > 0) {
      paths <- split_bidirectional_paths(paths, bidir_edges)
    }
  }
  
  # Step 1: Identify all unique nodes from the edges and sort them topologically
  all_nodes <- unique(c(edges_df$from, edges_df$to))
  resolve_nodes = intersect(all_nodes, resolve_nodes)
  all_nodes = c(resolve_nodes, setdiff(all_nodes, resolve_nodes))
  all_nodes = macpan2:::topological_sort_engine(edges_df, all_nodes, warn_not_dag = !align_dir_flows)
  
  # Step 2: Pad each path by aligning nodes to their respective positions
  if (align_dir_flows) {
    temp_fn = function(edges_df, bidir_edges, paths, nodes) {
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
      padded_path_matrix = padded_path_matrix[!duplicated(padded_path_matrix), , drop = FALSE]
      return(padded_path_matrix)
    }
    padded_paths_matrix = temp_fn(edges_df, bidir_edges, paths, all_nodes)
  } else {
    padded_paths <- lapply(paths, function(path) {
      padded <- rep("", length(all_nodes))  # Create an empty vector of the same length as all_nodes
      for (i in seq_along(path)) {
        node <- path[i]
        padded[which(all_nodes == node)] <- node  # Place each node in its correct position
      }
      return(padded)
    })
    
    # Convert the list of padded paths to a matrix for easy visualization
    padded_paths_matrix <- do.call(rbind, padded_paths)
  }
  
  if (sort_paths) {
    i = order_corresp(padded_paths_matrix)
    padded_paths_matrix = padded_paths_matrix[i, , drop = FALSE]
    paths = paths[i]
  }
  
  box_heights = data.frame(
      name = unique(padded_paths_matrix[padded_paths_matrix != ""])
    , min_path_id = vapply(apply(padded_paths_matrix != "", 2L, which, simplify = FALSE), min, integer(1L))
    , max_path_id = vapply(apply(padded_paths_matrix != "", 2L, which, simplify = FALSE), max, integer(1L))
  )
  
  path_to_edges = function(path) data.frame(from = path[-length(path)], to = path[-1])
  path_ids = lapply(paths, path_to_edges) |> bind_rows(.id = "path_id")
  path_ids$path_id = as.numeric(path_ids$path_id)
  
  ## only need one edge per node-pair
  if (deduplicate_edges) {
    i = duplicated(path_ids[, c("to", "from")])
    path_ids = path_ids[!i, , drop = FALSE]
  }
  if (combine_columns) {
    padded_paths_matrix = combine_adjacent_columns(padded_paths_matrix)
  }
  
  node_grp_ids = data.frame(
      name = all_nodes
    , node_grp_ids = vapply(all_nodes, \(x) which(apply(padded_paths_matrix == x, 2L, any)), integer(1L))
  )
  
  edges_df = merge(edges_df, path_ids, by = c("from", "to")) |> merge(node_grp_ids, by.x = "from", by.y = "name") |> merge(node_grp_ids, by.x = "to", by.y = "name", suffix = c("_from", "_to"))
  nodes_df = merge(box_heights, node_grp_ids, by = "name")
  
  path_twice = c(edges_df$path_id, edges_df$path_id)
  from_to = c(edges_df$from, edges_df$to)
  
  min_paths = tapply(path_twice, from_to, min)
  max_paths = tapply(path_twice, from_to, max)
  
  if (deduplicate_edges) {
    nodes_df$min_path_id = min_paths[nodes_df$name]
    nodes_df$max_path_id = max_paths[nodes_df$name]
  }
  
  if (nrow(removed_edges_df) > 0L) {
    removed_edges_df = merge(removed_edges_df, nodes_df, by.x = "from", by.y = "name") |> merge(nodes_df, by.x = "to", by.y = "name", suffix = c("_from", "_to"))
  }
  
  return(nlist(paths, padded_paths_matrix, edges_df, nodes_df, removed_edges_df, inflow_df, outflow_df))
}

mp_plot_layout = function(layout, size = 6) {
  p = (ggplot()
    + geom_segment(
        aes(x = from_east, xend = to_west, y = y, yend = y)
      , data = layout$edges_df
      , arrow = mp_flow_arrow
      , colour = "blue"
    )
    + geom_rect(aes(xmin = west, xmax = east, ymin = north, ymax = south)
      , fill = "lightblue"
      , data = layout$nodes_df
    )
    + geom_text(
        aes(x, y, label = name)
      , size = size
      , data = layout$nodes_df
      , colour = "blue"
    )
    + mp_ggtheme
  )
  p
}
mp_ggtheme = theme(
  axis.line = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  axis.title = element_blank(),
  panel.background = element_blank(),
  panel.grid = element_blank(),
  plot.background = element_blank()
)
mp_flow_arrow = arrow(length = unit(3, 'mm'))

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
