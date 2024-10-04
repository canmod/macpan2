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
