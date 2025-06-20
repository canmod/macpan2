## maybe one day these will be in the package but for now they are experimental
## being used only to support readme files and maybe vignettes

require("ggplot2")


#' Plot Flow Diagrams (experimental)
#' 
#' @param flows Data frame of flows produced by \code{\link{mp_flow_frame}}.
#' @param layout A character matrix with 
plot_flow_diagram = function(layout
    , pattern_node_filter = "", pattern_edge_filter = ""
    , pattern_node_mutate = "", pattern_edge_mutate = ""
    , pattern_node_replace = "", pattern_edge_replace = ""
    , show_flow_rates = FALSE, show_flow_names = FALSE
    , state_dependence = data.frame()
    , size = 6
    , size_arrow = 3
    , state_colour = "#002B5B", box_colour = "#EAF6FF"
    , line_colour = state_colour, flow_colour = state_colour
    , outline_colour = state_colour
  ) {
  edges = layout$edges_flows()
  nodes = layout$nodes()
  edges = edges[grepl(pattern_edge_filter, edges$name), , drop = FALSE]
  nodes = nodes[grepl(pattern_node_filter, nodes$state), , drop = FALSE]
  edges$name = sub(pattern_edge_mutate, pattern_edge_replace, edges$name)
  nodes$state = sub(pattern_node_mutate, pattern_node_replace, nodes$state)
  p = ggplot()
  if (nrow(state_dependence) > 0L) {
    state_dependence = merge(layout$nodes(), state_dependence, by = "state") |> merge(layout$edges_flows(), by.x = "flow", by.y = "name", suffix = c("_state", "_flow"))
    p = p + geom_segment(aes(x = x_state, y = y_state, xend = x_flow, yend = y_flow), data = state_dependence, lty = 2, colour = "blue")
  }
  p = (p
    + geom_segment(
        aes(x = x_from, xend = x_to, y = y_from, yend = y_to)
      , data = edges
      , arrow = mp_flow_arrow(size_arrow)
      , colour = line_colour
    )
    + geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
      , fill = box_colour
      , colour = outline_colour
      , data = nodes
    )
    + geom_text(
        aes(x, y, label = state)
      , size = size
      , data = nodes
      , colour = state_colour
    )
    + mp_ggtheme
  )
  if (show_flow_rates) {
    p = p + geom_label(
        aes(x = x, y = y, label = rate)
      , data = edges
      , colour = flow_colour
      , parse = TRUE
      , size = size
    )
  }
  if (show_flow_names) {
    p = p + geom_label(
        aes(x = x, y = y, label = name)
      , data = edges
      , colour = "blue"
      , parse = TRUE
    )
  }
  return(p)
}

draw_outside = function(plot, edges
    , x = "xmin", y = "ymax", lab = "name"
    , x_dir = "west", y_dir = "north"
    , pattern_filter = ""
    , pattern_mutate = ""
    , pattern_replace = ""
    , show_labels = FALSE
    , point_away = TRUE
    , size_arrow = 3
  ) {
  x_dir = (x_dir == "east" ) - (x_dir == "west" )
  y_dir = (y_dir == "north") - (y_dir == "south")
  x_delta = layout$x_gap * x_dir
  y_delta = layout$y_gap * y_dir
  
  edges = edges[grepl(pattern_filter, edges$name), , drop = FALSE]
  
  x = edges[[x]]
  y = edges[[y]]
  if (point_away) {
    edges$x = x
    edges$y = y
    edges$xend = x + x_delta
    edges$yend = y + y_delta
  } else {
    edges$xend = x
    edges$yend = y
    edges$x = x - x_delta
    edges$y = y - y_delta
  }
  edges$xlab = 0.5 * (edges$x + edges$xend)
  edges$ylab = 0.5 * (edges$y + edges$yend)
  edges$lab = sub(pattern_mutate, pattern_replace, edges[[lab]])
  
  p = (plot
    + geom_segment(
        aes(x = x, y = y, xend = xend, yend = yend)
      , data = edges
      , arrow = mp_flow_arrow(size_arrow)
      , colour = "blue"
    )
  )
  if (show_labels) {
    p = p + geom_label(
        aes(x = xlab, y = ylab, label = lab)
      , data = edges
      , colour = "blue"
      , parse = TRUE
    )
  }
  return(p)
}
draw_outflows = function(plot, layout
    , x = "xmin", y = "ymax", lab = "name"
    , x_dir = "west", y_dir = "north"
    , pattern_filter = ""
    , pattern_mutate = ""
    , pattern_replace = ""
    , show_labels = FALSE
) {
  draw_outside(plot
    , layout$edges_outflows()
    , x, y, lab, x_dir, y_dir
    , pattern_filter
    , pattern_mutate
    , pattern_replace
    , show_labels
    , point_away = TRUE
  )
}
draw_inflows = function(plot, layout
    , x = "xmin", y = "y", lab = "name"
    , x_dir = "east", y_dir = "central"
    , pattern_filter = ""
    , pattern_mutate = ""
    , pattern_replace = ""
    , show_labels = FALSE
) {
  draw_outside(plot
    , layout$edges_inflows()
    , x, y, lab, x_dir, y_dir
    , pattern_filter
    , pattern_mutate
    , pattern_replace
    , show_labels
    , point_away = FALSE
  )
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
mp_flow_arrow = function(size_mm) arrow(length = unit(size_mm, 'mm'))
