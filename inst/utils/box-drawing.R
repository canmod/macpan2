require("ggplot2")

#' Plot Flow Diagrams (experimental)
#' 
#' @param flows Data frame of flows produced by \code{\link{mp_flow_frame}}.
#' @param layout A character matrix with 
plot_flow_diagram = function(layout, show_flow_rates = FALSE, size = 6) {
  edges = layout$edges_flows()
  nodes = layout$nodes()
  p = (ggplot()
    + geom_segment(
        aes(x = x_from, xend = x_to, y = y_from, yend = y_to)
      , data = edges
      , arrow = mp_flow_arrow
      , colour = "blue"
    )
    + geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
      , fill = "lightblue"
      , data = nodes
    )
    + geom_text(
        aes(x, y, label = state)
      , size = size
      , data = nodes
      , colour = "blue"
    )
    + mp_ggtheme
  )
  if (show_flow_rates) {
    p = p + geom_label(
        aes(x = x, y = y, label = rate)
      , data = layout$edges_flows()
      , colour = "blue"
      , parse = TRUE
    )
  }
  return(p)
}

draw_outflows = function(plot, layout, show_flow_rates = FALSE) {
  p = (plot
    + geom_segment(
        aes(x = xmin, y = ymax, xend = xend, yend = yend)
      , data = layout$edges_outflows()
      , arrow = mp_flow_arrow
      , colour = "blue"
    )
  )
  if (show_flow_rates) {
    p = p + geom_label(
        aes(x = xlab, y = ylab, label = rate)
      , data = layout$edges_outflows()
      , colour = "blue"
      , parse = TRUE
    )
  }
  return(p)
}
draw_inflows = function(plot, layout, show_flow_rates = FALSE) {
  p = (plot
    + geom_segment(
        aes(x = xstart, y = y, xend = xmin, yend = y)
      , data = layout$edges_inflows()
      , arrow = mp_flow_arrow
      , colour = "blue"
    )
  )
  if (show_flow_rates) {
    p = p + geom_label(
        aes(x = xlab, y = y, label = rate)
      , data = layout$edges_inflows()
      , colour = "blue"
      , parse = TRUE
    )
  }
  return(p)
}
draw_conversions = function(plot, layout) {}

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
