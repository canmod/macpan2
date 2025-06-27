## takes output from make_liksurf in surfplot.R.
## here so we do not need to import ggplot2 functions
plot_liksurf <- function(dd, arrows = TRUE, contours = TRUE,
                    arrow_len = 0.05,
                    arrow_thin = 5) {
    require(ggplot2)
    gg0 <- (ggplot(dd, aes(x, y))
        + geom_tile(aes(fill = z))
        + theme_bw()
        + scale_x_continuous(expand = c(0,0), limits = range(dd$x))
        + scale_y_continuous(expand = c(0,0), limits = range(dd$y))
        + scale_fill_continuous(trans = "log10", type = "viridis")

    )
    if (contours) {
        gg0 <- gg0 + geom_contour(aes(z = z), colour = "grey")
    }
    if (arrows) {
        gg0 <- gg0 + geom_segment(
                         data = dd[seq(nrow(dd)) %% arrow_thin == 0 , ],
                         aes(xend = x - gx, yend = y - gy), 
                         arrow = arrow(length = unit(arrow_len, "inches")), 
                         colour = 'white'
                     )
    }
    gg0 = gg0 + geom_point(
        aes(x, y)
      , data = filter(dd, z == min(z))
      , colour = "red"
    )
    gg0
}
