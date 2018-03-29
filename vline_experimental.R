# vline_experimental.R

library(tidyverse)
library(grid)
library(ggiraph)




# ==== GeomInteractiveSegment ====


GeomInteractiveSegment <- ggproto("GeomSegment", Geom,
                                  draw_panel = function(data, panel_scales, coord, arrow = NULL,
                                                        lineend = "butt", na.rm = FALSE) {
                                        
                                        data <- remove_missing(data, na.rm = na.rm,
                                                               c("x", "y", "xend", "yend", "linetype", "size", "shape", "tooltip", "onclick", "data_id"),
                                                               name = "geom_segment_interactive")
                                        if (nrow(data) < 1 || ncol(data) < 1 ) return(zeroGrob())
                                        
                                        if (coord$is_linear()) {
                                              coord <- coord$transform(data, panel_scales)
                                              if( !is.null(coord$tooltip) && !is.character(coord$tooltip) )
                                                    coord$tooltip <- as.character(coord$tooltip)
                                              if( !is.null(coord$onclick) && !is.character(coord$onclick) )
                                                    coord$onclick <- as.character(coord$onclick)
                                              if( !is.null(coord$data_id) && !is.character(coord$data_id) )
                                                    coord$data_id <- as.character(coord$data_id)
                                              
                                              
                                              return(interactive_segments_grob(coord$x, coord$y, coord$xend, coord$yend,
                                                                               tooltip = coord$tooltip,
                                                                               onclick = coord$onclick,
                                                                               data_id = coord$data_id,
                                                                               default.units = "native",
                                                                               gp = gpar(
                                                                                     col = alpha(coord$colour, coord$alpha),
                                                                                     fill = alpha(coord$colour, coord$alpha),
                                                                                     lwd = coord$size * .pt,
                                                                                     lty = coord$linetype,
                                                                                     lineend = lineend
                                                                               ),
                                                                               arrow = arrow
                                              ))
                                        }
                                        
                                        data$group <- 1:nrow(data)
                                        starts <- subset(data, select = c(-xend, -yend))
                                        
                                        ends <- subset(data, select = c(-x, -y))
                                        names(ends)[names(ends) %in% "xend"] <- "x"
                                        names(ends)[names(ends) %in% "yend"] <- "y"
                                        
                                        pieces <- rbind(starts, ends)
                                        pieces <- pieces[order(pieces$group),]
                                        
                                        GeomPathInteractive$draw_panel(pieces, panel_scales, coord, arrow = arrow,
                                                                       lineend = lineend)
                                  },
                                  
                                  required_aes = c("x", "y", "xend", "yend"),
                                  default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA,
                                                    tooltip = NULL, onclick = NULL, data_id = NULL),
                                  
                                  draw_key = draw_key_path
)



# ==== geom_vline_interactive ====


geom_vline_interactive <- function(mapping = NULL, data = NULL,
                       ...,
                       xintercept,
                       na.rm = FALSE,
                       show.legend = NA) {
      
      # Act like an annotation
      if (!missing(xintercept)) {
            data <- data.frame(xintercept = xintercept)
            mapping <- aes(xintercept = xintercept)
            show.legend <- FALSE
      }
      
      layer(
            data = data,
            mapping = mapping,
            stat = StatIdentity,
            geom = GeomInteractiveVline,
            position = PositionIdentity,
            show.legend = show.legend,
            inherit.aes = FALSE,
            params = list(
                  na.rm = na.rm,
                  ...
            )
      )
}


GeomInteractiveVline <- ggproto("GeomVline", Geom,
                     draw_panel = function(data, panel_scales, coord) {
                           ranges <- coord$range(panel_scales)
                           
                           data$x    <- data$xintercept
                           data$xend <- data$xintercept
                           data$y    <- ranges$y[1]
                           data$yend <- ranges$y[2]
                           
                           GeomInteractiveSegment$draw_panel(unique(data), panel_scales, coord)
                     },
                     
                     default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),
                     required_aes = "xintercept",
                     
                     draw_key = draw_key_vline
)



# ==== Graph ====


hof_bat <- read_rds("data/13 - HOF Batting.rds")

a1 <- ggplot(data = hof_bat, aes(x = HR)) +
      geom_density(fill = "#000000", alpha = 0.7) +
      geom_vline_interactive(aes(xintercept = mean(HR), tooltip = round(mean(HR), 1)), color = "orange") +
      scale_y_continuous()

ggiraph(ggobj = a1)

