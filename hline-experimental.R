# hline-experimental.R



library(ggplot2)
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



# ==== geom_hline_interactive ====

geom_hline_interactive <- function(mapping = NULL, data = NULL,
                       ...,
                       yintercept,
                       na.rm = FALSE,
                       show.legend = NA) {
      
      # Act like an annotation
      if (!missing(yintercept)) {
            data <- data.frame(yintercept = yintercept)
            mapping <- aes(yintercept = yintercept)
            show.legend <- FALSE
      }
      
      layer(
            data = data,
            mapping = mapping,
            stat = StatIdentity,
            geom = GeomInteractiveHline,
            position = PositionIdentity,
            show.legend = show.legend,
            inherit.aes = FALSE,
            params = list(
                  na.rm = na.rm,
                  ...
            )
      )
}


GeomInteractiveHline <- ggproto("GeomHline", Geom,
                     draw_panel = function(data, panel_scales, coord) {
                           ranges <- coord$range(panel_scales)
                           
                           data$x    <- ranges$x[1]
                           data$xend <- ranges$x[2]
                           data$y    <- data$yintercept
                           data$yend <- data$yintercept
                           
                           GeomInteractiveSegment$draw_panel(unique(data), panel_scales, coord)
                     },
                     
                     default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),
                     required_aes = "yintercept",
                     
                     draw_key = draw_key_path
)




# ==== Graph ====

war_combo_avg <- read_rds("data/18 - JAWS pg line chart table.rds")


# Bench

war_line <- war_combo_avg %>% 
      filter(Name == "Johnny Bench")
line_filtered <- war_line %>% 
      filter(type == "WAR4")

p <- ggplot(data = war_line) + 
      geom_point_interactive(aes(x = yearId, y = WAR, group = type, tooltip = WAR), color = alpha("#000000", 0.5)) +
      geom_point_interactive(data = line_filtered, aes(x = yearId, y = WAR, color = type, tooltip = WAR), size = 2.5, shape = 17) +
      geom_line(aes(x = yearId, y = WAR)) +
      # all the Median WAR is the same, taking mean is just me hacking to get a value instead of a vector for the y-intercept
      geom_hline_interactive(aes(yintercept = mean(`Median WAR`), linetype = "Typical HOFer", tooltip = `Median WAR`), color = alpha("#C6011F", 0.5), size = 1.25) +
      scale_linetype_manual(values = 2, guide = guide_legend(override.aes = list(color = "#C6011F"))) +
      scale_y_continuous(limits = c(min(war_line$WAR)-5, max(war_line$WAR)+5)) +
      labs(title = "WAR") +
      theme_minimal() +
      theme(axis.title = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            legend.title = element_blank(),
            legend.justification = c(0,1),
            legend.position = c(.1, 1),
            legend.box = "horizontal",
            legend.background = element_blank(),
            legend.direction = "horizontal",
            plot.title = element_text(size = 20, margin = margin(b = 10))
      )

ggiraph(ggobj = p)
