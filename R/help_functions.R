
# function from the developmental R-package "metill" 
# that is used to create apporpriate theme for plots
theme_metill <- function(type = "standalone") {
  
  title <- "#484D6D"
  
  subtitle <- "#525252"
  
  caption <- "#36383A"
  
  axis_text <- "#4A4C45"
  
  axis_title <- "black"
  
  strip_background <- "#e0e0e0"
  
  background <- "#faf9f9"
  
  
  
  
  main_font <- "Lato"
  axis_title_font <- NULL
  axis_line_col <- "#403d39"
  
  strip_text <- "#2E2E2E"
  
  
  base_size <- 14
  
  out <- theme_classic() %+replace%
    theme(
      text = element_text(
        family = main_font,
        size = base_size
      ),
      plot.title = element_text(
        face = "bold",
        colour = title,
        size = base_size * 1.4,
        hjust = 0,
        margin = margin(t = 5, r = 0, b = 5, l = 0)
      ),
      plot.subtitle = element_text(
        colour = subtitle,
        size = base_size * 1,
        hjust = 0,
        margin = margin(t = 0, r = 0, b = 5, l = 5)
      ),
      plot.caption = element_text(
        colour = caption,
        hjust = 1,
        size = 0.5 * base_size,
        margin = margin(t = 7, r = 5, b = 5, l = 5)
      ),
      plot.caption.position = "panel",
      panel.background = element_rect(
        fill = background,
        colour = NA
      ),
      plot.background = element_rect(
        fill = background,
        colour = NA
      ),
      panel.grid = element_blank(),
      axis.title = element_text(
        size = base_size ,
        family = axis_title_font,
        color = "black",
        vjust = 1,
        margin = margin(t = 0, r = 0, b = 0, l = 0)
      ),
      axis.text = element_text(
        size = base_size * 0.7,
        colour = axis_text
      ),
      axis.line = element_line(
        colour = "black"
      ),
      axis.ticks = element_line(
        linewidth = 0.6,
        colour = axis_line_col
      ),
      strip.background = element_rect(
        fill = strip_background,
        colour = axis_line_col,
        linewidth = 0.8
      ),
      strip.text = element_text(
        size = 0.7 * base_size,
        margin = margin(t = 2, r = 3, b = 2, l = 3),
        colour = strip_text
      ),
      plot.margin = margin(
        t = 5,
        r = 5,
        b = 5,
        l = 5
      ),
      legend.background = element_rect(
        fill = background,
        colour = NA
      )
    )
  
  if (type == "standalone") return(out)
  else if (type == "blog") {
    out <- out %+replace%
      theme(
        plot.background = element_blank(),
        panel.background = element_blank(),
        legend.background = element_blank()
      )
  }
  
  out
}