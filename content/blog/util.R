
options(scipen = 100)
options(tidymodels.dark = TRUE) 

round_numerics <- 
  function(data, digits = 2){
    data %>%
      mutate(across(where(is.numeric), ~ round(.x, digits)))
  }

get_table <- 
  function(data){
    data %>%
      round_numerics() %>%
      reactable::reactable(., fullWidth = F, resizable = T, filterable = T, 
                           highlight = T, defaultPageSize = 10, wrap = FALSE,
                           showSortIcon = T, striped = T, compact = T)
  }

bg_theme <- 
  function(base_size = 11,
           strip_text_size = 12,
           label_text_size = 8
           strip_text_margin = 10,
           subtitle_size = 13,
           subtitle_margin = 10,
           plot_title_size = 16,
           plot_title_margin = 10,
           font = "RobotoMono-Regular",
           ...) {
    
    ret <-
      ggplot2::theme_gray(base_family = font,
                          base_size = base_size, ...,) +
      theme(
        panel.background = element_rect(fill = "#e9e9ea"),
        plot.background = element_rect(fill = "#f3f3f3"),
        legend.background = element_rect(fill = "#f3f3f3"),
        panel.grid = element_line(color = "#ffffff"),
        panel.grid.major = element_line(color = "#ffffff")
      )
    
    ret$strip.text <-
      ggplot2::element_text(
        # hjust = 0,
        vjust = -.8,
        size = strip_text_size,
        margin = ggplot2::margin(b = strip_text_margin),
        family = font
      )
    
    ret$plot.subtitle <-
      ggplot2::element_text(
        hjust = 0,
        size = subtitle_size,
        margin = ggplot2::margin(b = subtitle_margin),
        family = font
      )
    
    ret$plot.title <-
      ggplot2::element_text(
        hjust = 0,
        size = plot_title_size,
        margin = ggplot2::margin(b = plot_title_margin),
        family = font
      )
    
    ret$labels <-
        size = label_text_size,
        family = font
      )
    
    ret
  }

bg_red <- "#E64B35"
bg_green <- "#00a087"
bg_blue <- "#4DBBD5"