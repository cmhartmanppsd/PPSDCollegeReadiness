# Custom ggplot2 theme for graphics built off of theme_bw
theme_jb <- function(base_size = 12, base_family = "Open Sans"){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(panel.border = element_blank())
}
