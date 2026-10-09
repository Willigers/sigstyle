#' Significance ggplot2 theme for maps
#'
#' Defines the theme for ggplot2 plots. This includes fonts, margins, etc.
#' This version has no grid lines, which is useful for maps or for other plots where grid lines are not desired. 
#'
#' @keywords theme
#'
#' @return Theme to be used for ggplot2 plots
#'
#' @examples
#' # Basic plot
#' mtcars %>%
#'   ggplot(aes(gear)) +
#'     geom_bar() +
#'     labs(x="Number of gears",
#'          y="Count",
#'          title="Car types"
#'     ) +
#'     theme_sig_map()
#' 
#' # Map
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' ggplot(nc) +
#'     geom_sf(aes(fill = BIR74), color = "black", linewidth = 0.2) +
#'     labs(
#'         title = "North Carolina Counties - Births in 1974",
#'         subtitle = "Data from 'sf' package",
#'         caption = "Source: US Census (via sf)"
#'     ) +
#'     theme_sig_map() +
#'     theme(legend.position = "right") +
#'     scale_fill_sig("white2blue", discrete = "false")
#'
#' @export
theme_sig_map <- function(){

  #Make sure R can find the Georgia font on Windows
  windowsFonts(Georgia=windowsFont("Georgia"))

  font <- "Georgia"

  theme_void()  %+replace%    #replace elements we want to change

    ggplot2::theme(

      #Text elements
      plot.title = element_text(  #Title
        family = "Georgia",       #set font family
        size = 14,                #set font size
        hjust = 0.5,              #center align
        vjust = 1,
        margin = margin(t = 0, b = 10, unit = "pt"),  #raise slightly
        color = "black"),         #title is black
      plot.subtitle = element_text(  #Subtitle
        family = "Georgia",       #font family
        size = 13,                #font size
        color = "#2c2c2c"),       #subtitle is dark gray
      plot.caption = element_text(  #Caption
        family = "Georgia",       #font family
        size = 9,                 #font size
        color = "#2c2c2c",        #caption is dark gray
        hjust = 1),               #right align
      # Legend
      legend.position = c(0.15, 0.8),
      legend.title = element_text(
        family= "Georgia",
        size = 10,
        color = "#2c2c2c"),
      legend.text = element_text(
        family= "Georgia",
        size = 9,
        color = "#2c2c2c")
    )
}
