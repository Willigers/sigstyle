#' Significance ggplot2 theme
#'
#' Defines the theme for ggplot2 plots. This includes fonts, grid lines, axis ticks, margins, etc.
#'
#' @keywords theme
#'
#' @return Theme to be used for ggplot2 plots
#'
#' @examples
#' mtcars %>%
#'   ggplot(aes(gear)) +
#'     geom_bar() +
#'     labs(x="Number of gears",
#'          y="Count",
#'          title="Car types"
#'     ) +
#'     theme_sig() +
#'     scale_y_continuous(expand = c(0, 0))
#'
#' @export
theme_sig <- function(){

  #Structure adapted from: https://rpubs.com/mclaire19/ggplot2-custom-themes

  #Make sure R can find the Georgia font on Windows
  windowsFonts(Georgia=windowsFont("Georgia"))

  font <- "Georgia"

  theme_light() %+replace%    #replace elements we want to change

    ggplot2::theme(

      #grid elements: only horizontal grid lines remain
      panel.grid.major.x = element_blank(),  #strip vertical gridlines
      panel.grid.major.y = element_line(size = 0.1, colour = "grey75"),  #format horizontal gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      panel.border = element_blank(),        #strip border around plot
	  panel.spacing = unit(0.75, "cm"),      #spacing between facets
      axis.line = element_blank(),           #strip axis lines
      axis.ticks = element_blank(),          #strip axis ticks
      plot.margin = unit(c(10,10,10,10), "pt") ,

      #text elements
      plot.title = element_text(             #title
        family = font,            #set font family
        size = 14,                #set font size
        hjust = 0.5,              #center align
        vjust = 1,
        margin = margin(t = 0, b = 10, unit = "pt"),               #raise slightly
		color = "black"),         #title is black
      plot.subtitle = element_text(          #subtitle
        family = font,            #font family
        size = 13,                #font size
		color = "#2c2c2c"),       #subtitle is dark gray

      plot.caption = element_text(           #caption
        family = font,            #font family
        size = 9,                 #font size
        hjust = 1),               #right align

      axis.title = element_text(             #axis titles
        family = font,            #font family
        vjust = 0,                #center align
        size = 10,                #font size
        margin=margin(t = 5, b = 20, unit = "pt")),

      axis.text = element_text(              #axis text
        family = font,            #axis family
        vjust = 0.25,             #center align
        size = 9),                #font size

      axis.text.x = element_text(            #margin for axis text
        margin=margin(t = 3, b = 5, unit = "pt")),

      axis.text.y = element_text(            #margin for axis text
        margin=margin(l = 3, r = 5, unit = "pt")),

      # Strip labels for graphs with facets
      strip.text = element_text(
        family= font,
        size = 10,
        color = colors_sig("blue"),
		margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")),  #margin to avoid overlap with plot
      strip.background = element_blank(),

      # Legend
      legend.title = element_text(
        family= font,
        size = 10,
        color = "#2c2c2c"),
      legend.text = element_text(
        family= font,
        size = 9,
        color = "#2c2c2c")
    )

}
