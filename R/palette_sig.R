#' Significance color palettes
#'
#' Defines color palettes, e.g. for use in ggplot2 plots.
#' All color palettes are based on the colors from the Significance house style Excel template.
#'
#' Currently available palettes are:
#' basic : the 2 colors from the Significance logo. Suitable for category or continuous data.
#' main : the standard set of 6 colors in the significance theme. Particularly suitable for category data.
#' lighter : the lighter 6 colors in significance theme. Particularly suitable for category data.
#' darker : the darker 6 colors in significance theme. Particularly suitable for category data.
#' all : all 18 colors in significance theme (main, lighter and darker combined). Particularly suitable for category data.
#' light : the lighter 12 colors in significance theme (main and lighter combined). Particularly suitable for category data.
#' posneg : green (positive) and red (negative). Particularly suitable for (binary) category data.
#' posnegneut : green (positive), red (negative) and light blue (neutral). Particularly suitable for category data.
#' postoneg : green (positive), white (neutral) and red (negative). Particularly suitable for continuous data.
#' mintoplus : blue (low), white (neutral) and red (high). Particularly suitable for continuous data.
#' blues : white (low) and blue (high). Particularly suitable for continuous data.
#' lightblues : white (low) and light blue (high). Particularly suitable for continuous data.
#' intense : yellow (low), orange (mid) and red (high). Particularly suitable for continuous data.
#' highlight : light blue (regular) and red (emphasis). Particularly suitable for highlighting a selection of data points.
#' intense : yellow (low), orange (mid) and red (high). Particularly suitable for continuous data.
#'
#' For the color palettes "all", "main", "light", "lighter" and "darker" the colors are recycled if the required number of colors is larger than the number of colors in the palette. For the other palettes the colors will be interpolated.
#'
#' @keywords color palette
#'
#' @param palette String with name of a color palette. Possible values are listed below.
#' @param n Number of colors to extract
#'
#' @return List with color names and hex values
#'
#' @examples
#' palette_sig("main")
#'
#' @export
palette_sig <- function(palette = "all", n = 0) {

  #Structure of functions adapted from: https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2

  # Define palettes from Significance style colors
  pal_sig <- list(
    'all'         = colors_sig("light blue", "blue", "yellow", "orange", "red", "green",
                              "light blue - lighter", "blue - lighter", "yellow - lighter", "orange - lighter", "red - lighter", "green - lighter",
                              "light blue - darker", "blue - darker", "yellow - darker", "orange - darker", "red - darker", "green - darker"),
    'main'        = colors_sig("light blue", "blue", "yellow", "orange", "red", "green"),
    'lighter'     = colors_sig("light blue - lighter", "blue - lighter", "yellow - lighter", "orange - lighter", "red - lighter", "green - lighter"),
    'light'       = colors_sig("light blue", "blue", "yellow", "orange", "red", "green",
                               "light blue - lighter", "blue - lighter", "yellow - lighter", "orange - lighter", "red - lighter", "green - lighter"),
    'darker'      = colors_sig("light blue - darker", "blue - darker", "yellow - darker", "orange - darker", "red - darker", "green - darker"),
    'basic'       = colors_sig("light blue", "blue"),
    'posneg'      = colors_sig("green", "red"),
    'posnegneut'  = colors_sig("green", "red", "light blue"),
    'postoneg'    = colors_sig("green", "white", "light blue"),
    'mintoplus'   = colors_sig("blue", "white", "red"),
    'blues'       = colors_sig("white", "light blue - lighter", "blue"),
    'lightblues'  = colors_sig("white", "light blue"),
    'highlight'   = colors_sig("light blue", "red"),
    'intense'     = colors_sig("yellow", "orange", "red")
  )

  pal <- pal_sig[[palette]]

  if(palette %in% c("all", "main", "lighter", "darker", "light") & n>length(pal)) {
    pal <- rep(pal, ceiling(n/length(pal)))
  }
  if(n>0 & n<length(pal)) {
    pal <- pal[1:n]
  }

  return (pal)
}
