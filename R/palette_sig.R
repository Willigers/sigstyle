#' Significance color palettes
#'
#' Defines color palettes, e.g. for use in ggplot2 plots.
#'
#' @keywords color palette
#'
#' @param palette String with name of a color palette. Possible values are "all" (complete 18 colors palette), "main" (6 colors: light blue, blue, yellow, orange, red and green), "lighter" (same 6 colors, but lighter), "darker" (same 6 colors, but darker), "basic" (only the 2 Significance logo colors: light blue and blue), "posneg" (green and red) and "posnegneut" (green, red and light blue)
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
    'all'        = colors_sig("light blue", "blue", "yellow", "orange", "red", "green",
                              "light blue - lighter", "blue - lighter", "yellow - lighter", "orange - lighter", "red - lighter", "green - lighter",
                              "light blue - darker", "blue - darker", "yellow - darker", "orange - darker", "red - darker", "green - darker"),
    'main'       = colors_sig("light blue", "blue", "yellow", "orange", "red", "green"),
    'lighter'    = colors_sig("light blue - lighter", "blue - lighter", "yellow - lighter", "orange - lighter", "red - lighter", "green - lighter"),
    'darker'     = colors_sig("light blue - darker", "blue - darker", "yellow - darker", "orange - darker", "red - darker", "green - darker"),
    'basic'      = colors_sig("light blue", "blue"),
    'posneg'     = colors_sig("green", "red"),
    'posnegneut' = colors_sig("green", "red", "light blue"),
    'one'        = colors_sig("light blue"),
    'two'        = colors_sig("light blue", "blue"),
    'three'      = colors_sig("light blue", "blue", "yellow"),
    'four'       = colors_sig("light blue", "blue", "yellow", "orange"),
    'five'       = colors_sig("light blue", "blue", "yellow", "orange", "red")
  )

  pal <- pal_sig[[palette]]

  if(n>0 & n<length(pal)) {
    pal <- pal[1:n]
  }

  return (pal)
}
