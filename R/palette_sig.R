#' Significance color palettes
#'
#'
#' Returns colour palettes based on the Significance house style.
#' These palettes are derived from the colours used in the Significance
#' Excel template and can be used in plots, tables, and other visualisations.
#'
#' @details
#' Available palettes:
#'
#' **Categorical palettes**
#'
#' * `"basic"` – the two colours from the Significance logo.
#' * `"main"` – the standard set of six Significance colours.
#' * `"lighter"` – lighter variants of the six standard colours.
#' * `"darker"` – darker variants of the six standard colours.
#' * `"light"` – the standard and lighter colours combined (12 colours).
#' * `"all"` – all standard, lighter, and darker colours combined (18 colours).
#' * `"posneg"` – green (positive) and red (negative).
#' * `"posnegneut"` – green (positive), red (negative), and light blue (neutral).
#'
#' **Continuous palettes**
#'
#' * `"postoneg"` – green (positive), white (neutral), and red (negative).
#' * `"mintoplus"` – blue (low), white (neutral), and red (high).
#' * `"blues"` – white (low) to blue (high).
#' * `"lightblues"` – white (low) to light blue (high).
#' * `"intense"` – yellow (low), orange (mid), and red (high).
#'
#' **Highlighting palette**
#'
#' * `"highlight"` – light blue for regular data and red for highlighted values.
#'
#' For the palettes `"all"`, `"main"`, `"light"`, `"lighter"`, and `"darker"`,
#' colours are recycled when `n` exceeds the number of colours available in the
#' palette. For all other palettes, colours are interpolated between the palette
#' endpoints when additional colours are requested.
#'
#' @param palette Name of the palette to return.
#' @param n Number of colours to return. If `n = 0` (default), all colours in
#'   the selected palette are returned.
#'
#' @keywords color palette
#'
#' @param palette String with name of a color palette. Possible values are listed below.
#' @param n Number of colors to extract
#'
#' @return
#' A character vector containing colour hex codes.
#'
#' @examples
#' palette_sig("main")
#'
#' @keywords colour palette
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
