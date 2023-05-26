#' Color ramps from Significance style color palettes
#'
#' Defines colour palettes based on color ramps from color values
#'
#' @param palette String with name of a color palette. Possible values are "all" (complete 18 colors palette), "main" (6 colors: light blue, blue, yellow, orange, red and green), "lighter" (same 6 colors, but lighter), "darker" (same 6 colors, but darker), "basic" (only the 2 Significance logo colors: light blue and blue), "posneg" (green and red) and "posnegneut" (green, red and light blue)
#' @param reverse Reverses color ordering if TRUE
#'
#' @keywords color palette
#'
#' @examples
#' ggplot2::ggplot() +
#'   geom_point(colour = pal_sig("basic")
#'
#' @export
colorramp_sig <- function(palette = "all", reverse = FALSE, random = FALSE, n = 0, ...) {

  #Structure of functions adapted from: https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2

  pal <- palette_sig(palette, n)

  if (reverse) pal <- rev(pal)
  if (random) pal <- sample(pal)

  colorRampPalette(pal, ...)
}
