#' Color scale ramps from Significance style color palettes
#'
#' Defines color scales for ggplot2 plots, based on color ramps from color values. For determining point and line colors.
#'
#' @param palette String with name of a color palette. Possible values are "all" (complete 18 colors palette), "main" (6 colors: light blue, blue, yellow, orange, red and green), "lighter" (same 6 colors, but lighter), "darker" (same 6 colors, but darker), "basic" (only the 2 Significance logo colors: light blue and blue), "posneg" (green and red) and "posnegneut" (green, red and light blue)
#' @param discrete Indicates a discrete (TRUE) or continuous (FALSE) scale
#' @param reverse Reverses color ordering if TRUE
#'
#' @keywords color palette
#'
#' @examples
#' ggplot2::ggplot() +
#'   scale_color_sig()
#'
#' @export
scale_color_sig <- function(palette = "all", discrete = TRUE, reverse = FALSE, n = 0, ...) {
  pal <- colorramp_sig(palette = palette, reverse = reverse, n = n, space = "Lab")
  
  if (discrete) {
    ggplot2::discrete_scale("color", paste0(palette, "_sig"), palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}