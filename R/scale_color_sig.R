#' Significance colour scales for ggplot2
#'
#' Creates ggplot2 colour scales based on the predefined Significance
#' house style palettes. Both discrete and continuous scales are supported.
#'
#' Available palettes are identical to those supported by [palette_sig()].
#'
#' @param palette Name of the palette to use. See [palette_sig()] for the
#'   available options.
#' @param discrete Logical; if `TRUE` (default), creates a discrete colour
#'   scale. If `FALSE`, creates a continuous gradient scale.
#' @param reverse Logical; if `TRUE`, reverse the colour order before
#'   constructing the scale.
#' @param random Logical; if `TRUE`, randomly shuffle the colours before
#'   constructing the scale.
#' @param n Number of colours to extract from the base palette before
#'   constructing the scale. Defaults to `0`, which uses all colours in the
#'   selected palette.
#' @param ... Additional arguments passed to the underlying ggplot2 scale
#'   function.
#'
#' @return
#' A ggplot2 scale object suitable for addition to a plot using `+`.
#'
#' @examples
#' library(ggplot2)
#'
#' # Discrete colour scale
#' ggplot(mtcars,
#'        aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point(size = 3) +
#'   scale_color_sig("main") +
#'   labs(colour = "Cylinders")
#'
#' # Continuous colour scale
#' ggplot(mtcars,
#'        aes(wt, mpg, colour = hp)) +
#'   geom_point(size = 3) +
#'   scale_color_sig("mintoplus", discrete = FALSE) +
#'   labs(colour = "Horsepower")
#'
#' @keywords colour palette
#'
#' @export
scale_color_sig <- function(palette = "all", discrete = TRUE, reverse = FALSE, random = FALSE, n = 0, ...) {
  pal <- colorramp_sig(palette = palette, reverse = reverse, random = random, n = n, space = "Lab")

  if (discrete) {
    ggplot2::discrete_scale("color", paste0(palette, "_sig"), palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}
