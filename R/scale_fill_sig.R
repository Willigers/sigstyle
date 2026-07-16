#' Significance fill scales for ggplot2
#'
#' Creates ggplot2 fill scales based on the predefined Significance
#' house style palettes. Both discrete and continuous scales are supported.
#'
#' Available palettes are identical to those supported by [palette_sig()].
#'
#' @param palette Name of the palette to use. See [palette_sig()] for the
#'   available options.
#' @param discrete Logical; if `TRUE` (default), creates a discrete fill
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
#' # Discrete fill scale
#' ggplot(mtcars,
#'        aes(factor(cyl), fill = factor(cyl))) +
#'   geom_bar() +
#'   scale_fill_sig("main") +
#'   labs(
#'     x = "Number of cylinders",
#'     fill = "Cylinders"
#'   )
#'
#' # Continuous fill scale
#' ggplot(mtcars,
#'        aes(factor(cyl), mpg, fill = hp)) +
#'   geom_col() +
#'   scale_fill_sig("mintoplus", discrete = FALSE) +
#'   labs(
#'     x = "Number of cylinders",
#'     fill = "Horsepower"
#'   )
#'
#' @seealso
#' [colors_sig()], [palette_sig()], [colorramp_sig()],
#' [scale_color_sig()]
#'
#' @keywords colour palette
#'
#' @export
scale_fill_sig <- function(palette = "all", discrete = TRUE, reverse = FALSE, random = FALSE, n = 0, ...) {
  pal <- colorramp_sig(palette = palette, reverse = reverse, random = random, n = n, space = "Lab")

  if (discrete) {
    ggplot2::discrete_scale("fill", paste0(palette, "_sig"), palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}
