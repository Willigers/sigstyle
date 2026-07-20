#' Significance colour scales for ggplot2
#'
#' Creates ggplot2 colour scales based on the predefined Significance
#' house style palettes. Both discrete and continuous scales are supported.
#'
#' For discrete scales, the number of colours is determined automatically
#' from the number of levels present in the data.
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
#' @param n Deprecated.
#'   For discrete scales the required number of colours is now determined
#'   automatically by ggplot2 based on the number of levels in the mapped
#'   variable. This argument is ignored when `discrete = TRUE`.
#'
#'   For continuous scales, use a suitable palette instead of manually
#'   specifying the number of colours.
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
#' @seealso
#' [colors_sig()], [palette_sig()], [scale_fill_sig()]
#'
#' @keywords colour palette
#'
#' @export
scale_color_sig <- function(
  palette = "all",
  discrete = TRUE,
  reverse = FALSE,
  random = FALSE,
  n = 0,
  ...
) {

  if (discrete) {

    ggplot2::discrete_scale(
      aesthetics = "colour",
      scale_name = paste0(palette, "_sig"),
      palette = palette_sig_function(
        palette = palette,
        reverse = reverse,
        random = random
      ),
      ...
    )

  } else {

    pal <- colorramp_sig_function(
      palette = palette,
      reverse = reverse,
      random = random,
      n = n,
      space = "Lab"
    )

    ggplot2::scale_color_gradientn(
      colours = pal(256),
      ...
    )

  }
}
