#' Fill color scale ramps from Significance style color palettes
#'
#' Defines color scales for ggplot2 plots, based on color ramps from color values. For determining fill colors.
#'
#' @param palette String with name of a color palette. Possible values are "all" (complete 18 colors palette), "main" (6 colors: light blue, blue, yellow, orange, red and green), "lighter" (same 6 colors, but lighter), "darker" (same 6 colors, but darker), "basic" (only the 2 Significance logo colors: light blue and blue), "posneg" (green and red) and "posnegneut" (green, red and light blue)
#' @param discrete Indicates a discrete (TRUE) or continuous (FALSE) scale
#' @param reverse Reverses color ordering if TRUE
#' @param random Shuffles color ordering if TRUE
#'
#' @keywords color palette
#'
#' @examples
#' mtcars %>%
#' mutate(gear = as_factor(gear)) %>%
#'   group_by(gear) %>%
#'   summarise(count = n()) %>%
#'   ggplot(aes(x="", y=count, fill=gear)) +
#'   geom_bar(stat="identity", width=1, color="white") +
#'   labs(title="Number of gears") +
#'   coord_polar("y", start=0, direction=-1) +
#'   theme_sig() +
#'   scale_fill_sig("basic", discrete=TRUE, reverse=FALSE) +
#'   scale_y_discrete(expand = c(0, 0))
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
