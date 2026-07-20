#' Create colorramps for continuous scales
#'
#' @return String or list of strings with color hex codes
#'
colorramp_sig_function <- function(
  palette = "all",
  reverse = FALSE,
  random = FALSE,
  n = 0,
  ...
) {

  pal <- palette_sig(palette, n)

  if (reverse) pal <- rev(pal)

  if (random) pal <- sample(pal)

  colorRampPalette(pal, ...)

}
