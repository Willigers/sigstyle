#' Create color palette for discrete scales
#'
#' @return String or list of strings with color hex codes
#'
palette_sig_function <- function(
    palette = "all",
    reverse = FALSE,
    random = FALSE
) {

  function(n) {

    cols <- palette_sig(palette)

    if (reverse) {
      cols <- rev(cols)
    }

    if (random) {
      cols <- sample(cols)
    }

    interpolated_palettes <- c(
      "postoneg",
      "mintoplus",
      "blues",
      "lightblues",
      "intense",
      "white2lightblue"
    )

    if (palette %in% interpolated_palettes) {

      cols <- grDevices::colorRampPalette(
        cols,
        space = "Lab"
      )(n)

    } else {

      if (n > length(cols)) {
        cols <- rep(cols, length.out = n)
      } else {
        cols <- cols[seq_len(n)]
      }

    }

    unname(cols)

  }

}
