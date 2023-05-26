#' Significance color codes
#'
#' Definition of colors used in Significance style color palettes
#'
#' @keywords color palette
#'
#' @param ... String or list of strings with color name(s). Possible values are "light blue", "blue", "yellow", "orange", "red", "green", "light blue - darker", "blue - darker", "yellow - darker", "orange - darker", "red - darker", "green - darker", "light blue - lighter", "blue - lighter", "yellow - lighter", "orange - lighter", "red - lighter", "green - lighter"
#'
#' @return String or list of strings with color hex codes
#'
#' @examples
#' colors_sig("light blue")
#' colors_sig("blue", "light blue")
#' mtcars %>%
#'  ggplot2::ggplot(aes(gear)) +
#'    ggplot2::geom_bar(fill = colors_sig("blue")) +
#'    ggplot2::labs(x="Number of gears",
#'         y="Count",
#'         title="Car types"
#'    ) +
#'    theme_sig() +
#'    scale_y_continuous(expand = c(0, 0))
#'
#' @export
colors_sig <- function(...) {

  #Structure of functions adapted from: https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2

  cols_sig <- c(
    'light blue'           = "#009ee0", #  0-158-224
    'blue'                 = "#00567c", #  0- 86-124
    'yellow'               = "#e5c019", #229-192- 25
    'orange'               = "#ca7d16", #202-125- 22
    'red'                  = "#a81d39", #168- 29- 57
    'green'                = "#0c6846", # 12-107- 70
    'light blue - darker'  = "#005f86", #  0- 95-134
    'blue - darker'        = "#00344a", #  0- 52- 74
    'yellow - darker'      = "#89730f", #137-115- 15
    'orange - darker'      = "#794b0d", #121- 75- 13
    'red - darker'         = "#651122", #101- 17- 34
    'green - darker'       = "#07402a", #  7- 64- 42
    'light blue - lighter' = "#1abcff", # 26-188-255
    'blue - lighter'       = "#008cc9", #  0-140-201
    'yellow - lighter'     = "#ebcd46", #235-205- 70
    'orange - lighter'     = "#e99a31", #233-154- 49
    'red - lighter'        = "#da294d", #218- 41- 77
    'green - lighter'      = "#14b174", # 20-177-116
    'white'                = "#ffffff"  #255-255-255
  )

  cols <- c(...)

  if (is.null(cols))
    return (cols_sig)

  cols_sig[cols]
}
