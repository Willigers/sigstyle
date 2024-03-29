% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colorramp_sig.R
\name{colorramp_sig}
\alias{colorramp_sig}
\title{Color ramps from Significance style color palettes}
\usage{
colorramp_sig(palette = "all", reverse = FALSE, random = FALSE, n = 0, ...)
}
\arguments{
\item{palette}{String with name of a color palette. Possible values are "all" (complete 18 colors palette), "main" (6 colors: light blue, blue, yellow, orange, red and green), "lighter" (same 6 colors, but lighter), "darker" (same 6 colors, but darker), "basic" (only the 2 Significance logo colors: light blue and blue), "posneg" (green and red) and "posnegneut" (green, red and light blue)}

\item{reverse}{Reverses color ordering if TRUE}
}
\description{
Defines colour palettes based on color ramps from color values
}
\examples{
ggplot2::ggplot() +
  geom_point(colour = pal_sig("basic")

}
\keyword{color}
\keyword{palette}
