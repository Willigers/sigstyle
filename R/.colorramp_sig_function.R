.colorramp_sig_function <- function(
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
