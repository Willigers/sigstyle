.palette_sig_function <- function(
    palette = "all",
    reverse = FALSE,
    random = FALSE
) {

  function(n) {

    cols <- palette_sig(palette, n)

    if (reverse) cols <- rev(cols)

    if (random) cols <- sample(cols)

    cols[seq_len(n)]
    
  }
}
