my_colors <- function(palette="cb"){
  ### The palette with black:
  cb.palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  ## Same one Reversed
  rcb.palette <- rev(cb.palette)
  ## Blue and yellow first choices
  bly.palette <- c("#E69F00", "#0072B2", "#000000", "#56B4E9", "#009E73", "#F0E442", "#D55E00", "#CC79A7")
  if (palette=="cb") return(cb.palette) else if (palette=="rcb") return(rcb.palette) else if (palette=="bly") return(bly.palette) else stop("Choose cb, rcb, or bly ony.")
}
is_max <- function(x) {
  seq_along(x) == which.max(x)
}