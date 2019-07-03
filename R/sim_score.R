#' @export
sim_score <- function(simmat){
  suppressWarnings(scores <- data.frame(
    song2_rel_pos= 1:ncol(simmat) / ncol(simmat),
    song1_match= as.numeric(apply(simmat, 2, which.max)) / nrow(simmat),
    score= apply(simmat, 2, function(x) max(x, na.rm= T))
  ))
  return(scores)

  # For future use
  # Get the number of spectral windows in each song----
  # The nmi files are necessary in this step
  mif <- sub(pattern = ".sco", replacement = ".nmi", x = scof)
  mif <- orderbyId(mif)

  mid <- as.list(mif)

  s1n <- sapply(mid, function(x){
    d <- read.table(x)
    n <- nrow(d[!apply(X = d, 1, function(y) all(is.na(y))),])
    return(n)
  })
}


