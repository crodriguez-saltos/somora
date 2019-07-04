#' @export
sim_score <- function(simmat){
  suppressWarnings(scores <- data.frame(
    song2_rel_pos= 1:ncol(simmat) / ncol(simmat),
    song1_match= as.numeric(apply(simmat, 2, which.max)) / nrow(simmat),
    score= apply(simmat, 2, function(x) max(x, na.rm= T))
  ))
  return(scores)
}


