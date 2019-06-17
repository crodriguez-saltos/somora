#' @export
sim_score <- function(simmat){
  scores <- data.frame(
    max_s2= apply(simmat, 2, function(x) max(x, na.rm = T)),
    match_s1= apply(simmat, 2, which.max)
  )

  scores$match_s1 <- scores$match_s1 / nrow(simmat)

  return(scores)
}
