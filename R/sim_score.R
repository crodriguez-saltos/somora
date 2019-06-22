#' @export
sim_score <- function(simmat){
  scores <- data.frame(
    max_s2= apply(simmat, 2, function(x) max(x, na.rm = T)),
    match_s1= apply(simmat, 2, which.max)
  )

  scores$match_s1 <- scores$match_s1 / nrow(simmat)

  # The following code needs to be modified:
  # scores <- sapply(mi.normalized, function(x){
  #   ind <- apply(x, 2, function(y) !all(is.na(y)))
  #   sumscores <- sum(apply(x[,ind], 2, function(y) max(y, na.rm= T)))
  #   n <- length(which(ind))
  #   sumscores/n
  # })
  return(scores)
}


