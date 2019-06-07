#' @export
sim_score <- function(simmat){
  simmat <- simmat[!apply(X = simmat, MARGIN = 1, FUN = function(x) {
    all(is.nan(x))}
  ),]

  maxMI <- apply(simmat, 1, max)
  sim <- mean(maxMI)
  return(list(sim= sim, simPerBin= maxMI))
}
