#' Evaluate how well a reference song is imitated
#'
#' @inheritParams scoHeatmap
#' @param plot Should interpolated scores be plotted?
#' @param na.rm Should missing columns be removed from reference song?
#'
#' @details
#' Missing columns in the reference song are normally caused by silence.
#' @export

referenceImitation <- function(pos, score, plot= F, na.rm= F){
  if (na.rm){
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

  # Summary statistic per reference position----
  d <- data.frame(position_in_reference= pos, score= score)
  scores <- ddply(
    .data = d,
    .variables = .(position_in_reference),
    .fun = function(x){
      max(x[["score"]], na.rm= T)
    })
  colnames(scores)[colnames(scores) == "V1"] <- "score"

  # Smooth and interpolate point-wise statistics----
  interp.range <- seq(0, 1, 0.001)
  interp_scores <- predict(
    loess(
      formula = score ~ position_in_reference, data = scores, span= 0.1
    ), interp.range)
  interp_scores[is.na(interp_scores)] <- 0
  interp_scores[interp_scores < 0] <- 0

  if (plot){
    plot(interp.range, interp_scores, type= "l", ylim= c(0,1))
  }

  score <- sum(interp_scores) * 100 / length(interp_scores)
  return(score)
}

