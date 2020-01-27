#' Evaluate how well a reference song is imitated
#'
#' @inheritParams scoHeatmap
#' @param plot Should interpolated scores be plotted?
#' @param na.rm Should missing columns be removed from reference song?
#'
#' @details
#' Missing columns in the reference song are normally caused by silence.
#' @export

referenceImitation <- function(pos, score, plot= F,
                               na.rm= F, valid= NULL, smooth= F){
  # Summary statistic per reference position----
  d <- data.frame(position_in_reference= pos, score= score)
  scores <- plyr::ddply(
    .data = d,
    .variables = plyr::.(position_in_reference),
    .fun = function(x){
      max(x[["score"]], na.rm= T)
    })
  colnames(scores)[colnames(scores) == "V1"] <- "score"

  # Account for missing values in spectral windows of song1----
  if (na.rm){
    scores <- data.frame(
      position_in_reference= (1:length(valid)) / length(valid),
      score= scores$score[match(valid, scores$position_in_reference)]
    )
    scores$score[is.na(scores$score)] <- 0
  }
  scores <- scores[!is.na(scores$score),]

  # Smooth and interpolate point-wise statistics----
  if (smooth == T){
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
  }else{
    score <- sum(scores$score) * 100 / nrow(scores)
    if (plot){
      plot(scores$position_in_reference, scores$score, type= "l", ylim= c(0,1))
    }
  }

  return(score)
}

