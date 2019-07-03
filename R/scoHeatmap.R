#' Generate a heatmap of matches to a reference song
#'
#' @param pos Vector of positions in the reference song that have been matched.
#' @param score Vector of similarity scores.
#' @param title Title used for the plot.
#' @param plot Should the plot be printed?
#'
#' @details Each element in `score` must correspond to an element in `pos`.
#'
#' @export

similarityHeatmap <- function(pos, score, title= NULL, plot= T){
  d <- data.frame(position_in_reference= pos, score= score)
  p <- ggplot2::ggplot(
    data= d,
    ggplot2::aes(x = position_in_reference, y= score)
  )
  p <- p + ggplot2::geom_bin2d(bins=50)
  p <- p + ggplot2::ylim(0,1) + ggplot2::xlim(0,1)
  if (!is.null(title)){
    p <- p + ggplot2::labs(title= title)
  }

  if(plot){
    print(p)
  }

  return(p)
}
