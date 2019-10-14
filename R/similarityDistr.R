#' Plots the distribution of matches to a reference song
#'
#' @param pos Vector of positions in the reference song that have been matched.
#' @param score Vector of similarity scores.
#' @param title Title used for the plot.
#' @param plot Should the plot be printed?
#' @param ylab Title for the y-axis.
#' @param plottype Typle of geom to use for the plot
#' @param alpha Value of alpha argument in `ggplot2::geom_point()`, if using
#'   `plottype` "point".
#'
#' @details Each element in `score` must correspond to an element in `pos`.
#'
#'   If geom point is used for the plot, specify a value for the alpha argument.
#'
#' @export

similarityDistr <- function(pos, score, title= NULL, plot= T, ylab= NULL,
                              plottype= "point", alpha= NULL){
  d <- data.frame(position_in_reference= pos, score= score)

  p <- ggplot2::ggplot(
    data= d,
    ggplot2::aes(x = position_in_reference, y= score)
  )
  if (plottype == "bin2d"){
    p <- p + ggplot2::geom_bin2d(bins=50)
  }else if(plottype == "point"){
    p <- p + ggplot2::geom_point(alpha= alpha, color= "blue")
  }

  p <- p + ggplot2::ylim(0,1) + ggplot2::xlim(0,1)
  if (!is.null(title)){
    p <- p + ggplot2::labs(title= title)
  }

  if (!is.null(ylab)){
    p <- p + ggplot2::ylab(ylab)
  }

  if(plot){
    print(p)
  }

  return(p)
}
