#' Plot the spectrogram of a representative sound in a similariy heatmap
#'
#' @param coord Coordinates in the heatmap to explore.
#' @inheritParams scoHeatmap
#' @param sound2files Vector of file addresses of song 2 for each score value
#' @param sound1file File address of song 1.
#'   used to produce the headmap.
#' @export

exploreHeatmap <- function(coord, pos, score, sound2files, sound1file, title){
  p <- scoHeatmap(pos = pos, score = score, title = title, plot= F)
  p <- p + geom_point(aes(x= pos[1], y= pos[2], color= "red"))
  print(p)
}
