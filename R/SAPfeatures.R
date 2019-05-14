#' Extract spectral features
#'
#' The features that are extracted are similar or equivalent to those
#' used in Sound Analysis Pro 2011 for the calculation of similarity indexes.
#' When possible, functions from `seewave` were using.
#'
#'@param sound Wave object.
#'@param fmin Minimum frequency at which to analyze sound.
#'@param fmax Maximum frequency at which to analyze sound.
#'@param wl Window size for the analysis.
#'@param ovlp Overlap between two sucessive windows (in %).
#' @export

SAPfeatures <- function(wave, fmin = 0, fmax = NULL, wl, ovlp){
  if (is.null(fmax)){
    fmax <- wave@samp.rate / 2
  }
  sound <- seewave::ffilter(
    wave = sound1, from = fmin, to = fmax, output= "Wave"
  )

  spectro <- seewave::spectro(
    wave = sound1, wl = wl, ovlp = ovlp, plot= F, dB= NULL
  )$amp

  spec <- round(rowSums(spectro),2)
  lowest <- which(spec > 0)[1]
  top <- length(spec) - which(rev(spec) > 0)[1]
  spectro <- spectro[lowest:top,]
  wentr <- apply(spectro, 2, seewave::sfm)

  fundres <- gpitch(sound, wl= wl, ovlp= ovlp, fmax= fmax)
  pitch <- fundres$pitch
  pgood <- fundres$gpitch

  gpitch <- gpitch(sound, wl= wl, ovlp= ovlp, fmax= fmax)

  df <- seewave::dfreq(sound, wl= wl, ovlp = ovlp, plot= F,
                bandpass= c(fmin, fmax))
  ctime <- mean(diff(df[,1]))
  difffreq <- c(abs(df_1[2:nrow(df_1),2] - df_1[1:(nrow(df_1) - 1),2]), 0)

  am <- c(abs(diff(colSums(spectro))),0)

  features <- data.frame(
    pitch= pitch[,2],
    fm= difffreq,
    entropy= wentr,
    pgood= pgood,
    am= am
  )

  return(features)
}
