#' Extract spectral features
#'
#' The features that are extracted are similar or equivalent to those
#' used in Sound Analysis Pro 2011 for the calculation of similarity indexes.
#' When possible, functions from `seewave` were used. The calculation of the
#' pitch is not yet similar to that in SAP.
#'
#' The output also includes dominant frequency, which is not generally used
#' in SAP.
#'
#'@param sound Wave object.
#'@param fmin Minimum frequency (in Hz) from which to analyze sound.
#'@param fmax Maximum frequency (in Hz) up to which to analyze sound.
#'@param wl Window size for the analysis.
#'@param ovlp Overlap between two sucessive windows (in %).
#' @export

SAPfeatures <- function(wave, fmin = 0, fmax = NULL, wl, ovlp, threshold= 5,
                        segment= T, plotsegments= F, export_spectro= F){
  if (is.null(fmax)){
    fmax <- wave@samp.rate / 2
  }

  # Filter sound
  wave <- seewave::ffilter(
    wave = wave, from = fmin, to = fmax, output= "Wave"
  )

  # STFT
  spectro.full <- seewave::spectro(
    wave = wave, wl = wl, ovlp = ovlp, plot= F, dB= NULL
  )
  spec <- round(rowSums(spectro.full$amp),2)
  lowest <- which(spec > 0)[1]
  top <- length(spec) - which(rev(spec) > 0)[1]
  spectro <- spectro.full$amp[lowest:top,]

  # Frequency modulation
  df <- seewave::dfreq(wave, wl= wl, ovlp = ovlp, plot= F,
                       bandpass= c(fmin, fmax), threshold = threshold)
  #ctime <- mean(diff(df[,1]))
  difffreq <- c(abs(df[2:nrow(df),2] - df[1:(nrow(df) - 1),2]), 0)

  # Wiener entropy
  wentr <- apply(spectro, 2, seewave::sfm)

  # Goodness of pitch
  fundres <- gpitch(wave, wl= wl, ovlp= ovlp, fmax= fmax,
                    plot= F, threshold= threshold)
  pitch <- fundres$pitch
  pgood <- fundres$gpitch

  # Amplitude modulation
  am <- c(abs(diff(colSums(spectro))),0)

  # Find syllables
  relative_amplitude <- colSums(spectro) * 100 / max(colSums(spectro))
  signal <- relative_amplitude > threshold

  if (plotsegments)
    seewave::timer(wave = wave, msmooth= c(wl, ovlp), threshold= threshold)

  # Compile features
  features <- data.frame(
    pitch= pitch[,2],
    fm= difffreq,
    entropy= wentr,
    pgood= pgood,
    am= am,
    df= df[,2]
  )

  if (segment){
    features[!signal,] <- NA
  }
  features$signal <- signal

  features$time= spectro.full$time

  if (export_spectro){
    return(list(spectro= spectro, features= features))
  }else{
    return(features)
  }
}
