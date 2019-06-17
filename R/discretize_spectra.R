#' @export

discretize_spectra <- function(
  sound,
  wl,
  ovlp= 90,
  bins= 2^16,
  plot= F,
  threshold,
  ...
){
  library(somora)
  # Get spectrogram
  #spectro <- enhanceSignal(sound, wl= wl, ovlp= ovlp, PSD= "none",
   #                        fmin= 0.8, fmax= 8)

  spectro <- seewave::spectro(
    wave = sound,
    dB = NULL, wl= wl, ovlp= ovlp,
    plot= F, ...)
  #spectro$amp <- round(spectro$amp, 2)

  # Block silences
  signal <- detectEvents(
    wave= sound,
    msmooth= c(wl, ovlp),
    threshold= threshold
  )
  whichstart <- cut(signal$start, breaks = spectro$time)
  whichend <- cut(signal$end, breaks = spectro$time)
  signal <- rbind(as.numeric(whichstart), as.numeric(whichend))
  signal <- apply(signal, 2, function(x) seq(x[1], x[2]))
  signal <- do.call("c", signal)

  # Discretize signal
  spectro.discrete <- sapply(1:length(spectro$time), function(i){
      ifelse(
        test= is.element(i, signal),
        yes = infotheo::discretize(X = spectro$amp[,i],
                                   disc = "equalwidth",
                                   nbins= bins),
        no = rep(NA, length(spectro$freq))
      )
  })
  spectro.discrete <- as.matrix(do.call("cbind", spectro.discrete))

  if (plot){
    image(t(spectro.discrete))
  }
  return(spectro.discrete)

}
