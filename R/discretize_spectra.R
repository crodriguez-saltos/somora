#' @export

discretize_spectra <- function(
  sound,
  wl,
  ovlp= 90,
  bins= 2^16,
  plot= F,
  threshold,
  fmin= 0.8,
  fmax= 8,
  ...
){
  library(somora)
  # Get spectrogram
  spectro <- seewave::spectro(
    wave = sound,
    dB = NULL,
    wl= wl, ovlp= ovlp,
    flim = c(fmin,fmax),
    plot= F, ...)

  # Block silences
  signal <- detectEvents(
    wave= sound,
    msmooth= c(wl, ovlp),
    threshold= threshold
  )

  whichstart <- cut(signal$start, breaks = spectro$time)
  if (signal$start[1] == 0){
    whichstart[1] <- levels(whichstart)[1]
  }
  if (round(seewave::duration(sound), 6) == round(tail(signal$end, n= 1), 6)){
    signal$end[length(signal$end)] <- tail(spectro$time, n= 1)
  }
  whichend <- cut(signal$end, breaks = spectro$time)
  signal <- rbind(as.numeric(whichstart), as.numeric(whichend))
  signal <- apply(signal, 2, function(x) seq(x[1], x[2]))
  if (class(signal) == "matrix"){
    signal <- as.data.frame(signal)
    signal <- as.list(signal)
  }
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
