#' @export

discretize_spectra <- function(wl, sound, ovlp= 90, bins= 2^16,
                               plot= F){
  library(somora)
  spectro <- enhanceSignal(sound, wl= wl, ovlp= ovlp, PSD= "none",
                           fmin= 0.8, fmax= 8)

  # discretize signal
  spectro.discrete <- apply(spectro, 2, function(x){
    infotheo::discretize(X = x, disc = "equalwidth", nbins= bins)
  })
  spectro.discrete <- as.matrix(do.call("cbind", spectro.discrete))

  if (plot){
    image(t(spectro.discrete))
  }
  return(spectro.discrete)

}
