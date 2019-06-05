#' Get PSDs across a song
#' @export

getPSD <- function(sound, dB= NULL, wl= 512, plot= F,
                   tms= NULL, type= "normal", ...){
  if (type == "timestamp"){
    minDur <- wl / sound@samp.rate
    tms <- tms[apply(tms, 1, function(x) x[2] - x[1]) >= minDur,]
    PSD <- apply(tms, 1, function(x){
      round(meanspec(wave= sound, from= x[1], to= x[2], wl = wl, plot= plot,
                     dB = dB, PSD = T, ...)[,2], 2)
    })
  }else if (type == "normal"){
    PSD <- spectro(wave = sound, dB = NULL, wl= wl, plot= F, ...)$amp
    PSD <- round(PSD ^ 2, 2)
  }else if (type == "none"){
    PSD <- spectro(wave = sound, dB = NULL, wl= wl, plot= F, ...)$amp
    PSD <- round(PSD, 2)
  }
  return(PSD)
}
