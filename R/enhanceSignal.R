#'@export

enhanceSignal <- function(sound, sound2= NULL, type= "single",
                          PSD= "normal", tms= NULL, ...){
  if (!is.null(sound2)){
    # Get PSDs
    spectro <- getPSD(sound= sound, type= PSD, tms= tms[[1]], ...)
    spectro2 <- getPSD(sound= sound2, type= PSD, tms= tms[[2]], ...)

    spec <- rowSums(cbind(spectro, spectro2))
    lowest <- which(spec > 0)[1]
    top <- length(spec) - which(rev(spec) > 0)[1]
    spectro <- spectro[lowest:top,]
    spectro2 <- spectro2[lowest:top,]

    return(list(spectro1= spectro, spectro2= spectro2))
  }else{
    spectro <- getPSD(sound= sound, type= PSD, tms= tms, ...)

    # Filter PSDs
    spec <- rowSums(spectro)
    lowest <- which(spec > 0)[1]
    top <- length(spec) - which(rev(spec) > 0)[1]
    spectro <- spectro[lowest:top,]
    return(spectro)
  }
}
