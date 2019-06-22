#'@export

detectEvents <- function(wave= NULL, file= NULL,
                         mindur= 0.01, ...){
  # Detect sound events----
  if (is.null(wave)){
    sound <- tuneR::readWave(file)
  }else{
    sound <- wave
  }

  # Find signal/noise events
  segments <- seewave::timer(wave = sound, plot= T, dmin= mindur, ...)

  # Extract timestamps----
  timestamps <- data.frame(start= segments$s.start,
                           end= segments$s.start + segments$s)


  return(timestamps)
}
