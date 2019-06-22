#'@export

silencebuffer <- function(wave, d, msmooth, plotdelay= F, ...){
  # Get the amplitude of the audio baseline
  wave.env <- env(wave, msmooth= msmooth, plot= F, ...)
  env.sorted <- sort(wave.env)
  d.nsamp <- round(wave@samp.rate * d / ((1 - msmooth[2] / 100) * msmooth[1]))
  baseline <- max(env.sorted[1:d.nsamp])

  # Add silence with that baseline amplitude
  f <- wave@samp.rate
  silence <- noisew(f= f, d = d)
  silence <- silence * baseline
  wave1 <- pastew(silence, wave, f= f, output= "Wave", at= "start")
  wave2 <- pastew(silence, wave1, f= f, output= "Wave", at= "end")

  if (plotdelay){
    aa <- env(wave, plot= F)
    bb <- env(wave1, plot= F)
    plot(aa[1:10000,1], type= "l")
    lines(bb[1:10000,1], col= "blue")
  }

  return(wave2)
}
