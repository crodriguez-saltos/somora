#' Chop audio file into several segments
#' 
#' @keywords audio,segmentation
#' @param file Wave file address.
#' @param timethrs The time constant used to segment the audio.
#' @param ... Arguments passed to seewave::timer.
#' 
#' @details Chops an audio file into several segments defined by a time 
#' constant.
#' 
#' The script uses the seewave::timer function to ensure that the segment
#' boundaries do not cut across sounds. 
#' 
#' @export

chopAudio <- function(file, timethrs, plot= F, saveWav= T, ...){
  # Detect sound events----
  sound <- tuneR::readWave(file)
  
  # Add silence at beginning and end----
  # This helps with detecting signals
  silence_dur <- 0.2
  sound <- seewave::addsilw(wave = sound, at = "start", 
                            d = silence_dur, output = "Wave")
  sound <- seewave::addsilw(wave = sound, at = "end", 
                            d = silence_dur, output = "Wave")
  
  segments <- seewave::timer(wave = sound, plot= F, ...)
  
  # Ensure that starts always precede ends----
  starts.first <- segments$s.start[1]
  starts.last <- segments$s.start[length(segments$s.start)]
  ends.first <- segments$s.end[1]
  ends.last <- segments$s.end[length(segments$s.end)]
  
  if (starts.first > ends.first){
    segments$s.start <- c(0, segments$s.start)
  }
  
  if (starts.last > ends.last){
    segments$s.end <- c(segments$s.end, duration(sound))
  }

  # Extract timestamps----
  timestamps <- data.frame(start= segments$s.start, end= segments$s.end)
  
  # Group events contained within the same segment----
  breaks <- seq(
    0, 
    ceiling(max(timestamps$end) / timethrs) * timethrs, 
    timethrs
  )
  timestamps$segment <- cut(x = timestamps$end, breaks= breaks)
  
  segments <- plyr::ddply(
    .data = timestamps, 
    .variables = .(segment), 
    .fun = function(x){
      y <- x[1,]
      y$start <- min(x$start)
      y$end <- max(x$end)
      y$label <- "m"
      return(y)
    })
  
  # Plot segments----
  if (plot) {
    print(paste("Plotting audio events (red) and segments(green) in", file))
    seewave::timer(wave = sound, plot= T, ...)
    segments(x0= segments$start, y0= 1.1, x1= segments$end, y1= 1.1, 
             col= "green", lwd= 2)
    segments(x0= segments$start, y0= 0, x1= segments$start, y1= 1.1, 
             col= "green", lwd= 2)
    segments(x0= segments$end, y0= 0, x1= segments$end, y1= 1.1, 
             col= "green", lwd= 2)
  }
  
  # Write txt in the same format as that of Audacity label track exports----
  if (saveWav){
    label_file <- sub(pattern = ".wav", replacement = "_motifs.txt", x = file)
    segments$start <- segments$start - silence_dur
    segments$end <- segments$end - silence_dur
    write.table(
      x = segments[,!(colnames(segments) == "segment")], 
      file = label_file, 
      row.names = F, col.names = F, sep = "\t", quote = F
    )
    
    # Extract segments----
    source("./extractMotifs.R")
    extractMotifs(label_file)
  }
}