#'Plot histogram of durations of recordings analyzed with organizer.sh
#'
#'@param file File containing the durations returned by organizer.sh
#'@param pattern.filter Pattern used to filter recordings.
#'
#'@export

checkAudioDuration <- function(file, pattern.filter){
  #  TODO
  #  It would be good to make sure that the files in audioduration are filtered
  #  according to the actual files that were used to get the NMI scores, because
  #  there is a small discrepancy in the number of files between sco.all and
  #  audiodurations. To get the actual files, it is recommendable to get the
  #  database of all NMI values, previous to elimination of zeros.

  library(plyr)
  durations <- read.table(file= file, stringsAsFactors = F)
  colnames(durations) <- c("file", "seconds")

  # The following script needs a standard directory structure
  # for storing sound files
  durations$bird <- basename(dirname(dirname(durations$file)))
  durations$bird <- as.factor(durations$bird)

  # Some birds that are not operant birds were wrongly included in the list, I
  # eliminated those
  durations <- durations[!grepl(pattern.filter, durations$bird),]

  # Get duration per bird
  duration_perbird <- ddply(
    .data = durations,
    .variables = .(bird),
    .fun = function(x){
      sum(x$seconds)
    })
  colnames(duration_perbird)[colnames(duration_perbird) == "V1"] <- "duration"

  plot(
    density(duration_perbird$duration),
    main= "Distribution of duration of recordings"
  )

  return(duration_perbird)
}
