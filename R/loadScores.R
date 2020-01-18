#'Translate imitation scores from organizer.sh
#'
#'@param file File containing the scores.
#'
#'@details
#'The translation helps R handle the data.
#'
#'@export

loadScores <- function(file, output){
  # Import text files
  sco.all <- read.table(file)
  sco.all$bird <- sapply(sco.all$file, FUN = function(x){
    strsplit(as.character(x), split = "_")[[1]][1]
  })
  sco.all$bird <- as.factor(sco.all$bird)

  # Save RDS
  saveRDS(object = sco.all, file = output)
}
