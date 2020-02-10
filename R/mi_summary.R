#' Get comparisons for all mutual information scores

#' @details
#' This script is used to run in batch all available similarity scores per bird.
#' It produces a dataframe that shows for each spectral window of juvenile song:
#' 1) whether it is more similar to father song or neighbor song, 2) which part
#' it matches in the tutor song, and 3) how more similar is it so father song or
#' neighbor song than to the other one.

#' The resulting data frame is necessary to generate similarity heatmaps.
#' @export

mi_summary <- function(simdir, output){
  # Get bird folders list----
  birds <- dir(simdir)

  # Load libraries----
  library(somora)

  # Get files and pairs of files----
  score_files <- data.frame(
    scof = dir(simdir, pattern= ".sco", full.names = T, recursive = T),
    stringsAsFactors = F
  )
  score_files$song2 <- as.factor(sapply(score_files$scof, function(x){
    s2 <- strsplit(x = x, split = "_tutor")[[1]][1]
    s2 <- strsplit(x= s2, split= "_vs_")[[1]][2]
    return(s2)
  }))

  # Retrieve similary scores from every dataframe----
  p <- proc.time()
  print(paste("Processing", length(levels(score_files$song2)), "pairs"))
  library(plyr)
  library(somora)

  limbodata <- vector()
  sco <- dlply(
    .data = score_files,
    .variables = .(song2), .fun = function(x){
      tryCatch({
        compareScores(
          scof= x$scof,
          song1flag = "tutor",
          c1s1 = "father",
          c2s1 = "neighbor",
          plotdiff = F
        )
      }, error= function(e){
        limbodata <- c(limbodata, as.character(x$scoff))
      })
    }
  )
  sco <- do.call("rbind", sco)
  print(proc.time() - p)

  # Save data frame
  write.table(x = sco,  file = output)
  if (length(limbodata) > 0){
    writeLines(
      x = limbodata,
      file= sub(
        pattern= ".txt",
        replacement = "_limbo.txt",
        x = output
      )
    )
  }
  rm(sco)
}
