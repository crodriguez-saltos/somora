extractMotifs <- function(x, outdir= ".", label= "m"){
  # Import labels and load info into data frames
  labels <- read.table(x, sep= "\t", stringsAsFactors = F)

  # Format tables with labels
  colnames(labels) <- c("start", "end", "label")
  labels$label <- labels$label
  head(labels)

  # Create output directory
  dir_output <- file.path(outdir, "extracted-motifs")

  if (!dir.create(dir_output)){
    dir.create(path = dir_output, recursive = T)
  }

  # Extract motifs
  motifs <- labels[labels$label == label,]

  for (i in 1:nrow(motifs)){
    input <- x
    input <- sub(pattern = "_motifs.txt", replacement = ".wav", x = input)
    output <- sub(
      pattern = ".wav",
      replacement = paste0(
        "_extracted_start-",
        round(motifs$start[i], digits = 2),
        "s.wav"),
      x = basename(input)
    )

    duration <- motifs[i,]$end - motifs[i,]$start
    print(paste("Now saving file", output))
    system(command = paste(
      "sox", input, file.path(dir_output, output),
      "trim", motifs[i,]$start, duration
    )
    )
  }
}
