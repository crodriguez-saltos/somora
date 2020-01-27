#' Get the NA spectral columns in tutor spectrograms

#' This script was used to get the spectral columns in tutor spectrograms in
#' which all values are NA. These columns normally correspond to silences in the
#' song.

#' The resulting data frame is necessary to generate similarity heatmaps.
#' @export

tutorNAs <- function(simdir, output){
  # Get bird folders list----
  bird.dir <- dir(simdir, full.names = T)

  # Get files and pairs of files----
  nmi.examp <- data.frame(bird= basename(bird.dir))
  nmi.examp$s2 <- sapply(bird.dir, function(x){
    s <- dir(x, pattern = ".nmi", full.names = T, recursive = T)[1]
    s2 <- strsplit(x = s, split = "_tutor")[[1]][1]
    s2 <- strsplit(x= s2, split= "_vs_")[[1]][2]
    return(s2)
  })
  nmi.examp <- plyr::ddply(nmi.examp, .variables = plyr::.(s2), .fun = function(x){
    x <- rbind(x,x)
    q <- dir(path = file.path(simdir, x$bird[1]),
             pattern = x$s2, full.names = T, recursive = T)
    q <- q[grep(pattern = ".nmi", x = q)]
    x$nmi <- q
    return(x)
  })
  nmi.examp$tutor <- sapply(basename(nmi.examp$nmi), function(x){
    getValues(name = x, flags = "tutor", ext = ".nmi")
  })

  # Get list of columns that are NAs in song1----
  nmi.examp <- plyr::ddply(.data = nmi.examp, .variables = plyr::.(nmi), .fun = function(x){
    t <- read.table(x$nmi[1])
    d <- data.frame(
      pos= 1:nrow(t),
      na= apply(X = t, MARGIN = 1, FUN = function(y) all(is.na(y)))
    )
    data.frame(d, x)
  })

  # Save data frame
  write.table(
    x = nmi.examp,
    file = output
  )
}
