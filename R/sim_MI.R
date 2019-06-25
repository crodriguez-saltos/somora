#' Compare two sounds based using mutual information
#'
#' @param s1 Discretized spectrogram of sound 1.
#' @param s2 Discretized spectrogram of sound 2.
#' @param type Type of estimation of mutual information. See details.
#' @param ... Additional arguments passed to infotheo::mutinformation.
#'
#' @details
#' The types of estimation of mutual information are:
#'
#' symba -  Estimate mutual information using seewave::symba.
#' infotheo - Estimate mutual information using package infotheo.
#'
#'@export

sim_MI <- function(s1, s2, type= "infotheo", ...){
  if (type == "symba"){
    mi <- matrix(data = NA, nrow = ncol(spectro1), ncol = ncol(spectro2))
    h1 <- mi
    h2 <- mi
    nmi <- rep(NaN, ncol(spectro1) * ncol(spectro2))
    for (i in 1:ncol(spectro1)){
      for (j in 1:ncol(spectro2)){
        pixel <- symba(x = spectro1[,i], y = spectro2[,j], plot = F)
        mi[i,j] <- pixel$I
        h1[i,j] <- pixel$h1
        h2[i,j] <- pixel$h2
        nmi[i,j] <- pixel$I / pixel$h1
      }
    }
  }else if (type == "infotheo"){
    # Mark the columns in the spectra that correspond to signal
    s1.signal <- apply(s1, 2, function(x) !all(is.na(x)))
    s2.signal <- apply(s2, 2, function(x) !all(is.na(x)))

    # Remove silence from signals
    s1.filtered <- s1[,s1.signal]
    s2.filtered <- s2[,s2.signal]

    # Estimate mutual information on signal
    mi <- infotheo::mutinformation(
      as.data.frame(cbind(s1.filtered, s2.filtered)),
      ...
    )

    # Select comparisons of interest
    s1s2 <- mi[
      1:ncol(s1.filtered),
      (ncol(s1.filtered) + 1):(ncol(s1.filtered) + ncol(s2.filtered))
      ]

    # Find the coordinates of the output matrix that correspond to each
    # syllable per syllable comparison
    is.onset <- function(x){
      c(x[1], !x[1:(length(x) - 1)] & x[2:length(x)])
    }
    is.offset <- function(x){
      c(x[1:(length(x) - 1)] & !x[2:length(x)], x[length(x)])
    }
    s1.sylls <- data.frame(
      on = which(is.onset(s1.signal)),
      off = which(is.offset(s1.signal))
    )
    s2.sylls <- data.frame(
      on = which(is.onset(s2.signal)),
      off = which(is.offset(s2.signal))
    )
    sylls.comp <- expand.grid(1:nrow(s1.sylls), 1:nrow(s2.sylls))
    sylls.coord <- mapply(FUN = function(x, y){
      x[y,]
    },
    x= list(s1= s1.sylls, s2= s2.sylls),
    y= sylls.comp
    )
    sylls.coord <- do.call("data.frame", sylls.coord)
    colnames(sylls.coord) <- c("s1.on", "s1.off", "s2.on", "s2.off")

    # Map out the corresponding coordinates in the mutual information matrix
    dursil <- function(x){
      x[,2] - x[,1]
    }
    s1.dur <- dursil(s1.sylls)
    s2.dur <- dursil(s2.sylls)
    s1.sylls.mi <- data.frame(
      on= cumsum(c(1, s1.dur[1:(length(s1.dur) - 1)] + 1))
    )
    s1.sylls.mi$off <- s1.sylls.mi + s1.dur
    s2.sylls.mi <- data.frame(
      on= cumsum(c(1, s2.dur[1:(length(s2.dur) - 1)] + 1))
    )
    s2.sylls.mi$off <- s2.sylls.mi + s2.dur

    sylls.comp.mi <- expand.grid(1:nrow(s1.sylls.mi), 1:nrow(s2.sylls.mi))
    sylls.coord.mi <- mapply(FUN = function(x, y){
      x[y,]
    },
    x= list(s1= s1.sylls.mi, s2= s2.sylls.mi),
    y= sylls.comp.mi
    )
    sylls.coord.mi <- do.call("data.frame", sylls.coord.mi)
    colnames(sylls.coord.mi) <- c("s1.on", "s1.off", "s2.on", "s2.off")

    # Fill the output matrix with MI values
    totalbins <- ncol(s1) * ncol(s2)
    mi <- rep(NA, totalbins)
    mi <- matrix(mi, nrow=ncol(s1))

    for (i in 1:nrow(sylls.coord)){
      mi[
        sylls.coord$s1.on[i]:sylls.coord$s1.off[i],
        sylls.coord$s2.on[i]:sylls.coord$s2.off[i]
        ] <- s1s2[
          sylls.coord.mi$s1.on[i]:sylls.coord.mi$s1.off[i],
          sylls.coord.mi$s2.on[i]:sylls.coord.mi$s2.off[i]
          ]
    }
  }

  return(mi)
}
