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
    # Mark the lines in the spectra that correspond to signal
    s1.signal <- apply(s1, 2, function(x) !all(is.na(x)))
    s2.signal <- apply(s2, 2, function(x) !all(is.na(x)))

    # Remove silence from signals
    s1.ncols <- ncol(s1)
    s2.ncols <- ncol(s2)
    s1.filtered <- s1[,s1.signal]
    s2.filtered <- s2[,s2.signal]

    # Estimate mutual information on signal
    mi <- infotheo::mutinformation(
      as.data.frame(cbind(s1.filtered, s2.filtered)),
      ...
    )

    # Select comparisons of interest
    s1s2.val <- mi[
      1:ncol(s1.filtered),
      (ncol(s1.filtered) + 1):(ncol(s1.filtered) + ncol(s2.filtered))
      ]
    s1s2.ncol <- ncol(s1s2.val)
    s1s2.nrow <- nrow(s1s2.val)
    s1s2 <- expand.grid(1:s1s2.nrow, 1:s1s2.ncol)
    colnames(s1s2) <- c("row", "col")
    s1s2$value <- as.numeric(s1s2.val)

    # Fill output matrix with mi
    simat <- expand.grid(1:s1.ncols, 1:s2.ncols)
    colnames(simat) <- c("row", "col")
    simat$value <- NA

    j <- 1
    for (i in 1:nrow(simat)){
      if (s1.signal[simat$row[i]] & s2.signal[simat$col[i]]){
        simat$value[i] <- s1s2$value[j]
        j <- j + 1
      }
    }

    simat <- tidyr::spread(data = simat, key= col, value = value)
    simat <- dplyr::select(simat, -row)
    simat <- as.matrix(simat)
    mi <- simat
  }
  return(list(
    mi= mi
  ))
}
