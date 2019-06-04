sim_MI <- function(spectro1, spectro2, type= "emp", ...){
  mi <- matrix(data = NA, nrow = ncol(spectro1), ncol = ncol(spectro2))
  h1 <- mi
  h2 <- mi
  nmi <- mi
  for (i in 1:ncol(spectro1)){
    for (j in 1:ncol(spectro2)){
      if (type == "symba"){
        pixel <- symba(x = spectro1[,i], y = spectro2[,j], plot = F)
        mi[i,j] <- pixel$I
        h1[i,j] <- pixel$h1
        h2[i,j] <- pixel$h2
        nmi[i,j] <- pixel$I / pixel$h1
      }else if (type == "emp"){
        s1 <- infotheo::discretize(spectro1[,i], ...)
        s2 <- infotheo::discretize(spectro2[,j], ...)
        h1[i,j] <- infotheo::entropy(s1)
        h2[i,j] <- infotheo::entropy(s2)
        nmi[i,j] <- infotheo::mutinformation(s1, s2) / h1[i,j]
      }
    }
  }
  return(list(
    mi= mi,
    h1= h1,
    h2= h2,
    nmi= nmi
  ))
}
