#'@export

sim_MI <- function(spectro1, spectro2, type= "emp", ...){
  mi <- matrix(data = NA, nrow = ncol(spectro1), ncol = ncol(spectro2))
  h1 <- mi
  h2 <- mi
  nmi <- rep(NaN, ncol(spectro1) * ncol(spectro2))
  if (type == "symba"){
    for (i in 1:ncol(spectro1)){
      for (j in 1:ncol(spectro2)){
        pixel <- symba(x = spectro1[,i], y = spectro2[,j], plot = F)
        mi[i,j] <- pixel$I
        h1[i,j] <- pixel$h1
        h2[i,j] <- pixel$h2
        nmi[i,j] <- pixel$I / pixel$h1
      }
    }
  }else if (type == "emp"){
    h1 <- apply(X = spectro1, 2, infotheo::entropy)
    ij <- expand.grid(1:ncol(spectro1), 1:ncol(spectro2))

    mi <- mapply(FUN = function(x, y) {
      infotheo::mutinformation(x, y)
    },
    x = as.data.frame(spectro1)[ij$Var1],
    y = as.data.frame(spectro2)[ij$Var2]
    )
    mi <- matrix(mi, nrow= ncol(spectro1))

    nmi <- mapply(FUN = function(x,y) x/y,
                  x= as.data.frame(t(mi)), y= h1)
    nmi <- t(nmi)

  }
  return(list(
    h1= h1,
    nmi= nmi
  ))
}
