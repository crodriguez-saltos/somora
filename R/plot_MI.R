#'@export

plot_MI <- function(mi, s1, s2, type= "normal", thrs= NULL){
  if (!(is.null(thrs))){
    mi <- mi > thrs
  }

  if (type == "match"){
    for (i in 1:nrow(mi)){
      mi[i,] <- mi[i,] * (mi[i,] == max(mi[i,]))
    }
  }

  layout(mat= matrix(c(2,1,0,3),
                     nrow= 2,
                     ncol= 2),
         heights = c(0.75,2),
         widths = c(2,0.75))
  par(mar = c(2, 2, 0, 0))
  image(t(mi[nrow(mi):1,ncol(mi):1]), col= topo.colors(10), axes= F)

  par(mar = c(1, 2, 2, 0))
  image(t(s2[,ncol(s2):1]), axes= F)

  par(mar = c(2, 1, 0, 2))
  image(s1[,ncol(s1):1], axes= F)
}
