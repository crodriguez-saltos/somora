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

  spectro_mat1 <- spectro(s2, plot= F, wl= 2048, ovlp= 90)
  spectro_mat1.limit <- spectro_mat1$freq < 8 & spectro_mat1$freq > 0.8
  spectro_mat1 <- spectro_mat1$amp[spectro_mat1.limit,]

  spectro_mat2 <- spectro(s1, plot= F, wl= 2048, ovlp= 90)
  spectro_mat2.limit <- spectro_mat2$freq < 8& spectro_mat2$freq > 0.8
  spectro_mat2 <- spectro_mat2$amp[spectro_mat2.limit,]

  par(mar = c(1, 2, 2, 0))
  image(t(spectro_mat1[,ncol(spectro_mat1):1]), axes= F)

  par(mar = c(2, 1, 0, 2))
  image(spectro_mat2[,ncol(spectro_mat2):1], axes= F)
}
