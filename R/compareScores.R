#' Compare similarity scores for two songs
#'
#' @param scof Score file addresses for the two comparisons. The order of the
#'   comparisons, 1 or 2, does not matter here. Value must be vector of two
#'   character strings.
#' @param song1flag Flag in the name of each score file that marks the
#' @param c1s1 Identifier of song 1 in comparison 1.
#' @param c2s1 Identifier of song 1 in scof2. identifier of song 1.
#' @param plotdiff Logical. Should the score differences be plotted?
#'
#' @details This function calculates the difference in similarity scores for two
#'   songs, asumming that in each comparison the second songis the same. The
#'   function reads the scores calculated over each spectral window of song 2
#'   and substracts the score in the second comparison from the score in the
#'   first comparison.
#'
#'   Song 1 in each comparison must have a unique identifier expressed as a
#'   character string. The identifier needs to be unique only between the two
#'   score files loaded into the function. The identifier should be present in
#'   the name of the score file and marked by a flag, which must also be
#'   specified by the user. The format of the flag and the identifier is as
#'   follows:
#'
#'   _<flag>-<identifier><underscore or file extension>
#'
#'   In the following example the identifiers are "father" and "neighbor" and
#'   the flag is "tutor". They are written into the filenames in this way:
#'
#'   comparison1_tutor-father.sco
#'
#'   comparison2_tutor-neighbor.sco
#'
#'   Because a file may contain multiple flags, the user must specify the flag,
#'   and the identifiers, when calling the function.
#'
#'   Normalized mutual information matrices (nmi file) must be in the same
#'   folder as the score file. All score files must have their respective
#'   accompanying nmi file.
#' @export

compareScores <- function(
  scof,
  song1flag= "tutor", c1s1, c2s1,
  plotdiff= F
  ){
  # Function to order files according to identifiers----
  orderbyId <- function(x){
    # x - character vector with names of files
    forder <- c(paste0("_", song1flag, "-", c1s1),
                paste0("_", song1flag, "-", c2s1))
    forder <- sapply(forder, FUN = function(i){
      grep(pattern = i, x = x)
    })
    return(x[forder])
  }

  # Import score files----
  scof <- orderbyId(scof)
  sco <- as.list(scof)

  for (i in 1:length(scof)){
    sco[[i]] <- scof[[i]]
    sco[[i]]<- read.table(scof[[i]])
    sco[[i]]$tutor <-  getValues(scof[[i]], song1flag, ".sco")
    sco[[i]]$file <- strsplit(x = scof[[i]], split = "_tutor")[[1]][1]
    sco[[i]]$file <- strsplit(x= sco[[i]]$file, split= "_vs_")[[1]][2]
  }
  sco <- do.call("rbind", sco)

  #str(sco)

  # Filter data frame----
  sco <- sco[!is.na(sco$score),]
  sco <- sco[!(sco$score == Inf),]
  sco <- sco[!(sco$score == -Inf),]

  # Format data frame----
  sco$tutor <- as.factor(sco$tutor)
  sco$song2_rel_pos <- as.factor(sco$song2_rel_pos)
  sco$file <- as.factor(sco$file)

  if(length(levels(sco$file)) > 1){
    stop("Error with data frame. Check that song2 in each comparison is the same")
  }

  # Compute differences in scores----
  sco.scores <- reshape2::dcast(
    data = sco,
    formula = as.formula(paste0("file + song2_rel_pos ~ ", song1flag)),
    value.var = "score"
  )

  sco.match <- reshape2::dcast(
    data = sco,
    formula = as.formula(paste0("file + song2_rel_pos ~ ", song1flag)),
    value.var = "song1_match"
  )

  sco.scores$difference <- sco.scores[[c1s1]] - sco.scores[[c2s1]]

  exactzeroes <- sco.scores$difference == 0
  sco.scores <- sco.scores[!exactzeroes,]
  sco.match <- sco.match[!exactzeroes,]

  sco.scores$best_match <- ifelse(sco.scores$difference < 0, c2s1, c1s1)
  colnames(sco.scores)[colnames(sco.scores) == c1s1] <- paste0(c1s1, "_score")
  colnames(sco.scores)[colnames(sco.scores) == c2s1] <- paste0(c2s1, "_score")
  colnames(sco.match)[colnames(sco.match) == c1s1] <- paste0(c1s1, "_match")
  colnames(sco.match)[colnames(sco.match) == c2s1] <- paste0(c2s1, "_match")
  sco.scores <- merge(sco.scores, sco.match)
  rm(sco.match)
  sco.scores$pos_in_match <- as.numeric(apply(
    X = sco.scores, MARGIN = 1, FUN = function(x){
    x[paste0(x["best_match"], "_match")]
  }))

  if(plotdiff){
    plot(density(sco.diff$difference, na.rm= T))
    abline(v= 0, col="red")
  }

  return(sco.scores)
}
