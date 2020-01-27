#' Summarize data on mutual information for a batch of recordings
#'
#' @param scof Name of file with MI scores
#' @param nafile File containing positions with NA values (See details)
#' @param durdf Data frame containing durations of sound files
#' @param plot Logical. If true, the distribution of mutual information scores
#'   in each comparison is plotted.
#'
#' @details
#'
#' Before estimating the imitation score, the position of the spectral windows
#' with missing values in tutor song are required, so that the scores can be
#' adjusted accordingly. For each tutor, I had a dataframe with a column
#' containing the absolute position in the discretized spectrogram of the tutor
#' and another column with a T/F value, depending on whether at that position
#' there is only missing data. I needed to convert the absolute position to
#' relative position.
#'
#' If a duration data frame is provided, the script tests whether the total
#' duration of sound files had an effect on the imitation scores.
#' @export

imitationScores <- function(scof, nafile, durdf= NULL, plot= F){
  # Import data
  input_ext <- tools::file_ext(scof)

  if (input_ext == "txt"){
    sco.all <- read.table(scof)

    sco.all$bird <- sapply(sco.all$file, FUN = function(x){
      strsplit(as.character(x), split = "_")[[1]][1]
    })
    sco.all$bird <- as.factor(sco.all$bird)
  }else if (input_ext == "rds"){
    sco.all <- readRDS(scof)
  }

  # Include information on positions with NAs
  napos <- read.table(nafile)
  napos <- plyr::ddply(
    .data = napos,
    .variables = plyr::.(bird, tutor),
    .fun = function(x){
      x$relpos <- x$pos / tail(x$pos, n= 1)
      return(x)
    })

  # Imitation scores
  imitation_scores <- data.frame(
    bird= levels(sco.all$bird),
    father= rep(NA, length(levels(sco.all$bird))),
    neighbor= rep(NA, length(levels(sco.all$bird)))
  )

  if (plot){
    print("Distribution of mutual information scores per comparison")
  }

  for (i in levels(sco.all$bird)){
    sco <- sco.all[sco.all$bird == i,]

    if (plot){
      print(i)
      p.fath <- similarityDistr(
        pos= sco$father_match[sco$best_match == "father"],
        score = abs(sco$difference[sco$best_match == "father"]),
        title= "Matches in father song",
        plottype = "point", alpha = 1/50,
        ylab= "dNMI score", plot= F
      )

      p.neigh <- similarityDistr(
        pos= sco$neighbor_match[sco$best_match == "neighbor"],
        score = abs(sco$difference[sco$best_match == "neighbor"]),
        title= "Matches in neighbor song",
        plottype = "point", alpha = 1/50,
        ylab= "dNMI score", plot= F
      )
      gridExtra::grid.arrange(p.fath, p.neigh, ncol= 2)
    }

    imitation_scores$father[imitation_scores$bird == i] <- referenceImitation(
      pos= sco$father_match[sco$best_match == "father"],
      score = abs(sco$difference[sco$best_match == "father"]),
      plot= F,
      na.rm = T,
      valid = napos$relpos[napos$bird == toupper(i) & napos$tutor == "father" & !napos$na]
    )

    imitation_scores$neighbor[imitation_scores$bird == i] <- referenceImitation(
      pos= sco$neighbor_match[sco$best_match == "neighbor"],
      score = abs(sco$difference[sco$best_match == "neighbor"]),
      plot= F,
      na.rm = T,
      valid = napos$relpos[napos$bird == toupper(i) & napos$tutor == "neighbor" & !napos$na]
    )

  }

  # Formatting data frame
  imitation_scores2 <- reshape2::melt(
    imitation_scores,
    id.vars = "bird",
    variable.name = "tutor",
    value.name= "score"
  )

  # Scores are in percentages, they will be changed them to proportions
  imitation_scores2$score <- imitation_scores2$score / 100

  if (plot){
    # Filter by father
    imitation.father <- imitation_scores2[imitation_scores2$tutor == "father",]

    # The bars will be ordered according to magnitude of father score.
    imitation.father$bird <- factor(
      x = imitation.father$bird,
      levels = levels(imitation.father$bird)[order(imitation.father$score)]
    )
    levels(imitation.father$bird) <- toupper(levels(imitation.father$bird))

    # Normalizing imitation scores
    imit.max <- max(imitation.father$score)
    imitation.father$norm <- imitation.father$score / imit.max

    # Plot
    p <- ggplot2::ggplot(
      data = imitation.father,
      ggplot2::aes(x= bird, y= norm)
    )
    p <- p + ggplot2::geom_bar(stat= "identity")
    p <- p + ggplot2::theme_classic() + ggplot2::theme(
      text= ggplot2::element_text(size= 20),
      axis.text.x = ggplot2::element_text(angle = 90)
    )
    p <- p + ggplot2::ylab(expression(atop(
      "Imitation score for father song",
      "(normalized)")))
    p <- p + ggplot2::xlab("Juvenile")
    print(p)

    # Filter by father
    imitation.neighbor <- imitation_scores2[imitation_scores2$tutor == "neighbor",]

    # The bars will be ordered according to magnitude of father score.
    imitation.neighbor$bird <- factor(
      x = imitation.neighbor$bird,
      levels = levels(imitation.neighbor$bird)[order(imitation.neighbor$score)]
    )
    levels(imitation.neighbor$bird) <- toupper(levels(imitation.neighbor$bird))

    # Normalizing imitation scores
    imit.max <- max(imitation.neighbor$score)
    imitation.neighbor$norm <- imitation.neighbor$score / imit.max

    # Plot
    p <- ggplot2::ggplot(
      data = imitation.neighbor,
      ggplot2::aes(x= bird, y= norm)
    )
    p <- p + ggplot2::geom_bar(stat= "identity")
    p <- p + ggplot2::theme_classic() + ggplot2::theme(
      text= ggplot2::element_text(size= 20),
      axis.text.x = ggplot2::element_text(angle = 90)
    )
    p <- p + ggplot2::ylab(expression(atop(
      "Imitation score for father song",
      "(normalized)")))
    p <- p + ggplot2::xlab("Juvenile")
    print(p)
  }

  if(!is.null(durdf)){
    # Get duration per bird
    duration_perbird <- plyr::ddply(
      .data = durdf,
      .variables = plyr::.(bird),
      .fun = function(x){
        sum(x$seconds)
      })
    colnames(duration_perbird)[colnames(duration_perbird) == "V1"] <- "duration"

    plot(density(duration_perbird$duration), main= "Distribution of duration of recordings")
    print("Summary statistics for duration of sound files")
    summary(duration_perbird)
    print("Standard deviation of duration of sound files")
    sd(duration_perbird$duration)

    imitation_scores$bird <- toupper(as.character(imitation_scores$bird))
    duration_perbird$bird <- as.character(duration_perbird$bird)
    imit_dur <- merge(imitation_scores, duration_perbird)
    imit_dur$bird <- as.factor(imit_dur$bird)
    plot(imit_dur)

    print(cor.test(imit_dur$duration, imit_dur$father, method= "spearman"))
    print(cor.test(imit_dur$duration, imit_dur$neighbor, method= "spearman"))

  }

  return(imitation_scores2)
}

