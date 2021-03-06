% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compareScores.R
\name{compareScores}
\alias{compareScores}
\title{Compare similarity scores for two songs}
\usage{
compareScores(scof, song1flag = "tutor", c1s1, c2s1, plotdiff = F)
}
\arguments{
\item{scof}{Score file addresses for the two comparisons. The order of the
comparisons, 1 or 2, does not matter here. Value must be vector of two
character strings.}

\item{song1flag}{Flag in the name of each score file that marks the}

\item{c1s1}{Identifier of song 1 in comparison 1.}

\item{c2s1}{Identifier of song 1 in scof2. identifier of song 1.}

\item{plotdiff}{Logical. Should the score differences be plotted?}
}
\description{
Compare similarity scores for two songs
}
\details{
This function calculates the difference in similarity scores for two
  songs, asumming that in each comparison the second songis the same. The
  function reads the scores calculated over each spectral window of song 2
  and substracts the score in the second comparison from the score in the
  first comparison.

  Song 1 in each comparison must have a unique identifier expressed as a
  character string. The identifier needs to be unique only between the two
  score files loaded into the function. The identifier should be present in
  the name of the score file and marked by a flag, which must also be
  specified by the user. The format of the flag and the identifier is as
  follows:

  _<flag>-<identifier><underscore or file extension>

  In the following example the identifiers are "father" and "neighbor" and
  the flag is "tutor". They are written into the filenames in this way:

  comparison1_tutor-father.sco

  comparison2_tutor-neighbor.sco

  Because a file may contain multiple flags, the user must specify the flag,
  and the identifiers, when calling the function.

  Normalized mutual information matrices (nmi file) must be in the same
  folder as the score file. All score files must have their respective
  accompanying nmi file.
}
