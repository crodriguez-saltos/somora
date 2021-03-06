% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_MI.R
\name{sim_MI}
\alias{sim_MI}
\title{Compare two sounds based using mutual information}
\usage{
sim_MI(s1, s2, type = "infotheo", ...)
}
\arguments{
\item{s1}{Discretized spectrogram of sound 1.}

\item{s2}{Discretized spectrogram of sound 2.}

\item{type}{Type of estimation of mutual information. See details.}

\item{...}{Additional arguments passed to infotheo::mutinformation.}
}
\description{
Compare two sounds based using mutual information
}
\details{
The types of estimation of mutual information are:

symba -  Estimate mutual information using seewave::symba.
infotheo - Estimate mutual information using package infotheo.
}
