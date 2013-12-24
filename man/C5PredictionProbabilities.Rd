\name{C5PredictionProbabilities}
\alias{C5PredictionProbabilities}
\title{Read out the prediction probabilities.}
\description{Read out the prediction probabilities of all object for a particular 
  position and channel_region)}
\usage{
C5PredictionProbabilities(ch5file, position, channel_region, ...)
}
\arguments{
  \item{postions}{Item of \code{\link{C5Positions}}}
  \item{channel_region}{A channel-region pair e.g. \code{primary__primary}}
  \item{frames}{List of frames numbers. Default is \code{NULL} meaning all available frames.}
}
\value{
  \code{C5PredcitonsProbablities} returns a data.frame of the predictions 
  probabilities.
}
\author{Rudolf Hoefler <rudolf.hoefler@imba.oeaw.ac.at>}