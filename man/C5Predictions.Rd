\name{C5Predictions}
\alias{C5Predictions}
\title{Read out the predicted class names.}
\description{Read out the predicted class names of all object for a particular 
  position and channel_region)}
\usage{
C5Predictions(ch5file, position, channel_region, ...)
}
\arguments{
  \item{postions}{Item of \code{\link{C5Positions}}}
  \item{channel_region}{A channel-region pair e.g. \code{primary__primary}}
  \item{frames}{List of frames numbers. Default is \code{NULL} meaning all available frames.}
}
\value{
  \code{C5Predcitons} returns a list with the predicted class labels (list of strings)
}
\author{Rudolf Hoefler <rudolf.hoefler@imba.oeaw.ac.at>}