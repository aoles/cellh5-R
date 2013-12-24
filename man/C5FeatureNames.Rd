\name{C5FeatureNames}
\alias{C5FeatureNames}
\title{Read out the feature names used for a particular classifier}
\description{
Read out the feature names used for a particular classifier which was setup for 
particular channel-region
}
\usage{
C5FeatureNames(ch5file, channel_region, ...)
}
\arguments{
  \item{postions}{\code{Item of \link{C5Positions}}}
  \item{channel_region}{A channel-region pair e.g. \code{primary__primary}}
}
\value{Return an array
}
\author{Rudolf Hoefler <rudolf.hoefler@imba.oeaw.ac.at>}