\name{C5FeatureByName}
\alias{C5FeatureByName}
\title{Read out the features}
\description{
Read out the features for a particular channel-region
}
\usage{
C5FeatureNames(ch5file, channel_region, feature_names, frame=NULL, ...)
}
\arguments{
  \item{postions}{\code{Item of \link{C5Positions}}}
  \item{channel_region}{A channel-region pair e.g. \code{primary__primary}}
  \itme{feature_names}{List of feature names}
}
\value{Return an array
}
\author{Rudolf Hoefler <rudolf.hoefler@imba.oeaw.ac.at>}