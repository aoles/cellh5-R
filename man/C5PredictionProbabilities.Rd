\name{C5PredictionProbabilities}
\alias{C5PredictionProbabilities}
\title{Read out the prediction probabilities.}
\description{Read out the prediction probabilities of all object for a particular
  position and channel_region. The function returns \code{NULL} if the position
  contains no prediction probabilities.}
\usage{
probs <- C5PredictionProbabilities(ch5file, position, channel_region)
}
\arguments{
  \item{position}{Item of \code{\link{C5Positions}}}
  \item{channel_region}{A channel-region pair e.g. \code{primary__primary}}
}
\value{
  \code{C5PredcitonsProbablities} returns a data.frame of the predictions
  probabilities.
}
\author{Rudolf Hoefler <rudolf.hoefler@imba.oeaw.ac.at>}
\seealso{
  \code{\link{C5Close}},
  \code{\link{C5FileInfo}},
  \code{\link{C5Plates}},
  \code{\link{C5Positions}},
  \code{\link{C5Timelapse}},
  \code{\link{C5TimeIdx}},
  \code{\link{C5ObjectCounts}},
  \code{\link{C5ObjectLabels}},
  \code{\link{C5ReadImage}},
  \code{\link{C5ChannelRegions}},
  \code{\link{C5ClassifierDefinition}},
  \code{\link{C5FeatureNames}},
  \code{\link{C5FeaturesByName}},
  \code{\link{C5Events}},
  \code{\link{C5EventFeatures}},
  \code{\link{C5Center}},
  \code{\link{C5BoundingBoxes}},
  \code{\link{C5Orientation}},
  \code{\link{C5Predictions}},
  \code{\link{C5PredictionProbabilities}},
  }
