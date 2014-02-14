\name{C5Center}
\alias{C5Center}
\title{Return the centers of the object.}
\description{Return the centers of the objects for a particular position and channel-region.
The function returns \code{NULL} if the position contains no information about centers.}
\usage{
centers <- C5Centers(position, channel_region)}
\arguments{
  \item{position}{Item of \code{\link{C5Positions}}}
  \item{channel_region}{A channel-region pair e.g. \code{primary__primary}}
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
