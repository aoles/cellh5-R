\name{C5Events}
\alias{C5Events}
\title{Read out the tracks found by the event selection.}
\description{Read out the tracks found by the event selection.}
\usage{
events <- C5Events(ch5file, position, channel_region, include_branches = TRUE, return_indices = FALSE, ...)
}

\arguments{
  \item{ch5file}{ \code{\link{CellH5}} instance }
  \item{postions}{\code{Item of \link{C5Positions}}}
  \item{channel_region}{A channel-region pair e.g. \code{primary__primary}}
  \item{frames}{List of frames numbers. Default is \code{NULL} meaning all available frames.}
  \item{include_branches}{If FALSE only one branch of a split track 
    is taken into account}
  \item{return_indices}{If TRUE the items of the array are the object indices}
}
\value{\code{Returns a 2x2 array}}
\author{Rudolf Hoefler <rudolf.hoefler@imba.oeaw.ac.at>}\seealso{
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