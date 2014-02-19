\name{C5HasEvents}
\alias{C5HasEvents}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Checks if a position contains events found by event selection.
}
\description{
Checks if a position contains events found by event selection. The current version
of cellh5 does not support events from different channel regions (segmentation regions)
}
\usage{
if (C5HasEvents(position) {
  print("Current position contains events")
  }
}
\arguments{
  \item{position}{\code{Item of \link{C5Positions}}}
}
\value{The return value is boolean.}
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
  \code{\link{C5HasEvents}},
  \code{\link{C5HasObjects}},
  \code{\link{C5HasTimelapse}},
  \code{\link{C5ObjectDetails}},
 \code{\link{C5HasClassifiedObjects}},
  }
