\name{C5SegmentationMasks}
\alias{C5SegmentationMasks}
\title{List of the segmentation masks
}
\description{
Retruns a list of the segmentation mask identifiers e.g. "primary__primary".
}
\usage{
masks <- C5SegmentationMasks(ch5file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ch5file}{ \code{\link{CellH5}} instance }
}
\note{There are different color channels mapped to "primary", "secondary or "tertiary" channel. Each channel can have different masks e.g. exanded or outside.
A mask identifier is a string consisting of the channel name and
mask (or region) name, separated by two underscores e.g. \code{"primary__primary"}}

\author{Rudolf Hoefler <rudolf.hoefler@imba.oeaw.ac.at>}
\seealso{
  \code{\link{C5Close}},
  \code{\link{C5FileInfo}},
  \code{\link{C5Plates}},
  \code{\link{C5Positions}},
  \code{\link{C5Timelapse}},
  \code{\link{C5FrameNumbers}},
  \code{\link{C5ObjectCounts}},
  \code{\link{C5ObjectLabels}},
  \code{\link{C5ReadImage}},
  \code{\link{C5SegmentationMasks}},
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
  \code{\link{C5Contours}},
  \code{\link{C5ClosePositions}},
  \code{\link{c5read}},
  }
