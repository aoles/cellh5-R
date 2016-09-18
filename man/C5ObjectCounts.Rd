\name{C5ObjectCounts}
\alias{C5ObjectCounts}
\title{Read out the number of cells per frame and class label}
\description{Returns a data.frame of the object counts. The column names correspond
to the class names, rows to the frame numbers. The Function returns \code{NULL}
if the position contains no objects.}
\usage{counts <- C5ObjectCounts(ch5file, position, mask)}

\arguments{
  \item{ch5file}{\code{\link{CellH5}} instance}
  \item{position}{\code{item of \link{C5Positions}}}
  \item{mask}{A segmentation mask identifier e.g. \code{primary__primary}}}
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
