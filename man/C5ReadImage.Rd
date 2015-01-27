\name{C5ReadImage}
\alias{C5ReadImage}
\title{Read an raw image as array}
\description{Read an image as numerical data for given coordinates.}
\usage{
C5ReadImage(ch5file, position, channel_region, frame_index, zstack, label_image = FALSE, ...)
}
\arguments{
  \item{ch5file}{\code{\link{CellH5}} instance}
  \item{position}{\code{item of \link{C5Positions}}}
  \item{channel_region}{A channel-region pair e.g. \code{primary__primary}}
  \item{frame_index}{frame number (integer)}
  \item{zstack}{integer value of the zstack (integer). If no zstacks present use \code{1}}
  \item{label_image}{Boolean, if \code{TRUE}, the label image is returned.}
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
  \code{\link{C5HasEvents}},
  \code{\link{C5HasObjects}},
  \code{\link{C5HasTimelapse}},
  \code{\link{C5ObjectDetails}},
  \code{\link{C5HasClassifiedObjects}},
  \code{\link{C5GalleryImageByIndex}},
  \code{\link{C5ExportGallery}},
  \code{\link{C5Contours}},
  \code{\link{C5ContourImage}},
  \code{\link{C5ClosePositions}},
  \code{\link{c5read}},
  }
