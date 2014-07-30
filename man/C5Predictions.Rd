\name{C5Predictions}
\alias{C5Predictions}
\title{Read out the predicted class names.}
\description{Read out the predicted class names of all object for a particular
  position and channel_region. The function returns \code{NULL} if the position
  contains no class predictions.}
\usage{
predictions <- C5Predictions(ch5file, position, channel_region, as="label")
}
\arguments{
  \item{position}{Item of \code{\link{C5Positions}}}
  \item{channel_region}{A channel-region pair e.g. \code{primary__primary}}
  \item{as}{string - on of "label", "name" or "color". Returns a list according to
    the class definition i.e. the class label, name or color.}
}
\value{
  \code{C5Predictons} returns an array of the predicted class labels (array of strings)
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
  }
