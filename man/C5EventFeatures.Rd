\name{C5EventFeatures}
\alias{C5EventFeatures}
\title{Return the feature vector per track found by the event selection}
\description{Returns a 3-dimensional array of features similar to the trajectory
matrices. Each row is a track, each column is a frame. The 3rd dimenension is the
feature vector for a particular object. The function returns
\code{NULL} if the position contains no events.
}
\usage{
event_features <- C5EventFeatures(ch5file, position, mask, include_branches = TRUE, feature_names = NULL)
}
\arguments{
  \item{ch5file}{ \code{\link{CellH5}} instance }
  \item{position}{\code{Item of \link{C5Positions}}}
  \item{mask}{A segmentation mask identifier e.g. \code{primary__primary}}
  \item{include_branches}{If FALSE only one branch of a split track
    is taken into account.}
  \item{feature_names}{
list of feature names e.g. \code{c("n2_avg", "n2_stddev", "roisize")} for mean
intensity, standard deviation of the intensity and the size of the object. See
\code{\link{C5FeatureNames}} to obtain a list of feature names.
}
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
  \code{\link{C5GalleryImageByIndex}},
  \code{\link{C5ExportGallery}},
  \code{\link{C5Contours}},
  \code{\link{C5ContourImage}},
  \code{\link{C5ClosePositions}},
  \code{\link{c5read}},
  }
