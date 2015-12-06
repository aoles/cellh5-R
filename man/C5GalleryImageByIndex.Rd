\name{C5GalleryImageByIndex}
\alias{C5GalleryImageByIndex}
\title{
C5GalleryImageByIndex
}
\description{
Read a single gallery image from the hdf file. You rarely want to read out a single gallery image.
To export the galleries from the events found, use \link{C5ExportGallery}.
}
\usage{
gal_image <- C5GalleryImageByIndex(ch5file, position, mask, index, zstack=1, gallery_size=60)
}
\arguments{
  \item{ch5file}{\code{\link{CellH5}} instance}
  \item{position}{Item of \code{\link{C5Positions}}}
  \item{mask}{A segmentation mask identifier e.g. \code{primary__primary}}
  \item{index}{Object index of the cell. Index is in the range form 1 to max number of objects}
  \item{zstack}{Index of the zslice. Default is 1, there is always 1}
  \item{gallery_size}{Size in pixels of the gallery image. Default is 60}
}

\value{
Returns a matrix that can be interpreted as 8bit grey level image.
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
