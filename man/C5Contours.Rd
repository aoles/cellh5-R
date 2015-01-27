\name{C5Contours}
\alias{C5Contours}
\title{
C5Contours
}
\description{
Reads segmentation contours from file. Contours are saved as compressed string to
the file. Decompression take time, therfore it is recommended to use the frame
argument to speed up reading. Contours are refered as "Crack Contours".
}
\usage{
contours <- C5Contours(ch5file, position, channel_region, frame = NULL)
}
\arguments{
  \item{ch5file}{\code{\link{CellH5}} instance}
  \item{position}{Item of \code{\link{C5Positions}}}
  \item{channel_region}{A channel-region pair e.g. \code{primary__primary}}
  \item{frame}{List of frame numbers. If this option is ommited all contours will
  be read out. It is recommended to use this option}
}

\value{
Returns a list of arrays. Lenght of the array equals the lenght of the frame argument.
The dimension of the array is Nx2, where N is the number of points in the polygon.
The other dimension simply represents a vector thats points to a pixel in the image.
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
