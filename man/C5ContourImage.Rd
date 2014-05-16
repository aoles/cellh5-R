\name{C5ContourImage}
\alias{C5ContourImage}
\title{C5ContourImage}
\description{
Plots an image to the screen (or saves it as png). The image includes color coded
segmentation contours. The color code is defined in the class defintion. In case 
of no class definition the color of the contours is by default white. 
Optional the object labels can be drawn.
}
\usage{
C5ContourImage(ch5file, position, channel_region, frame, zstack = 1, filename = NULL, default_color = "#ffffff", fontsize = 12, show_labels = TRUE)
}

\arguments{
  \item{ch5file}{\code{\link{CellH5}} instance}
  \item{position}{Item of \code{\link{C5Positions}}}
  \item{channel_region}{A channel-region pair e.g. \code{primary__primary}}
  \item{index}{Object index of the cell. Index is in the range form 1 to max number of objects}
  \item{frame}{Frame number of the image}
  \item{zstack}{Index of the zsclice}
  \item{filename}{The image is saved as png to the hard disk if a filename is provided}
  \item{default_color}{Color of unclassified objects (default is white)}
  \item{fontsize}{Font size of the object labels}
  \item{show_labels}{Shows object labels if this option is \code{TRUE}}
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

