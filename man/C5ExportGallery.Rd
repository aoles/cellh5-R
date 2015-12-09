\name{C5ExportGallery}
\alias{C5ExportGallery}
\title{C5ExportGallery}
\description{
Export events as gallery images (one file per track). The output format is png.
}
\usage{
C5ExportGallery(c5f, outdir, position, mask, include_branches=FALSE, gallery_size=60)
}

\arguments{
  \item{ch5file}{\code{\link{CellH5}} instance}
  \item{outdir}{Out directory where the images will be saved.}
  \item{position}{Item of \code{\link{C5Positions}}}
  \item{mask}{A segmentation mask identifier e.g. \code{primary__primary}}
  \item{include_branches}{If FALSE only one branch of a split track
    is taken into account}
  \item{gallery_size}{Size in pixels of the gallery image. Default is 60}
}

\value{
Returns \code{NULL}. The files follow a naming scheme e.g. \code{TR48__W0__S0037__T00096__O0315.png}.
The meaning is: track, well, site (former position), time and object.
}
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
  \code{\link{C5GalleryImageByIndex}},
  \code{\link{C5ExportGallery}},
  \code{\link{C5Contours}},
  \code{\link{C5ContourImage}},
  \code{\link{C5ClosePositions}},
  \code{\link{c5read}},
  }
