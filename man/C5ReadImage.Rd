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