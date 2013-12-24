\name{C5OBoundingBoxes}
\alias{C5OBoundingBoxes}
\title{Return the bounding boxes of the object.}
\description{Return the bounding boxes of all objects for a particular position and channel-region.}
\usage{
C5OBoundingBoxes(postion, channel_region, frames=NULL)}
\arguments{
  \item{postions}{\code{Item of \link{C5Positions}}}
  \item{channel_region}{A channel-region pair e.g. \code{primary__primary}}
  \item{frames}{List of frames numbers. Default is \code{NULL} meaning all available frames.}
}
\author{Rudolf Hoefler <rudolf.hoefler@imba.oeaw.ac.at>}