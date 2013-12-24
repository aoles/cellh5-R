\name{C5TimeIdx}
\alias{C5TimeIdx}
\title{Return a list of frame numbers for each object.}
\description{Returns a list of frame numbers for each object. The lenght of the 
list is  the total number of objects found in a particular position and channel-region.}
\usage{C5TimeIdx(position, channel_region)}
\arguments{
  \item{position}{\code{item of \link{C5Positions}}}
  \item{channel_region}{A channel-region pair e.g. \code{primary__primary}}
}
\author{Rudolf Hoefler <rudolf.hoefler@imba.oeaw.ac.at>}