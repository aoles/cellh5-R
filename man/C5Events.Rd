\name{C5Events}
\alias{C5Events}
\title{Read out the tracks found by the event selection.}
\description{Read out the tracks found by the event selection.}
\usage{
C5Events(ch5file, position, channel_region, include_branches = TRUE, return_indices = FALSE, ...)
}

\arguments{
  \item{postions}{\code{Item of \link{C5Positions}}}
  \item{channel_region}{A channel-region pair e.g. \code{primary__primary}}
  \item{frames}{List of frames numbers. Default is \code{NULL} meaning all available frames.}
  \item{include_branches}{If FALSE only one branch of a split track 
    is taken into account}
  \item{return_indices}{If TRUE the items of the array are the object indices}
}
\value{\code{Returns a 2x2 array}}
\author{Rudolf Hoefler <rudolf.hoefler@imba.oeaw.ac.at>}