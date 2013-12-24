\name{C5Center}
\alias{C5Center}
\title{Return the centers of the object.}
\description{Return the centers of the objects for a particular position and channel-region.}
\usage{
centers <- C5OCenters(postion, channel_region, frames=NULL)}
\arguments{
  \item{postions}{\code{Item of \link{C5Positions}}}
  \item{channel_region}{A channel-region pair e.g. \code{primary__primary}}
  \item{frames}{List of frames numbers. Default is \code{NULL} meaning all available frames.}
}
\author{Rudolf Hoefler <rudolf.hoefler@imba.oeaw.ac.at>}