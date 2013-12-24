\name{C5Orientation}
\alias{C5Orientation}
\title{Angle and eccentricity of an ellipse fit.}
\description{Return the angle and eccentricity of an ellipse fit for all objects}
\usage{
C5Orientation(position, channel_region, frames = NULL)}

\arguments{ 
  \item{postions}{\code{Item of \link{C5Positions}}}
  \item{channel_region}{A channel-region pair e.g. \code{primary__primary}}
  \item{frames}{List of frames numbers. Default is \code{NULL} meaning all available frames.}
}
\note{Note that \code{angle} and \code{eccentricity} are not invariant against affine transformations}
\value{\code{Returns a data.frame}}
\author{Rudolf Hoefler <rudolf.hoefler@imba.oeaw.ac.at>}