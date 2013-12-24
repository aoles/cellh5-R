\name{C5ObjectLabels}
\alias{C5ObjectLabels}
\title{List of all Object found in the timelapse
}
\description{Returns a data.frame of all object found in the timelapse. 
The columns are \code{frame_index} and \code{object_label}. The rows are simply indices.
}
\usage{
C5ObjectLabels(position, channel_region, frames = NULL)
}
\arguments{
  \item{position}{\code{item of \link{C5Positions}}}
  \item{channel_region}{A channel-region pair e.g. \code{primary__primary}}
}
\author{Rudolf Hoefler <rudolf.hoefler@imba.oeaw.ac.at>}