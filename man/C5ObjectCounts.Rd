\name{C5Objectcounts}
\alias{C5Objectcounts}
\title{Read out the number of cells per frame and class label}
\description{Returns a data.frame of the object counts. The column names correspond
to the class names, rows to the frame numbers.}
\usage{C5Objectcounts(ch5file, position, channel_region)}

\arguments{
  \item{ch5file}{\code{\link{CellH5}} instance}
  \item{position}{\code{item of \link{C5Positions}}}
  \item{channel_region}{A channel-region pair e.g. \code{primary__primary}}}
\author{Rudolf Hoefler <rudolf.hoefler@imba.oeaw.ac.at>}