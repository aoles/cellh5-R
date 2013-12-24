\name{C5Positions}
\alias{C5Positions}
\title{Returns a list of positions (hdf5 groups)}
\description{Return a list of hdf5 groups for further processing. }
\usage{C5Positions(ch5file, plate, ...)}
\arguments{
  \item{ch5file}{\code{\link{CellH5}} instance}
  \item{plate}{plate name see \code{\link{C5Plates}}}
}
\author{Rudolf Hoefler <rudolf.hoefler@imba.oeaw.ac.at>}
\note{The posistions need to be released to not run out of memory.}