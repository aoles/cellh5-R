\name{c5read}
\alias{c5read}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read arbitrary tables from the cellh5 file.
}
\description{
Read arbitrary data sets from a cellh5 file. This method is mentioned
to give low-level access to a cellh5 file.

This method is equivalent to the \link{h5read} method of the rhdf5 package except it throws
a different error message if a subgroup does not exits (more readable to not programmers).
}
\usage{
c5read(position, name, index = NULL, start = NULL, stride = NULL, block = NULL, count = NULL, compoundAsDataFrame = TRUE, callGeneric = TRUE, read.attributes = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{position}{\code{Item of \link{C5Positions}. In the low-level context \code{position} could be any \link{H5IdComponent}}, but here it's usually a \code{cellh5-position}}
  \item{name}{
The name of the dataset in the cellh5 file.
}
  \item{index}{
  List of indices for subsetting. The length of the list has to agree with the dimensional extension of the HDF5 array. Each list element is an integer vector of indices. A list element equal to NULL choses all indices in this dimension. Counting is R-style 1-based.
}
  \item{start}{
The start coordinate of a hyperslab (similar to subsetting in R). Counting is R-style 1-based. This argument is ignored, if index is not NULL.
}
  \item{stride}{
The stride of the hypercube. Read the \href{http://ftp.hdfgroup.org/HDF5/Tutor/phypecont.html}{introduction} before using this argument. R behaves like Fortran in this example. This argument is ignored, if index is not NULL.
}
  \item{block}{
The block size of the hyperslab. Read the \href{http://ftp.hdfgroup.org/HDF5/Tutor/phypecont.html}{introduction} before using this argument. R behaves like Fortran in this example. This argument is ignored, if index is not NULL.
}
  \item{count}{
The number of blocks to be written. This argument is ignored, if index is not NULL.
}
  \item{compoundAsDataFrame}{
If true, a compound datatype will be coerced to a data.frame. This is not possible, if the dataset is multi-dimensional. Otherwise the compound datatype will be returned as a list. Nested compound data types will be returned as a nested list.
}
  \item{callGeneric}{
If TRUE a generic function h5read.classname will be called if it exists depending on the dataset's class attribute within the HDF5 file. This function can be used to convert the standard output of h5read depending on the class attribute. Note that h5read is not a S3 generic function. Dispatching is done based on the HDF5 attribute after the standard h5read function.
}
  \item{read.attributes}{
(logical) If TRUE, the HDF5 attributes are read and attached to the respective R object
}
}
\details{
The argument description was taken from the \link{h5read} man page. Further detail can be found there.
}

\references{
   \href{http://www.bioconductor.org/packages/release/bioc/html/rhdf5.html}{BioConductor}
   \href{http://www.hdfgroup.org/HDF5/}{HDF5 format}
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
