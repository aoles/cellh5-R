\name{C5ChannelRegion}
\alias{C5ChannelRegion}
\title{Information about the segmentation regions
}
\description{
Retruns a list of the segmentation regions (channel-region pairs).
}
\usage{
cheannel_regions <- C5ChannelRegion(ch5file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ch5file}{ \code{\link{CellH5}} instance }
}
\note{There are different color channel mapped to primary, secondary or tertiary 
channel. Each channel can have different regions e.g. exanded or outside. 
A channel-region pair is encoded as string consisting of the channel name and 
region name, separated by two underscores e.g. \code{primary__primary}}

\author{Rudolf Hoefler <rudolf.hoefler@imba.oeaw.ac.at>}