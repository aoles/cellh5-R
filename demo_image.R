source("R/cellh5.R")
# library("cellh5")

primary <- "primary__primary"
c5f <- CellH5(file="data/_all_positions.ch5")
chreg <- C5ChannelRegions(c5f)
plates <- C5Plates(c5f)
positions <- C5Positions(c5f, plates[[1]])

image_ <- C5ReadImage(c5f, positions[[1]], primary, frame=1, zstack=1)
label_image <- C5ReadImage(c5f, positions[[1]], primary, frame=10, zstack=1, label_image=TRUE)

colors = grey.colors(256)
image(label_image, col=colors, axes=FALSE, useRaster=TRUE)
image(image_, col=colors, axes=FALSE, useRaster=TRUE)
