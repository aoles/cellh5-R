source("R/cellh5.R")
# library("cellh5")

primary <- "primary__primary"
c5f <- CellH5(file="data/_all_positions.ch5")
chreg <- C5ChannelRegions(c5f)
plates <- C5Plates(c5f)
positions <- C5Positions(c5f, plates[[1]])

pos <- positions$W0_P0037
colors = grey.colors(256)
if (C5HasEvents(pos)) {
  # events <- C5Events(c5f, positions[[1]], primary, include_branches=TRUE, return_indices=TRUE)
  # index <- events[[1, 1]]
  # gal <- C5GalleryImageByIndex(c5f, positions[[1]], primary, index, zstack=1, gallery_size=100)
  # image(gal, col=colors, axes=FALSE, useRaster=TRUE)
  C5ExportGallery(c5f, "/tmp", pos, primary, include_branches=TRUE, gallery_size=60)
} else {
  print("no events found!")
}