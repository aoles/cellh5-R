# Export Event gallery images to a director. Script is incredibly slow. Exporting
# galleries is always slow.
library("cellh5")

primary <- "primary__primary"
c5f <- CellH5(file="data/_all_positions.ch5")
chreg <- C5ChannelRegions(c5f)
plates <- C5Plates(c5f)
positions <- C5Positions(c5f, plates[[1]])


if (C5HasEvents(positions$W0_P0013)) {
  C5ExportGallery(c5f, "/tmp", positions$W0_P0013, "primary__primary", include_branches=FALSE, gallery_size=60)
} else {
  warning("no events found!")
}