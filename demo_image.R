
library("grid")
source("cellh5/cellh5.R")
source("cellh5/cellh5_image.R")

# helper see tutorial
toRaster <- function(x, cuts=-1:255+0.5,
                     colors = colorRampPalette(c("black","white"))(256)) {
  cux =cut(x,cuts,include.lowest = TRUE, labels=FALSE)
  rv=x
  rv[] = colors[cux]
  return(rv)}


primary <- "primary__primary"
c5f <- CellH5(file="data/_all_positions.ch5")
chreg <- C5ChannelRegions(c5f)
plates <- C5Plates(c5f)
positions <- C5Positions(c5f, plates[[1]])

image_ <- C5ReadImage(c5f, positions[[1]], primary, frame=1, zstack=1)
label_image <- C5ReadImage(c5f, positions[[1]], primary, frame=1, zstack=1, label_image=TRUE)

pdf(file="demo_image.pdf")
grid.raster(toRaster(t(image_)))
dev.off()


pdf(file="demo_labelimage.pdf")
grid.raster(toRaster(t(label_image)))
dev.off()