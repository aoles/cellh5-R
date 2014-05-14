library("grid")
# library("cellh5")

#vp1 <- viewport(x=0.1, y=0.8, width=0.1, height=0.2, angle=0)
# pushViewport(vp1)
# grid.show.viewport(vp1)
source("R/cellh5.R")

toRaster = function(x, cuts=-1:255+0.5, 
                    colors = colorRampPalette(c("black","white"))(256)) {
  cux =cut(x, cuts, include.lowest = TRUE, labels=FALSE)
  rv = x
  rv[] = colors[cux]
  return(t(rv))
  }

native = function(value) {
  return(unit(value, "native"))
}

primary <- "primary__primary"
c5f <- CellH5(file="data/_all_positions.ch5")
chreg <- C5ChannelRegions(c5f)
plates <- C5Plates(c5f)
positions <- C5Positions(c5f, plates[[1]])
contours <- C5Contours(c5f, positions[[1]], primary, frame=1)
image_ <- C5ReadImage(c5f, positions[[1]], primary, frame=1, zstack=1)

dim_<- dim(image_)
if (dev.cur() != 1) {
  dev.off()
  print("closing current device")
}
# png(file="/Users/hoefler/testi.png", width=dim_[1], height=dim_[2])
vp <- viewport(xscale=c(1, dim_[1]), yscale=c(dim_[2],1), default.units="native")
pushViewport(vp)
# grid.rect(x=0.5, y=0.5, width=100, height=100, 
#            gp = gpar(lty = "dashed"))
grid.raster(toRaster(image_), x=dim_[1]/2, y=dim_[2]/2,
            width=dim_[1], height=dim_[2],
            default.units="native", interpolate=FALSE)
for (i in 1:length(contours)) {
  x <- contours[[i]][, 1]
  y <- contours[[i]][, 2]
  grid.polygon(x, y, default.units="native", gp=gpar(fill=FALSE, col="#ff0000"))
}
grid.text("foobar", x=dim_[1]/2, y=dim_[2]/2, 
          default.units="native", gp = gpar(col="red", fontsize=12))

