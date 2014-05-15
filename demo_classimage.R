source("R/cellh5.R")
# library("cellh5")

primary <- "primary__primary"
c5f <- CellH5(file="data/_all_positions.ch5")
chreg <- C5ChannelRegions(c5f)
plates <- C5Plates(c5f)
positions <- C5Positions(c5f, plates[[1]])

# using the filename option would save a file to disk, otherwise a window pops up
C5ContourImage(c5f, positions[[1]], primary, frame=1, zstack=1) #,
               # filename="/Users/hoefler/testi.png")