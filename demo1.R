
source('cellh5/cellh5.R')
# library("grid")


primary <- "primary__primary"
c5f <- CellH5(file="data/_all_positions.ch5")
chreg <- C5ChannelRegions(c5f)
plates <- C5Plates(c5f)
positions <- C5Positions(c5f, plates[[1]])
tl = C5Timelapse(positions[[1]])
clfeatures <- C5FeatureNames(c5f, primary)

main_features = c("n2_avg", "n2_stddev", "roisize")
main_features <- clfeatures[c(1,2,3)]
frames_ <-  c(0,1,2,3,4,5,6,7,8)

object_counts <- C5ObjectCounts(c5f, positions[[1]], primary)
features <- C5FeaturesByName(c5f, positions[[1]], primary,
                             main_features, frames=frames_)

print(paste("Number of plates: ", length(positions)))
C5Close(c5f)