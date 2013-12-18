
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
frames_ <-  c(0, 1)#,1,2,3,4,5,6,7,8)

# read out data
object_counts <- C5ObjectCounts(c5f, positions[[1]], primary)
features <- C5FeaturesByName(c5f, positions[[1]], primary, main_features)#, frames=frames_)
object_ids <- C5ObjectLabels(positions[[1]], primary, frames=frames_)
center <- C5Center(positions[[1]], primary)
bbox <- C5BoundingBoxes(positions[[1]], primary)
orientation <- C5Orientation(positions[[1]], primary)
predicitons <- C5Predictions(c5f, positions[[1]], primary)
probs <- C5PredictionProbabilities(c5f, positions[[1]], primary)
image_ <- C5ReadImage(c5f, positions[[1]], primary, frame_index=1, zstack=1)[, , 1,1,1]


C5Close(c5f)