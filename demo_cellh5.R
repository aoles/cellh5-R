# S4 class with some convenience function to read data from cellh5 (hdf5) file
# 2013/12/07
# rudolf.hoefler@gmail.com

library(cellh5)

c5f <- CellH5("data/_all_positions.ch5")

chreg <- C5ChannelRegions(c5f)
primary <- chreg[[1]] # primary__primary
plates <- C5Plates(c5f)
positions <- C5Positions(c5f, plates[[1]])
tl = C5Timelapse(positions[[1]])
clfeatures <- C5FeatureNames(c5f, primary)

# mean intensity, standard deviation, size
main_features = c("n2_avg", "n2_stddev", "roisize")
# main_features <- clfeatures[c(1,2,3)]
frames_ <-  c(0, 1)#,1,2,3,4,5,6,7,8)

# read out data
object_counts <- C5ObjectCounts(c5f, positions[[1]], primary)
features <- C5FeaturesByName(c5f, positions[[1]], primary, main_features, frames=frames_)
object_ids <- C5ObjectLabels(positions[[1]], primary)
center <- C5Center(positions$W0_P0013, primary)
bbox <- C5BoundingBoxes(positions$W0_P0013, primary)
orientation <- C5Orientation(positions$W0_P0013, primary)
predictions <- C5Predictions(c5f, positions$W0_P0013, primary)
probs <- C5PredictionProbabilities(c5f, positions$W0_P0013, primary)
image_ <- C5ReadImage(c5f, positions$W0_P0013, primary, frame_index=1, zstack=1)
events <- C5Events(c5f, positions[[1]], primary, include_branches=TRUE, return_indices=TRUE)
event_features <- C5EventFeatures(c5f, positions[[1]], primary, TRUE, main_features)
# don't forget to release the resources
# C5Close(c5f)