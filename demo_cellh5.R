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
timelapse <- C5Timelapse(position)$frame
# mean intensity, standard deviation, size
main_features = c("n2_avg", "n2_stddev", "roisize")
# main_features <- clfeatures[c(1,2,3)]

# read out data, we need to check if there objects/cells found. The same applies
# to the methods that read the event data. The checks are not mandatory since
# the function return NULL if the file contains no data. For sake of readability
# is recommended to use the functions C5HasObjecs/C5HasEvents

if (C5HasObjects(positions[[1]], primary)) {
  object_counts <- C5ObjectCounts(c5f, positions[[1]], primary)
  features <- C5FeaturesByName(c5f, positions[[1]], primary, main_features)
  object_ids <- C5ObjectLabels(positions[[1]], primary)
  center <- C5Center(positions$W0_P0013, primary)
  bbox <- C5BoundingBoxes(positions$W0_P0013, primary)
  orientation <- C5Orientation(positions$W0_P0013, primary)
  predictions <- C5Predictions(c5f, positions$W0_P0013, primary)
  probs <- C5PredictionProbabilities(c5f, positions$W0_P0013, primary)
}

if (C5HasEvents(positions[[1]])) {
  events <- C5Events(c5f, positions[[1]], primary, include_branches=TRUE, return_indices=TRUE)
  event_features <- C5EventFeatures(c5f, positions[[1]], primary, TRUE, main_features)
}

image_ <- C5ReadImage(c5f, positions$W0_P0013, primary, frame_index=1, zstack=1)

# don't forget to release the resources
# C5Close(c5f)