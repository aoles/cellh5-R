# S4 class with some convenience function to read data from cellh5 (hdf5) file
# 2013/12/07
# rudolf.hoefler@gmail.com
library(cellh5)


c5f <- CellH5("data/_all_positions.ch5")
masks <- C5SegementationMasks(c5f)
primary <- masks[[1]] # primary__primary
plates <- C5Plates(c5f)
positions <- C5Positions(c5f, plates[[1]])
tl = C5Timelapse(positions[[1]])
clfeatures <- C5FeatureNames(c5f, primary)
classdef <- C5ClassifierDefinition(c5f, primary)

# mean intensity, standard deviation, size
main_features = c("n2_avg", "n2_stddev", "roisize")
main_features <- clfeatures[c(1,2,3)]

pos <- positions$W0_P0037

# read out data, we need to check if there objects/cells found. The same applies
# to the methods that read the event data. The checks are not mandatory since
# the function return NULL if the file contains no data. For sake of readability
# is recommended to use the functions C5HasObjecs/C5HasEvents

if (C5HasObjects(pos, primary)) {
  object_counts <- C5ObjectCounts(c5f, pos, primary)
  features <- C5FeaturesByName(c5f, pos, primary, main_features)
  object_ids <- C5ObjectLabels(pos, primary)
  center <- C5Center(pos, primary)
  bbox <- C5BoundingBoxes(pos, primary)
  orientation <- C5Orientation(pos, primary)
  predictions <- C5Predictions(c5f, pos, primary, as="label")
  probs <- C5PredictionProbabilities(c5f, pos, primary)
  details <- C5ObjectDetails(c5f, pos, primary)
  contours <- C5Contours(c5f, pos, primary, frame=1)
} else {
  warning("no objects found!")
}

if (C5HasEvents(pos)) {
  events <- C5Events(c5f, pos, primary, include_branches=TRUE, return_indices=F)
  event_features <- C5EventFeatures(c5f, pos, primary, TRUE, main_features)
} else {
  warning('no events found!')
}

# demonstrate low level access
print(head(c5read(pos, "feature/primary__primary/center")))

# don't forget to release the resources
# C5ClosePositions(positions)
C5Close(c5f, positions)