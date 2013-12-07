

source('cellh5.R')
library("grid")

file_ = H5Fopen('data/_all_positions.ch5')
# ch5.PrintFileInfo(file_)

# INFORMATION FROM THE HEADER
gdef = ch5.GlobalDefinition(file_)
chreg <- ch5.ChannelRegions(gdef)
primary <- chreg[[1]] # primary__primary
classdef = ch5.ClassifierDefinition(gdef, primary)
clfeatures = ch5.FeatureNames(gdef, primary)

main_features = c("n2_avg", "n2_stddev", "roisize")
main_features = clfeatures[c(1,2,3)]

plates <- ch5.Plates(file_)
for ( i in 1:length(plates)) {
    print(paste("loading plate: ", plates[i]))
    positions <- ch5.Positions(file_, plates[i])
}


pos = positions[[1]] # 0013
# tl <- ch5.Timelapse(positions[[1]])
object_coutns <- ch5.ObjectCounts(positions[[1]], gdef, primary)
features <- ch5.FeaturesByName(positions[[1]], gdef, primary,
                               main_features, frames=NULL)

print(paste("Number of plates: ", length(positions)))

H5Fclose(file_)