# cellh5.R - convenience function for easy access of cellh5 files
# rudolf.hoefler@gmail.com
# 2013/12/05


library('rhdf5')

ch5.print_file_info <- function(file) {
  # list recursively the definiton of a cellh5 file
  list_ <- h5ls(H5Gopen(h5loc=file, name="/definition"))
  idx = which((substr(list_$group, nchar(list_$group), 
                      nchar(list_$group)) == "/") & (nchar(list_$group) == 1))
  list_$group[idx] = ""
  list_$name = sprintf("%s/%s",list_$group,list_$name)
  list_$group = NULL
  list_$dclass[list_$otype == "H5I_GROUP"] = "group"
  list_$otype = NULL
  print(list_, right=FALSE)
}

ch5.definition <- function(file) {
  return(h5read(file, compoundAsDataFrame=FALSE, name='/definition'))
}

ch5.plates <- function(file) {
  # return all list of plate names
  group = H5Gopen(h5loc=file, name="/sample/0/plate/")
  return(h5ls(group, recursive=F)$name)
}

ch5.positions <- function(file, plate) {
  # return a list of position for a given file and plate  
  
  result <- list()
  plate_path <- sprintf("/sample/0/plate/%s/experiment/", plate)
  wells <- h5ls(H5Gopen(h5loc=file, name=plate_path), recursive=F)$name
  
  for (wi in 1:length(wells)) {
    well = wells[wi]
    well_path <- sprintf("/sample/0/plate/%s/experiment/%s/position", 
                         plate, well)
    positions = h5ls(H5Gopen(h5loc=file, name=well_path), recursive=F)$name
    
    for (pi in 1:length(positions)) {
      position = positions[pi]
      position_path <- sprintf("/sample/0/plate/%s/experiment/%s/position/%s", 
                               plate, well, position)
      group = H5Gopen(h5loc=file, name=position_path)
      print(position_path)
      result[[sprintf("W%s_P%s", well, position)]] = group
    }
  } 
  return(result)
}

ch5.timelapse <- function(position, unit='frame') {
  if (unit == "frame") {
    return(position$image$time_lapses$frame)
  } else if (unit=="timestamp_abs") {
    return(position$image$time_lapses$timestamp_abs)
  } else if (unit == "timestamp_rel") {
    return(position$image$time_lapses$timestamp_rel)
  }
}

# 
# # open file (usually _all_positions.ch5)
# file = H5Fopen("C:/Users/sommerc/R/cellh5/0038.hdf5")
# 
# # get plate name
# plate = ch5_get_plate_names(file)[1]
# 
# # get all positions of that plate
# position_list = ch5_get_positions(file, plate)
# 
# # get for example the first position
# pos = position_list[[1]]
# 
# # count objects for time index 0
# time_index = 0
# time_indecies = h5read(pos, name='object/primary__primary')$time_idx == time_index
# object_count = length(which(time_indecies))
# 
# # extract all features for the primary segmentation
# features = h5read(pos, name='feature/primary__primary/object_features')
# 
# # get feature description to choose the feature index of e.g. n2_avg
# global_def = h5read(file, name="/definition")
# 
# # print all computed feature names
# feature_names = global_def$feature$primary__primary$object_classification$features
# #print(feature_names)
# 
# # find index of n2_avg
# feature_index = which(feature_names == "n2_avg")
# 
# # extract feature for time index 0
# 
# feature_Values = features[feature_index, time_indecies]
# 
# 
# #H5Gclose(pos)
# #H5Fclose(file)
# 
# 



