# cellh5.R - convenience function for easy access of cellh5 files
# rudolf.hoefler@gmail.com
# 2013/12/05

library('rhdf5')

cToRIndex <- function(list_) {
  return(list_ + 1)
}

ch5.PrintFileInfo<- function(file) {
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


ch5.GlobalDefinition <- function(file) {
  return(h5read(file, compoundAsDataFrame=FALSE, name='/definition'))
}


ch5.Plates <- function(file) {
  # return all list of plate names
  group = H5Gopen(h5loc=file, name="/sample/0/plate/")
  return(h5ls(group, recursive=F)$name)
}


ch5.Positions <- function(file, plate) {
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
      result[[sprintf("W%s_P%s", well, position)]] = group
    }
  } 
  return(result)
}


ch5.Timelapse <- function(position) {
  return(h5read(position, name="image/time_lapse"))
}


ch5.ChannelRegions <- function(global_def) {
  return(names(global_def[['feature']]))
}


ch5.ClassifierDefinition <- function(gdef, channel_region) {
    return(gdef$feature[[channel_region]]$object_classification$class_labels)
  }


ch5.FeatureNames <- function(gdef, channel_region) {
    return(gdef$feature[[channel_region]]$object_features$name)
  }

ch5.ObjectCounts <- function(position, global_def, channel_region) {
  
  classdef <- ch5.ClassifierDefinition(global_def, channel_region)  
  time_idx <- ch5.TimeIdx(position, channel_region)
  label_idx <- h5read(position,
    name=sprintf("feature/%s/object_classification/prediction", channel_region))$label_idx

  time_idx <- as.list(cToRIndex(time_idx))
  label_idx <- as.list(cToRIndex(label_idx))
  frames <- ch5.Timelapse(position)$frame
  object_counts = data.frame()
  
  for (i in 1:length(frames)) {
    frame = frames[[i]]
    row = list()
    for (j in 1:length(classdef$name)) {
      classname = classdef$name[[j]]
      object_counts[i, "frame"] = frame
      object_counts[i, classname] = sum((label_idx == j)*(time_idx == frame))
    }
  }
  return(object_counts)
}

ch5.FeaturesByName <- function(position, global_def,
                               channel_region, feature_names, frames=NULL) {
  features = h5read(pos, name='feature/primary__primary/object_features')
  
  ftr_idx = match(feature_names, ch5.FeatureNames(gdef, primary))
    
  if (is.null(frames)) {
    features <- features[ftr_idx, ] 
  } else {
    time_idx <- ch5.TimeIdx(position, channel_region)
    frame_idx <- which(time_idx %in% frames)
    features <- features[ftr_idx, frame_idx]
  }
  df <- data.frame(t(features))
  colnames(df) <- feature_names
  return(df)
}

ch5.TimeIdx <- function(position, channel_region) {
  return(h5read(position, name=sprintf("object/%s", channel_region))$time_idx)
}


