# S4 class with some convenience function to read data from cellh5 (hdf5) file
# 2013/12/07
# rudolf.hoefler@gmail.com

library('rhdf5')

cToRIndex <- function(list_) {
  # Cellh5 is index based, R used fortran index convention --> it sucks
  return(list_ + 1)
}

rToCIndex <- function(list_) {
  return(list_ - 1)
}

CellH5 <- setClass("CellH5", 
                   slots = c(filename="character", 
                             fid="H5IdComponent",
                             global_def="list"))

CellH5 <- function(file=NA) {
  fid <- H5Fopen(file)
  gdef <- h5read(fid, compoundAsDataFrame=FALSE, name='/definition')
  new("CellH5", filename=file, fid=H5Fopen(file), global_def=gdef)
}

# XXX remove standardGenerics if possible
setGeneric("C5Close", function(object) {H5Fclose(object@fid)})

setGeneric("C5FileInfo", function(object) {standardGeneric("C5FileInfo")})

setGeneric("C5Plates", function(object) {standardGeneric("C5Plates")})

setGeneric("C5Positions", function(object, plate, ...) {standardGeneric("C5Positions")})

setGeneric("C5ChannelRegions", function(object) {standardGeneric("C5ChannelRegions")})

setGeneric("C5ObjectCounts", function(object, position, channel_region, ...) {
  standardGeneric("C5ObjectCounts")})

setGeneric("C5FeaturesByName", function(object, position, channel_region, 
                                        feature_names, frames=NULL, ...) {
  standardGeneric("C5FeaturesByName")})

setGeneric("C5ClassifierDefinition", function(object, channel_region, ...) {
  standardGeneric("C5ClassifierDefinition")})

setGeneric("C5FeatureNames", function(object, channel_region, ...) {
  standardGeneric("C5FeatureNames")})

setGeneric("C5Timelapse", function(position) {
  return(h5read(position, name="image/time_lapse"))
})

setGeneric("C5TimeIdx", function(position, channel_region) {
  return(h5read(position, name=sprintf("object/%s", channel_region))$time_idx)
})

setGeneric("C5Orientation", function(position, channel_region, frames=NULL) {
  center = h5read(position, name=sprintf('feature/%s/orientation', channel_region))
  
  if (is.null(frames)) {
    df <- data.frame(center)
  } else { 
    time_idx <- C5TimeIdx(position, channel_region)   
    frame_idx <- which(time_idx %in% frames)
    df <- data.frame(center[frame_idx, ])
  }
  return(df)
})

setGeneric("C5BoundingBoxes", function(position, channel_region, frames=NULL) {
  center = h5read(position, name=sprintf('feature/%s/bounding_box', channel_region))
  
  if (is.null(frames)) {
    df <- data.frame(center)
  } else { 
    time_idx <- C5TimeIdx(position, channel_region)   
    frame_idx <- which(time_idx %in% frames)
    df <- data.frame(center[frame_idx, ])
  }
  return(df)
})

setGeneric("C5Center", function(position, channel_region, frames=NULL) {
    center = h5read(position, name=sprintf('feature/%s/center', channel_region))
            
    if (is.null(frames)) {
      df <- data.frame(center)
    } else { 
      time_idx <- C5TimeIdx(position, channel_region)   
      frame_idx <- which(time_idx %in% frames)
      df <- data.frame(center[frame_idx, ])
    }
    return(df)
})

setGeneric("C5ObjectLabels", function(position, channel_region, frames=NULL) {
  object_labels = h5read(position, name=sprintf('object/%s', channel_region))
  
  if (is.null(frames)) {
    df <- data.frame(object_labels)
  } else { 
    time_idx <- C5TimeIdx(position, channel_region)   
    frame_idx <- which(time_idx %in% frames)
    df <- data.frame(object_labels[frame_idx, ])
  }
  
  colnames(df) <- c("frame_index", "object_label")
  df$frame_index <- cToRIndex(df$frame_index)
  return(df)
})

setGeneric("C5Predictions", function(object, position, channel_region, ...) {
  standardGeneric("C5Predictions")})

setGeneric("C5PredictionProbabilities", function(object, position, channel_region, ...) {
  standardGeneric("C5PredictionProbabilities")})

setGeneric("C5ReadImage", function(object, position, channel_region, frame_index, zstack, 
                                   label_image=FALSE, ...) {
  standardGeneric("C5ReadImage")})

setMethod("C5ObjectCounts", "CellH5", function(object, position, channel_region) {
  classdef <- C5ClassifierDefinition(object, channel_region)  
  time_idx <- C5TimeIdx(position, channel_region)
  label_idx <- h5read(position,
                      name=sprintf("feature/%s/object_classification/prediction",
                                   channel_region))$label_idx
  
  time_idx <- as.list(cToRIndex(time_idx))
  label_idx <- as.list(cToRIndex(label_idx))
  frames <- C5Timelapse(position)$frame
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
})

setMethod("C5FeaturesByName", "CellH5", 
          function(object, position, channel_region, feature_names, frames=NULL) {
    features = h5read(position, name=sprintf('feature/%s/object_features', channel_region))
    ftr_idx = match(feature_names, C5FeatureNames(object, channel_region))
             
    if (is.null(frames)) {
      features <- features[ftr_idx, ] 
    } else {
      time_idx <- C5TimeIdx(position, channel_region)
      frame_idx <- which(time_idx %in% frames)
      features <- features[ftr_idx, frame_idx]
    }
    df <- data.frame(t(features))
    colnames(df) <- feature_names
    return(df)
  })
       
setMethod("C5FeatureNames", "CellH5", function(object, channel_region) {
    return(object@global_def$feature[[channel_region]]$object_features$name)
  })

setMethod("C5ClassifierDefinition", "CellH5", function(object, channel_region) {
  return(object@global_def$feature[[channel_region]]$object_classification$class_labels)
})
          
setMethod("C5ChannelRegions", "CellH5", function(object) {
    return(names(object@global_def[["feature"]]))
})

setMethod("C5Positions", "CellH5", function(object, plate) {
  result <- list()
  plate_path <- sprintf("/sample/0/plate/%s/experiment/", plate)
  wells <- h5ls(H5Gopen(h5loc=object@fid, name=plate_path), recursive=F)$name
  
  for (wi in 1:length(wells)) {
    well = wells[wi]
    well_path <- sprintf("/sample/0/plate/%s/experiment/%s/position", 
                         plate, well)
    positions = h5ls(H5Gopen(h5loc=object@fid, name=well_path), recursive=F)$name
    
    for (pi in 1:length(positions)) {
      position = positions[pi]
      position_path <- sprintf("/sample/0/plate/%s/experiment/%s/position/%s", 
                               plate, well, position)
      group = H5Gopen(h5loc=object@fid, name=position_path)
      result[[sprintf("W%s_P%s", well, position)]] = group
      }
    } 
  return(result)
  })

setMethod("C5Plates", "CellH5",
          function(object) {
            group = H5Gopen(h5loc=object@fid, name="/sample/0/plate/")
            return(h5ls(group, recursive=F)$name)
          })
          
setMethod("C5Timelapse", "CellH5",
          function(position) {
              return(h5read(position, name="image/time_lapse"))
          })

setMethod("C5FileInfo", "CellH5",
          function(object) {
            print(paste("File: ", object@filename))
            list_ <- h5ls(H5Gopen(h5loc=object@fid, name="/definition"))
            idx = which((substr(list_$group, nchar(list_$group), 
                                nchar(list_$group)) == "/") & (nchar(list_$group) == 1))
            list_$group[idx] = ""
            list_$name = sprintf("%s/%s",list_$group,list_$name)
            list_$group = NULL
            list_$dclass[list_$otype == "H5I_GROUP"] = "group"
            list_$otype = NULL
            print(list_, right=FALSE)
          }
        )

setMethod("C5Predictions", "CellH5", function(object, position, channel_region) {
  classdef <- C5ClassifierDefinition(object, channel_region) 
  label_idx <- h5read(position,
                      name=sprintf("feature/%s/object_classification/prediction",
                                   channel_region))$label_idx
  

  labels_ <- list()
  # print(label_idx)
  for (i in 1:length(classdef$label)) {
    labels_[which(label_idx == rToCIndex(i))] <- classdef$name[[i]]
  }
  return(labels_)         
})

setMethod("C5PredictionProbabilities", "CellH5", function(object, position,
                                                          channel_region) {
  classdef <- C5ClassifierDefinition(object, channel_region) 
  probs <- h5read(position,
                  name=sprintf("feature/%s/object_classification/probability",
                                channel_region))
  
  df = data.frame(t(probs))
  colnames(df) <- classdef$name
  return(df)         
  })

setMethod("C5ReadImage", "CellH5", function(object, position, channel_region,
                                            frame_index, zstack=1, label_image=FALSE) {
  cdf = data.frame(object@global_def$image$region)
  color_index <- cToRIndex(cdf$channel_idx[which(cdf$region_name == sprintf("region___%s", channel_region))])
  
  if (label_image) {
    itype <- "image/region"
  } else {
    itype <- "image/channel"
  }

  image_ <- h5read(position, name=itype,
                 index=list(NULL, NULL, zstack, frame_index, color_index))[, , 1,1,1]
  
  return(image_)
})
