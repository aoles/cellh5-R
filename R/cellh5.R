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
                             global_def="list",
                             positions="list"))

CellH5 <- function(file=NA) {
  fid <- H5Fopen(file)
  gdef <- h5read(fid, compoundAsDataFrame=FALSE, name='/definition')
  new("CellH5", filename=file, fid=H5Fopen(file), global_def=gdef, positions=list())
}

# XXX remove standardGenerics if possible
setGeneric("C5Close", function(ch5file) {
  H5Fclose(ch5file@fid)})

setGeneric("C5FileInfo", function(ch5file) {standardGeneric("C5FileInfo")})

setGeneric("C5Plates", function(ch5file) {standardGeneric("C5Plates")})

setGeneric("C5Positions", function(ch5file, plate, ...) {standardGeneric("C5Positions")})

setGeneric("C5ChannelRegions", function(ch5file) {standardGeneric("C5ChannelRegions")})

setGeneric("C5ObjectCounts", function(ch5file, position, channel_region, ...) {
  standardGeneric("C5ObjectCounts")})

setGeneric("C5FeaturesByName", function(ch5file, position, channel_region, 
                                        feature_names, frames=NULL, ...) {
  standardGeneric("C5FeaturesByName")})

setGeneric("C5ClassifierDefinition", function(ch5file, channel_region, ...) {
  standardGeneric("C5ClassifierDefinition")})

setGeneric("C5FeatureNames", function(ch5file, channel_region, ...) {
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

setGeneric("C5ObjectLabels", function(position, channel_region) {
  object_labels = h5read(position, name=sprintf('object/%s', channel_region))
  df <- data.frame(object_labels)
  colnames(df) <- c("frame_index", "object_label")
  df$frame_index <- cToRIndex(df$frame_index)
  return(df)
})

setGeneric("C5Predictions", function(ch5file, position, channel_region, ...) {
  standardGeneric("C5Predictions")})

setGeneric("C5PredictionProbabilities", function(ch5file, position, channel_region, ...) {
  standardGeneric("C5PredictionProbabilities")})

setGeneric("C5ReadImage", function(ch5file, position, channel_region, frame_index, zstack, 
                                   label_image=FALSE, ...) {
  standardGeneric("C5ReadImage")})

setGeneric("C5Events", function(ch5file, position, channel_region, 
                                include_branches=TRUE, return_indices=FALSE, ...) {
  standardGeneric("C5Events")
})

setGeneric("C5EventFeatures", function(ch5file, position, channel_region, 
                                include_branches=TRUE, feature_names=NULL, ...) {
  standardGeneric("C5EventFeatures")
})

setMethod("C5ObjectCounts", "CellH5", function(ch5file, position, channel_region) {
  classdef <- C5ClassifierDefinition(ch5file, channel_region)  
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
          function(ch5file, position, channel_region, feature_names, frames=NULL) {
    features = h5read(position, name=sprintf('feature/%s/object_features', channel_region))
    ftr_idx = match(feature_names, C5FeatureNames(ch5file, channel_region))
             
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
       
setMethod("C5FeatureNames", "CellH5", function(ch5file, channel_region) {
    return(ch5file@global_def$feature[[channel_region]]$object_features$name)
  })

setMethod("C5ClassifierDefinition", "CellH5", function(ch5file, channel_region) {
  return(ch5file@global_def$feature[[channel_region]]$object_classification$class_labels)
})
          
setMethod("C5ChannelRegions", "CellH5", function(ch5file) {
    return(names(ch5file@global_def[["feature"]]))
})

setMethod("C5Positions", "CellH5", function(ch5file, plate) {
  result <- list()
  plate_path <- sprintf("/sample/0/plate/%s/experiment/", plate)
  h5wells <- H5Gopen(h5loc=ch5file@fid, name=plate_path)
  wells <- h5ls(h5wells, recursive=F)$name
  H5Gclose(h5wells)
  
  for (wi in 1:length(wells)) {
    well = wells[wi]
    well_path <- sprintf("/sample/0/plate/%s/experiment/%s/position", 
                         plate, well)
    
    h5pos <- H5Gopen(h5loc=ch5file@fid, name=well_path)
    positions = h5ls(h5pos, recursive=F)$name
    H5Gclose(h5pos)
    
    for (pi in 1:length(positions)) {
      position = positions[pi]
      position_path <- sprintf("/sample/0/plate/%s/experiment/%s/position/%s", 
                               plate, well, position)
      group = H5Gopen(h5loc=ch5file@fid, name=position_path)
      result[[sprintf("W%s_P%s", well, position)]] = group
      }
    } 
  return(result)
  })

setMethod("C5Plates", "CellH5",
          function(ch5file) {
            group <- H5Gopen(h5loc=ch5file@fid, name="/sample/0/plate/")
            plates <-h5ls(group, recursive=F)$name
            H5Gclose(group)
            return(plates)
          })
          
setMethod("C5Timelapse", "CellH5",
          function(position) {
              return(h5read(position, name="image/time_lapse"))
          })

setMethod("C5FileInfo", "CellH5",
          function(ch5file) {
            print(paste("File: ", ch5file@filename))
            gdef <- H5Gopen(h5loc=ch5file@fid, name="/definition")
            list_ <- h5ls(gdef)
            H5Gclose(gdef)
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

setMethod("C5Predictions", "CellH5", function(ch5file, position, channel_region) {
  classdef <- C5ClassifierDefinition(ch5file, channel_region) 
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

setMethod("C5PredictionProbabilities", "CellH5", function(ch5file, position,
                                                          channel_region) {
  classdef <- C5ClassifierDefinition(ch5file, channel_region) 
  probs <- h5read(position,
                  name=sprintf("feature/%s/object_classification/probability",
                                channel_region))
  
  df = data.frame(t(probs))
  colnames(df) <- classdef$name
  return(df)         
  })

setMethod("C5ReadImage", "CellH5", function(ch5file, position, channel_region,
                                            frame_index, zstack=1, label_image=FALSE) {
  cdf = data.frame(ch5file@global_def$image$region)
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

setMethod("C5Events", "CellH5", function(ch5file, position, channel_region, 
                                         include_branches=TRUE, return_indices=FALSE) {
  events <- data.frame(h5read(position, name="object/event"))
  # class labels for certain channels
  predictions <- C5Predictions(ch5file, position, channel_region)
  track_ids <- unique(events$obj_id)
  
  if (!return_indices) {
    toPredictions <- function(i) {return(predictions[[i]])}  
  } else {
    toPredictions <- function(i) {return(i)}
  }
  
  tracks = list()
  for (i in 1:length(track_ids)) {
    idx <- which(events$obj_id == rToCIndex(i))
    pos_dub <- anyDuplicated(events$idx1[idx])
    if (pos_dub == 0) {
      track <- c(events$idx1[idx], tail(events$idx2[idx], n=1))
      tracks <- rbind(tracks, lapply(track, toPredictions))
    } else {
      # in case of a 2nd branch
      i1 <- anyDuplicated(events$idx1[idx]) - 1 # position of the duplicate
      i2 <- anyDuplicated(events$idx1[idx], fromLast=TRUE) # position of the "original"
      track <- c(events$idx1[idx][1], events$idx2[idx][1:i1])
      tracks <- rbind(tracks, lapply(track, toPredictions))
      if (include_branches) {
        track <- c(events$idx1[idx][1:i2], events$idx2[idx][(i1+1):length(idx)])
        tracks <- rbind(tracks, lapply(track, toPredictions))
      }
    } 
  }
  return(data.frame(tracks))
})

setMethod("C5EventFeatures", "CellH5", function(ch5file, position, channel_region, 
                                                include_branches=TRUE, feature_names=NULL) {
  
  features <- C5FeaturesByName(ch5file, position, channel_region, feature_names)       
  events <- C5Events(ch5file, position, channel_region, return_indices=TRUE)

  dims <- c(dim(events), dim(features)[2])
  efeatures = array(NA, dim=dims)
          
  for (i in 1:dims[1]){
    for (j in 1:dims[2]) {
      # it sucks, why is it not working!
      # efeatures[i,j, ] <- features[[events[[i, j]], ]]
      for (k in 1:dims[3]) {
        efeatures[i,j,k] <- features[[events[[i, j]], k ]]
      }
    }
  }
  return(efeatures)
})
