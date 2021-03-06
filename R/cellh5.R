# S4 class with some convenience function to read data from cellh5 (hdf5) file
# 2013/12/07
# rudolf.hoefler@gmail.com

library('rhdf5', verbose=FALSE)
library('grid', verbose=FALSE)
library('base64enc', verbose=FALSE)


c5read <- function(position, name, index=NULL,
                   start=NULL, stride=NULL, block=NULL, count=NULL,
                   compoundAsDataFrame = TRUE, callGeneric = TRUE,
                   read.attributes = TRUE) {
  
  if (H5Lexists(position, name)){
    ret <- h5read(position, name, index=index,
                  start=start, stride=stride, block=block, count=count,
                  compoundAsDataFrame=compoundAsDataFrame, callGeneric=callGeneric,
                  read.attributes=read.attributes)
  } else {
    show(position)
    stop(sprintf("No subgroup %s in HDF5 group", name))
  }
  return(ret)
}

cToRIndex <- function(list_) {
  # Cellh5 is index based, R used fortran index convention --> it sucks
  return(list_ + 1)
}

rToCIndex <- function(list_) {
  return(list_ - 1)
}

# helper for grid.raster
toRaster = function(x, cuts=-1:255+0.5, 
                    colors = colorRampPalette(c("black","white"))(256)) {
  cux =cut(x, cuts, include.lowest = TRUE, labels=FALSE)
  rv = x
  rv[] = colors[cux]
  return(t(rv))
}

wellName = function(h5group) {
  # return a string that identifies the well and site (position)
  name <- H5Iget_name(h5group)
  site <- basename(name)
  well <- basename(dirname(dirname(name)))
  return(sprintf("W%s__S%s", well, site))
}


CellH5 <- setClass("CellH5", 
                   slots = c(filename="character", 
                             fid="H5IdComponent",
                             global_def="list",
                             positions="ANY"),
                   # prototype=list(positions = list())
                   )

CellH5 <- function(file=NA) {
  if (file.exists(file)) {
    fid = H5Fopen(file)
  } else {
    stop(sprintf("file %s does not exist!", file))
  }
  gdef <- c5read(fid, compoundAsDataFrame=FALSE, name='/definition')
  new("CellH5", filename=file, fid=H5Fopen(file), global_def=gdef)
}

setGeneric("C5HasClassifiedObjects", function(position, mask) {
  if (H5Lexists(position, name=sprintf("feature/%s/object_classification", mask))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
})
  
setGeneric("C5HasObjects", function(position, mask) {
  # unfortunately there is no meta data how many ojbects are found 
  # use the fk time index
  ti <- c5read(position, name=sprintf("object/%s", mask))$time_idx
  if (length(ti) == 0) {
    return(FALSE)
  } else { 
    return(TRUE)}
})

setGeneric("C5HasEvents", function(position) {
  oid <- c5read(position, name="object/event")$obj_id
  if (length(oid) == 0) {
    return(FALSE)
  } else { 
    return(TRUE)}
})

setGeneric("C5HasTimelapse", function(position) {
  if (H5Lexists(position, name="image/time_lapse")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
})

# XXX remove standardGenerics if possible
setGeneric("C5Close", function(ch5file, positions=NULL, ...) {
  standardGeneric("C5Close")
  })

setGeneric("C5FileInfo", function(ch5file) {standardGeneric("C5FileInfo")})

setGeneric("C5Plates", function(ch5file) {standardGeneric("C5Plates")})

setGeneric("C5Positions", function(ch5file, plate, ...) {standardGeneric("C5Positions")})

setGeneric("C5SegementationMasks", function(ch5file) {standardGeneric("C5SegementationMasks")})

setGeneric("C5ObjectCounts", function(ch5file, position, mask, ...) {
  standardGeneric("C5ObjectCounts")})

setGeneric("C5FeaturesByName", function(ch5file, position, mask, 
                                        feature_names, frames=NULL, ...) {
  standardGeneric("C5FeaturesByName")})

setGeneric("C5ClassifierDefinition", function(ch5file, mask, ...) {
  standardGeneric("C5ClassifierDefinition")})

setGeneric("C5FeatureNames", function(ch5file, mask, ...) {
  standardGeneric("C5FeatureNames")})

setGeneric("C5Timelapse", function(position) {
  if (C5HasTimelapse(position)) {
    return(c5read(position, name="image/time_lapse"))
  } else {
    return(NULL)
  }
})

setGeneric("C5FrameNumbers", function(position, mask) {
  tidx <- c5read(position, name=sprintf("object/%s", mask))$time_idx
  if (length(tidx) == 0) {
    return(NULL)
  } else {
    return(cToRIndex(tidx))
  }
})

setGeneric("C5Orientation", function(position, mask) {
  if (!C5HasObjects(position, mask)) {
    return(NULL)
  } 
  orientation = c5read(position, name=sprintf('feature/%s/orientation', mask))
  df <- data.frame(orientation)
  return(df)
})

setGeneric("C5BoundingBoxes", function(position, mask) {
  if (!C5HasObjects(position, mask)) {
    return(NULL)
  } 
  center = c5read(position, name=sprintf('feature/%s/bounding_box', mask))
  df <- data.frame(center)
  return(df)
})

setGeneric("C5Center", function(position, mask) {
  if (!C5HasObjects(position, mask)) {
    return(NULL)
  } 
  center = c5read(position, name=sprintf('feature/%s/center', mask))
  df <- data.frame(center)
  return(df)
})

setGeneric("C5ObjectLabels", function(position, mask) {
  if (!C5HasObjects(position, mask)) {
    return(NULL)
  } 
  
  object_labels = c5read(position, name=sprintf('object/%s', mask))
  df <- data.frame(object_labels)
  colnames(df) <- c("frame_index", "object_label")
  df$frame_index <- cToRIndex(df$frame_index)
  return(df)
})

setGeneric("C5Predictions", function(ch5file, position, mask, as_=FALSE, ...) {
  standardGeneric("C5Predictions")})

setGeneric("C5PredictionProbabilities", function(ch5file, position, mask, ...) {
  standardGeneric("C5PredictionProbabilities")})

setGeneric("C5ReadImage", function(ch5file, position, mask, frame_index, zstack, 
                                   label_image=FALSE, ...) {
  standardGeneric("C5ReadImage")})

setGeneric("C5Events", function(ch5file, position, mask, 
                                include_branches=TRUE, return_indices=FALSE, ...) {
  standardGeneric("C5Events")
})

setGeneric("C5EventFeatures", function(ch5file, position, mask, 
                                include_branches=TRUE, feature_names=NULL, ...) {
  standardGeneric("C5EventFeatures")
})
           
setGeneric("C5ObjectDetails", function(ch5file, position, mask) {
  standardGeneric("C5ObjectDetails")
})

setGeneric("C5Contours", function(ch5file, position, mask, frame=NULL, ...) {
 standardGeneric("C5Contours") 
})

setGeneric("C5ContourImage", function(ch5file, position, mask, frame, 
                                      zstack=1, filename=NULL, default_color="#ffffff",
                                      fontsize=12, show_labels=TRUE, background_region=NULL, ...) {
  standardGeneric("C5ContourImage")
})

setGeneric("C5ExportGallery", function(ch5file, outdir, position, mask, 
                                      include_branches=FALSE, gallery_size=60, zstack=1, ...) {
  standardGeneric("C5ExportGallery")
})

setGeneric("C5GalleryImageByIndex", function(ch5file, position, mask, index,
                                             gallery_size=60, ...) {
  standardGeneric("C5GalleryImageByIndex")
})

setGeneric("C5ClosePositions", function(positions) {
  # rhdf interface sucks, R sucks!
  for (pos in positions) {
   H5Gclose(pos)
  }
})

setMethod("C5Close", "CellH5", function(ch5file, positions=NULL, ...) {
  H5Fclose(ch5file@fid)
  
  if (!is.null(positions)) {
    C5ClosePositions(positions)
  }
})

setMethod("C5ObjectDetails", "CellH5", function(ch5file, position, mask) {
  if (!C5HasObjects(position, mask)) {
    return(NULL)
  }
  
  tidx <- c5read(position, name=sprintf("object/%s", mask))$time_idx
  objid <- c5read(position, name=sprintf("object/%s", mask))$obj_label_id
  timelapse <- C5Timelapse(position)
  features <- C5FeaturesByName(ch5file, position, mask, 
                               c("n2_avg", "n2_stddev", "roisize"))

  # first case -> single timepoint
  # second case -> timelapse
  if (length(unique(tidx)) == 1) {
    frames <- array(unique(tidx))
  } else {
    frames = array(0, length(tidx))
    for (i in 1:length(timelapse$frame)) {
      frames[which(tidx == rToCIndex(i))] <- timelapse$frame[[i]]
    }
  }

  # use empty arrays if no classifier is given
  if (C5HasClassifiedObjects(position, mask)){
    class_names <- C5Predictions(ch5file, position, mask, as_="name")
    class_labels <- C5Predictions(ch5file, position, mask, as_="label")
  } else {
    class_names <- array(NA, length(frames))
    class_labels <- array(NA, length(frames))
  }
  
  df <- data.frame(cbind(frames, objid, class_names, class_labels, features))
  colnames(df) <- c("frame", "ojb_id", "class_name", "class_label",
                   "mean", "stddev", "size")
  
  return(df)
})

setMethod("C5ObjectCounts", "CellH5", function(ch5file, position, mask) {
  if (!(C5HasObjects(position, mask) & C5HasClassifiedObjects(position, mask))) {
    return(NULL)
  }
  
  classdef <- C5ClassifierDefinition(ch5file, mask)  
  time_idx <- C5FrameNumbers(position, mask)
  label_idx <- c5read(position,
                      name=sprintf("feature/%s/object_classification/prediction",
                                   mask))$label_idx
  
  label_idx <- as.list(cToRIndex(label_idx))
  
  # first case -> single timepoint
  # second case -> timelapse
  if (length(unique(time_idx)) == 1) {
    frames <- array(unique(time_idx))
  } else {
    frames <- C5Timelapse(position)$frame
  }

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
          function(ch5file, position, mask, feature_names) {  
    if (!C5HasObjects(position, mask)) {
      return(NULL)
    } 
    
    features = c5read(position, name=sprintf('feature/%s/object_features', mask))
    ftr_idx = match(feature_names, C5FeatureNames(ch5file, mask))
    features <- features[ftr_idx, ]
         
    # R returns different data types  
    if (length(feature_names) == 1) {
      df <- data.frame(features)
    } else {
      df <- data.frame(t(features))
    }
    colnames(df) <- feature_names
    return(df)
  })

setMethod("C5FeatureNames", "CellH5", function(ch5file, mask) {
    return(ch5file@global_def$feature[[mask]]$object_features$name)
  })

setMethod("C5ClassifierDefinition", "CellH5", function(ch5file, mask) {
  return(ch5file@global_def$feature[[mask]]$object_classification$class_labels)
})
          
setMethod("C5SegementationMasks", "CellH5", function(ch5file) {
    return(as.list(names(ch5file@global_def[["feature"]])))
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

setMethod("C5Predictions", "CellH5", function(ch5file, position, mask, 
                                              as_="label") {
  if (!(C5HasObjects(position, mask) & C5HasClassifiedObjects(position, mask))) {
    return(NULL)
  }  

  classdef <- C5ClassifierDefinition(ch5file, mask) 
  label_idx <- c5read(position,
                      name=sprintf("feature/%s/object_classification/prediction",
                                   mask))$label_idx
  

  labels_ <- array()
  for (i in 1:length(classdef$label)) {
    if (as_ == "label") {
      labels_[which(label_idx == rToCIndex(i))] <- classdef$label[[i]]
    } else if (as_ == "color") {
      labels_[which(label_idx == rToCIndex(i))] <- classdef$color[[i]]
    } else if (as_ == "name") {
      labels_[which(label_idx == rToCIndex(i))] <- classdef$name[[i]]
    } else {
      stop(paste("Invalid argument", as_))
    }
  }

  return(labels_)         
})

setMethod("C5PredictionProbabilities", "CellH5", 
          function(ch5file, position, mask) {
  if (!(C5HasObjects(position, mask) & C5HasClassifiedObjects(position, mask))) {
    return(NULL)
  }
  
  classdef <- C5ClassifierDefinition(ch5file, mask) 
  probs <- c5read(position,
                  name=sprintf("feature/%s/object_classification/probability",
                                mask))
  
  df = data.frame(t(probs))
  colnames(df) <- classdef$name
  return(df)         
  })

setMethod("C5ReadImage", "CellH5", function(ch5file, position, mask,
                                            frame_index, zstack=1, label_image=FALSE) {
  
  cdf = data.frame(ch5file@global_def$image$region)
  color_index <- cToRIndex(cdf$channel_idx[which(cdf$region_name == sprintf("region___%s", mask))])
  
  if (label_image) {
    itype <- "image/region"
  } else {
    itype <- "image/channel"
  }

  image_ <- c5read(position, name=itype,
                 index=list(NULL, NULL, zstack, frame_index, color_index))[, , 1,1,1]
  return(image_)
})

setMethod("C5Events", "CellH5", function(ch5file, position, mask, 
                                         include_branches=TRUE, return_indices=FALSE) {
  events <- data.frame(c5read(position, name="object/event"))
  if (length(events$obj_id) == 0) {
    return(NULL)
  }
  
  # class labels for certain channels
  predictions <- C5Predictions(ch5file, position, mask)
  track_ids <- unique(events$obj_id)
  
  if (!return_indices) {
    toPredictions <- function(i) {return(predictions[[cToRIndex(i)]])}  
  } else {
    toPredictions <- function(i) {return(cToRIndex(i))}
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

setMethod("C5EventFeatures", "CellH5", function(ch5file, position, mask, 
                                                include_branches=TRUE, feature_names=NULL) {
  if (!C5HasEvents(position)) {
    return(NULL)
  }
  
  features <- C5FeaturesByName(ch5file, position, mask, feature_names)
  events <- C5Events(ch5file, position, mask, return_indices=TRUE)

  dims <- c(dim(events), dim(features)[2])
  efeatures = array(NA, dim=dims)
          
  for (i in 1:dims[1]){
    for (j in 1:dims[2]) {
      # it sucks, why is it not working!
      # efeatures[i,j, ] <- features[[events[[i, j]], ]]
      for (k in 1:dims[3]) {
        efeatures[i, j, k] <- features[[events[[i, j]], k ]]
      }
    }
  }
  return(efeatures)
})

setMethod("C5Contours", "CellH5", function(ch5file, position, mask, frame=NULL){
  raw <- c5read(position, name=sprintf('feature/%s/crack_contour', mask))
  
  # saves a lot of computing time due to the decoding of the string
  if (!is.null(frame)) {
    time_idx <- C5FrameNumbers(position, mask)
    frame_idx <- which(time_idx == frame)
    raw <- raw[frame_idx]
  }
  
  contours = list()
  for (i in 1:length(raw)) {
    cnt = strsplit(rawToChar(memDecompress(base64decode(raw[[i]]), type="g")), split=",")[[1]] 
    npoints <- length(cnt)/2
    polygon <- array(NA, dim=c(npoints, 2))
  
    for (j in 1:npoints) {
       polygon[j, 1] <- as.numeric(cnt[[j*2-1]])
       polygon[j, 2] <- as.numeric(cnt[[j*2]])
    }
    contours[[i]] <- polygon
  }
  return(contours)
})