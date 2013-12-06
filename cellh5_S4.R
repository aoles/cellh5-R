library('rhdf5')


CellH5 <- setClass("CellH5", 
                   slots = c(filename="character", fid="H5IdComponent"))

CellH5 <- function(file=NA) {
  new("CellH5", filename=file, fid=H5Fopen(file))
}


c5f <- CellH5(file="data/0013.ch5")