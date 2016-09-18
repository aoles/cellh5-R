cellh5-R
========

cellh5-R is a tiny R-package providing an S4-class for highlevel access to cellh5-files. It depends on the [bioconductor](http://www.bioconductor.org)-packages `rhdf5` package and `EBImage`. Another dependency is `base64enc` package.
#### Documentation
The package is fully documented within the R-manual pages.
```
help(cellh5)
```

#### Installing bioconductor
```
source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")
```


#### Installation
Open a terminal
Clone the git-repository and change to the source directory
```
R CMD INSTALL --binary <path/to/source/directory/>
```

#### Installing from tar.gz file
First build the binary package (tar.gz)
```
R CMD BUILD <path/to/source/directory/>
```

Type in the `R`-terminal (applies also to the rhdf-package)
```
intall.packages(<path/to/source/directory/cellh5_X.X.tar.gz, repos=NULL, type="source")
```

Once installed start `R` and type in the `R`-terminal
```
library(cellh5)
help(CellH5)
```
to get started.

**Note:**
Uppercase `"CellH5"` is the S4-class, lowercase `"cellh5"` the R-package.



#### Remove cellh5
```
remove.packages(cellh5)
```
