## C5


C5 is a small R-library that provides an S4-class for easy access of cellh5-files. It depends on the [bioconductor](http://www.bioconductor.org)-package, especially the the `rhdf5` package, which is part of bioconductor.


#### Installation
Open a terminal 
Clone the git-repository and change to the source directory
```
R CMD INSTALL --binary <path/to/source/directory/>
```

#### Installing from tar.gz file
First build the binary package (tar.gz)
```
R CMD BUILD -l <path/to/source/directory/>
```

Type in the `R`-terminal
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
