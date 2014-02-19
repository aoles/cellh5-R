## C5


C5 is a small R-library that provides an S4-class for easy access of cellh5-files. It depends on the [bioconductor](http://www.bioconductor.org)-package, especially the the `rhdf5` package, which is part of bioconductor. 


#### [rhdf5 2.7.4](http://www.bioconductor.org/packages/devel/bioc/html/rhdf5.html)
You need to install the development version of [rhdf5 2.7.4](http://www.bioconductor.org/packages/devel/bioc/html/rhdf5.html). Older versions do not read data correctly from file. Download the file for your platform and install it using the ```install.packages``` command (see below).


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
