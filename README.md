<img src="kwb_utils.png" alt="kwb.utils" />

[![R-CMD-check](https://github.com/KWB-R/kwb.utils/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/kwb.utils/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/kwb.utils/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/kwb.utils/actions?query=workflow%3Apkgdown) 
[![codecov](https://codecov.io/github/KWB-R/kwb.utils/branch/master/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.utils)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/kwb.utils)](http://cran.r-project.org/package=kwb.utils)
[![R-Universe_Status_Badge](https://kwb-r.r-universe.dev/badges/kwb.utils)](https://kwb-r.r-universe.dev/)


**Cite as:** [![DOI](https://zenodo.org/badge/23293/KWB-R/kwb.utils.svg)](https://zenodo.org/badge/latestdoi/23293/KWB-R/kwb.utils)

## Introduction

The `KWB.utils` package is designed to enhance the quality and safety of your R code by offering a set of utility functions. The primary objective 
is to streamline code development by minimizing duplication and promoting cleaner coding practices.

## Key Features

- **Code Cleanliness:** This package focuses on helping developers create cleaner code, which is characterized by limited duplication and increased readability.
- **General Utility Functions:** Currently there are 335 functions defined in this package 

## Installation

```r
# Enable repository from kwb-r
options(repos = c(
  kwbr = 'https://kwb-r.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
  
# Download and install kwb.utils in R
install.packages('kwb.utils')

# Browse the kwb.swmm manual pages
help(package = 'kwb.utils')
```

This will install the package in the following locations based on your operating system:
- **Linux:** /usr/local/lib/R/site-library
- **Windows:** C:/Users/{Your Username}/Documents/R/win-library/{R Version}

Once the installation is complete the package may be included as a library in any future R session, just be sure to include 
```r
#library(kwb.utils)
```

Additional details on how to install KWB-R packages can be found in our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).


## Usage

The functions included in this package have been organized into the following categories:

- [Arrays/Matrices](https://kwb-r.github.io/kwb.utils/articles/functions_array.html)
- [Calling](https://kwb-r.github.io/kwb.utils/articles/functions_call.html)
- [Check](https://kwb-r.github.io/kwb.utils/articles/functions_check.html)
- Comparing Objects
- [Conversion](https://kwb-r.github.io/kwb.utils/articles/functions_conversion.html)
- [Crytographic](https://kwb-r.github.io/kwb.utils/articles/functions_crypto.html)
- [Debug](https://kwb-r.github.io/kwb.utils/articles/functions_debug.html)
- [For Data Frames](https://kwb-r.github.io/kwb.utils/articles/functions_data_frame.html)
- [For Lists](https://kwb-r.github.io/kwb.utils/articles/functions_list.html)
- Functions Returning Functions
- [General Objects](https://kwb-r.github.io/kwb.utils/articles/functions_object.html)
- [Grammar/Dictionary](https://kwb-r.github.io/kwb.utils/articles/functions_dictionary.html)
- [Input and Output](https://kwb-r.github.io/kwb.utils/articles/functions_in_out.html)
- [Logical Returning](https://kwb-r.github.io/kwb.utils/articles/functions_logical.html)
- [Mathematical or Statistical](https://kwb-r.github.io/kwb.utils/articles/functions_stats.html)
- PDF Related
- [Operating Systems](https://kwb-r.github.io/kwb.utils/articles/functions_system.html)
- [Shortcuts to If Statements](https://kwb-r.github.io/kwb.utils/articles/functions_if.html)
- [String](https://kwb-r.github.io/kwb.utils/articles/functions_string.html)
- [Vectors](https://kwb-r.github.io/kwb.utils/articles/functions_vector.html)
- [Miscellaneous](https://kwb-r.github.io/kwb.utils/articles/functions_ungrouped.html)

*This list is not exhaustive of all the functions defined in the package. Additionally, the links to documentation are currently a work in progress.


## Documentation

Release: [https://kwb-r.github.io/kwb.utils](https://kwb-r.github.io/kwb.utils)

Development: [https://kwb-r.github.io/kwb.utils/dev](https://kwb-r.github.io/kwb.utils/dev)

## License

This project is licensed under the [MIT License](LICENSE.md).
