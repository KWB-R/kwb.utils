---
title: ''
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


[![Appveyor build status](https://ci.appveyor.com/api/projects/status/t9w8xyoyyg7tbk2y/branch/master?svg=true)](https://ci.appveyor.com/project/KWB-R/kwb-utils/branch/master)
[![Build Status](https://travis-ci.org/KWB-R/kwb.utils.svg?branch=master)](https://travis-ci.org/KWB-R/kwb.utils)
[![codecov](https://codecov.io/github/KWB-R/kwb.utils/branch/master/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.utils)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/kwb.utils)](http://cran.r-project.org/package=kwb.utils)
[![DOI](https://zenodo.org/badge/23293/KWB-R/kwb.utils.svg)](https://zenodo.org/badge/latestdoi/23293/KWB-R/kwb.utils)


## Installation

For details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).


```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

# Install KWB package 'kwb.utils' from GitHub

remotes::install_github("kwb-r/kwb.utils")
```