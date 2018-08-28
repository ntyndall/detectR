<img align="right" width="125" height="150" src="https://raw.githubusercontent.com/ntyndall/detectR/master/images/sticker.png">

# detectR
[![Build Status](https://travis-ci.org/ntyndall/detectR.svg?branch=master)](https://travis-ci.org/ntyndall/detectR)
[![codecov](https://codecov.io/gh/ntyndall/detectR/branch/master/graph/badge.svg)](https://codecov.io/gh/ntyndall/detectR)

This package is inspired by the likes of `libinject (https://github.com/client9/libinjection)`, and provides an easy to use interface for analysing and classifying strings of specific content built on the `neuralnet` package. A lot of the data used for building these models has been taken from various sources, including `libinject`, to build features and therefore construct training and test sets. This package comes pre-built with a detection mechanism, where the positive class of `"N"` represents the normal data source, and the following classes have been selected to try and identify other string sources
  - `S` : SQL injection
  - `X` : Cross-site scripting
  - `B` : Bash/Shell injection

You may need to extend or enhance these further, and this package provides a useful way to construct and utilise neural networks by controlling the input which will be explained later in this readme.

# Installation
To get started, install the `detectR` package;
```r
# Run this as a script
library(devtools)

devtools::install_github(
  repo = "ntyndall/detectR"
)

library(detectR)
```

# Guide
### Usage
This package is ready to go, with models already pre-built. To get started, supply input arguments as follows
```r
detectR::detectR(x = "this is a test") 
# "N"
detectR::detectR(x = list("input 1", "input 2")) 
# c("N", "N")
detectR::detectR(x = "SELECT * FROM Users WHERE UserId = 105 OR 1=1;") 
# "S" 
```


### Building your own model
It is possible to play around with the input and load your own data sets, provided the data sets are in the appropriate format. To do so, you can write a small script and pass the data frames like so,
```r
# Data source 1
dataX <- data.frame(
  argument = c("..."), 
  label = c("...")
)

# Data source 2
dataY <- data.frame(
  argument = c("..."), 
  label = c("...")
)

results <- detectR::builder(dataX, dataY, detectR::d.sqli)
```
Assuming that the data sets have an argument and label column in that order, you can incorporate custom, prepared data. You can bulk up the data like in the example above by using one of the four pre-built data sets accessible via `detectR::d.normal` / `detectR::d.sqli` / `detectR::d.xss` / `detectR::d.bash`.

The `results` list object contains
  - `nn` : The Nueral Network object created.
  - `dataScales` : The scaling parameters after normalisation. 

To use your new neural network during the investigation, you can supply them to `detectR::detectR` with the named input argument of `new.model`, such as;
```r
detectR::detectR(x = "this is a test", new.model = results)
```
With this method, you can define your own characteristics and even your own classes, or have a more general approach with only two classes of "P" (for normal data), and "A" (for abnormal looking data).

### Other input
Here is a list of the important input keyword arguments supplied to `detectR::builder`;

  - `posClass (="N")` : The positive class label of the full data set.
  - `saveData (=FALSE)` : A boolean to save the data to `/data/` or not.
  - `normalData (=10000)` : The number of positive class data rows to build the model with.
  - `percentage (=80)` : The percentage of the full data set that the positive class should be built with.

