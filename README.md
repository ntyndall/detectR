<img align="right" width="125" height="150" src="https://raw.githubusercontent.com/ntyndall/detectR/master/images/sticker.png">

# detectR
[![Build Status](https://travis-ci.org/ntyndall/detectR.svg?branch=master)](https://travis-ci.org/ntyndall/detectR)
[![codecov](https://codecov.io/gh/ntyndall/detectR/branch/master/graph/badge.svg)](https://codecov.io/gh/ntyndall/detectR)

# Installation

# Guide
This package is ready to use, with models already pre-built into the package. To get started, supply input arguments as follows
```{r}
detectR::detectR(x = "this is a test") 
# "N"
detectR::detectR(x = list("input 1", "input 2")) 
# c("N", "N")
detectR::detectR(x = "SELECT * FROM Users WHERE UserId = 105 OR 1=1;") 
# "S" 
```

It is possible to play around with the input and load your own data sets, provided it is in the appropriate format. To do so, you can write a small script and pass the data frames like so,
```{r}
dataX <- data.frame() # Data source 1
dataY <- data.frame() # Data source 2

detectR::builder(dataX, dataY)
```
