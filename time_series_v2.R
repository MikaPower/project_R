# Title     : TODO
# Objective : TODO
# Created by: Nuno
# Created on: 11/17/2020

############################################################
##  Intro to time series analysis in R - Part 2
##  GISS Surface Temperature Analysis (GISTEMPv4) prepared by
##  NASA Goddard Institute for Space Studies
##  Load data, decompose time series, fit ARIMA model and forecast, assess accuracy
##  Data available here: https://data.giss.nasa.gov/gistemp/
############################################################


############################################################
##  Load Libraries
##  ggplot2, forecast
############################################################

library(ggplot2)

library(forecast)


###########################################################
##  Set working directory
##  Load data
##  Create time series
###########################################################

#path <- "C:/Users/Nuno/IdeaProjects/project_R/"
path <-'/Users/nuno/IdeaProjects/project_R/'
setwd(paste(path, "data/part2/", sep = ""))


##  Read in csv
gt <- read.csv("Temperature.csv")

##  Remove years column
gt <- gt[,-1]


##  Transpose columns and create vector
gt <- as.vector(t(gt))


##  Create time series object from vector
gt.ts <- ts(gt, start = c(1880, 1), end = c(2018, 12), frequency = 12)

##  Plot time series
plot(gt.ts)

gt.xts <- as.xts(gt.ts)
plot(gt.xts)

##########################################################
##  Explore time series
##  Decompose time series
##########################################################


##  Time series have 3 components: trend, seasonality, random error (irregular fluctuations)

##  Explore TREND
##  Fit regression to series
abline(reg = lm(gt.ts ~ time(gt.ts)), col = "red", lwd = 2)
##  Calculate annual mean
plot(aggregate(gt.ts, FUN = mean))
abline(reg = lm(gt.ts ~ time(gt.ts)), col = "red", lwd = 2)
##  Remove trend by taking first difference (change in value across months)
plot(diff(gt.ts))

##  Explore SEASONALITY
plot(gt.ts)
gt.subset <- window(gt.ts, start = c(2000, 1), end = c(2018, 12))
plot(gt.subset)
##  Create box plots for months
boxplot(gt.ts~cycle(gt.ts))
##  Try season plot from ggplot2 package
ggseasonplot(diff(gt.ts))

##  Another way of decomposing time series
gt.subset <- window(gt.ts, start = c(1980, 1), end = c(2018, 12))
gt.ts.decomp <- decompose(gt.subset, type = "multiplicative")
plot(gt.ts.decomp)


##########################################################
##  Fit ARIMA model to time series
##  Perform model diagnostics
##  Forecast future using model
##########################################################

##  ARIMA - Auto Regressive Integrated Moving Average
##  Data must be stationary - trend and seasonality must be removed
##  Can do this manually using diff() for trend and log for seasonality
##  Here we will use an automated method that takes care of stationarity for us

path <-'/Users/nuno/IdeaProjects/project_R/'

#########################################################
##  Save output and time series data
#########################################################

save.image(paste(path, "output/Global_Temperature.RData", sep = ""))

##  How to re-load a RData file
rm(gt.ts)
prior <- load(paste(path, "output/Global_Temperature.RData", sep = ""), verbose =TRUE)








