# Title     : TODO
# Objective : TODO
# Created by: Nuno
# Created on: 11/17/2020

############################################################
##  Intro to time series analysis in R - Part 1
##  Global Gridded Monthly Climate Series prepared by Drs. Wilmott & Matsuura
##  Load in data, subset, create time series, estimate autocorrelation
##  Data available here: http://climate.geog.udel.edu/~climate/index.shtml
############################################################

############################################################
##  Load Libraries
##  zoo, xts, forecast
############################################################

library(zoo)

library(xts)

library(forecast)

###########################################################
##  Set working directory
##  Load data
##  Add headers
###########################################################


path <- "C:/Users/Nuno/IdeaProjects/project_R/"

all_data <- data.frame()

setwd(paste(path, "data/", sep = ""))
files <- dir()

##  For each file in directory assign year in file name to year,
##  read it in, assign headers, bind to all_data

for(file in files){
  year <- substr(file, 8, 11)
  year_data <- read.table(file)
  names(year_data) <- c("lon", "lat", "jan", "feb", "mar", "apr", "may",
                        "jun", "jul", "aug", "sep", "oct", "nov", "dec", "ann_total")
  year_data[,"year"] <- as.numeric(year)
  all_data <- rbind(all_data, year_data)
}



###########################################################
##  Prep data for analysis
##  Subset all_data to location in Southern Africa
##  Remove non-time related columns and create time series
###########################################################


##	Extract data from a spatial subset:  -17.75 deg. S, and 23.25 deg. E:
subset_indices <- (all_data[,"lon"] == 23.25 & all_data[,"lat"] == -17.75)
subset <- all_data[subset_indices,]

##	Drop the lat/long, ann_total, and year columns from the dataset:
subset <- subset[,c(-1,-2,-15,-16)]

##  Transpose columns and create vector
subset <- as.vector(t(subset))

##  Create time series object from vector
precip.ts <- ts(subset, start = c(2000, 1), end = c(2017, 12), frequency = 12)


##########################################################
##  Perform Analysis
##  Basic Plots of time series
##  Estimate Autocorrelation Function
##########################################################

##	Perform some basic plotting and analyses:
plot(precip.ts)

##  Let's tidy up that plot a little
plot(precip.ts, xlab = "Date", ylab = "Precip. (mm)", axes = FALSE)
axis(1, at = seq(2000, 2017, by=1), cex.axis = 0.7)
axis(2, at = seq(0, 800, by=50), cex.axis = 0.7)
abline(v = seq(2000, 2017, by=1), h = seq(0, 400, by=50), col = "grey", lty = 3)
lines(precip.ts)
box()

##  Subset the time series to 2004 and plot
precip.subset <- window(precip.ts, start = c(2004,1), end = c(2004, 12))
plot(precip.subset)
plot(precip.subset, xaxt = "n")
axis(1, at =seq(2004.0, 2004.99, by=1/12), labels = c("jan", "feb", "mar", "apr", "may",
                                                      "jun", "jul", "aug", "sep", "oct", "nov", "dec"))

##  Another (easier) way to subset a time series using xts package
precip.xts <- as.xts(precip.ts)
plot(precip.xts)
plot(precip.xts["2004"])
plot(rollmean(precip.xts, 3))

##  Sum function applied at yearly period in time series
yearly_total <- apply.yearly(precip.xts, "sum")
plot(yearly_total)

##  Estimate Autocorrelation Function
acf(precip.xts)


