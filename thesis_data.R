library(ggplot2)
library(tidyverse)
library(dplyr)
library(anytime)
library(nanotime)
library(dygraphs)
path <-'/Users/nuno/IdeaProjects/project_R/'
#path <- "C:/Users/Nuno/IdeaProjects/project_R/"
setwd(paste(path, "data/thesis", sep = ""))
# Load dataset from github
data <- read.csv("teste1_iphone.csv", header = TRUE,sep = '|')

#data <- select(data,-7,-11:-41)

#join_times <- function(time_string) {
#  list_times <- strsplit(time_string, " ")
#  result <- paste(list_times[[1]][1],'T')
#  result <- paste(result,list_times[[1]][2])
#  result <- paste(result,list_times[[1]][3])
#  result <- gsub(" ", "", result, fixed = TRUE)
#  print("resultado: ")
#  print(result)
#  return (result)
#  }
#
#data$loggingTime.txt. <- sapply(data$loggingTime.txt., join_times)

acelo<- data.frame(
  day = data$loggingSample.N.,
  value = data$accelerometerAccelerationX.G.)


# Most basic bubble plot
p <- ggplot(acelo, aes(x=day, y=value)) +
  geom_line() +
  xlab("")
p


# Format 3: Several variables for each date
info_acel <- data.frame(
  time=data$loggingSample.N.,
  accX=data$accelerometerAccelerationX.G.,
  accY=data$accelerometerAccelerationY.G.,
  accZ=data$accelerometerAccelerationZ.G.
)

# Chart
p <- dygraph(info_acel) #%>% dySeries(color = "white")
p



x <- data$accelerometerAccelerationX.G.
y <- data$accelerometerAccelerationY.G.
z <- data$accelerometerAccelerationZ.G.
# Plot the bar chart.
plot(x,type = "o",col = "red", xlab = "HZ", ylab = "Valores",
     main = "Acelarometro 3 eixos")+
  lines(y, type = "o", col = "blue")+
  lines(z, type = "o", col = "green")+
  geom_line(aes(colour=variable))

# Accelarometer raw
data_1 <- as.vector(as.matrix(data[,c(3,4,6)]))
names <- colnames(data[4:6])
column_names <- sort(rep(names,nrow(data)))

final <- data.frame(x=data$loggingSample.N.,val = data_1,
                    variable = column_names)


ggplot(data = final, aes(x=x, y=val)) + geom_line(aes(colour=variable))+xlim(c(0,500))+ggtitle("Raw Acce Data")

#Accelarometer core motion
core_motion <- select(data,-1:-17,-21:-41)

data_1 <- as.vector(as.matrix(core_motion))
names <- colnames(data[18:20])
column_names <- sort(rep(names,nrow(data)))

final <- data.frame(x=data$loggingSample.N.,val = data_1,
                    variable = column_names)


ggplot(data = final, aes(x=x, y=val,)) + geom_line(aes(colour=variable))+xlim(c(0,500))+  ggtitle("Core Motion Acc Data")


#Gyroscope rotation
gyroscope <- select(data,-1:-7,-11:-41)

data_1 <- as.vector(as.matrix(gyroscope))
names <- colnames(data[8:10])
column_names <- sort(rep(names,nrow(gyroscope)))

final <- data.frame(x=data$loggingSample.N.,val = data_1,
                    variable = column_names)


ggplot(data = final, aes(x=x, y=val,)) + geom_line(aes(colour=variable))+xlim(c(0,500))+  ggtitle("Gyroscope")


#Motion Rotation
motion_gyroscope <- select(data,-1:-14,-18:-41)

data_1 <- as.vector(as.matrix(motion_gyroscope))
names <- colnames(data[15:17])
column_names <- sort(rep(names,nrow(motion_gyroscope)))

final <- data.frame(x=data$loggingSample.N.,val = data_1,
                    variable = column_names)


ggplot(data = final, aes(x=x, y=val,)) + geom_line(aes(colour=variable))+xlim(c(0,500))+  ggtitle("Motion rotation gyroscope")



#Motion Yaw,Roll,Pitch
motion_yaw_roll_pitch <- select(data,-1:-11,-15:-41)

data_1 <- as.vector(as.matrix(motion_yaw_roll_pitch))
names <- colnames(data[12:14])

motionYaw.rad. <- rep(names[1],nrow(motion_yaw_roll_pitch))
motionRoll.rad. <- rep(names[2],nrow(motion_yaw_roll_pitch))
motionPitch.rad. <- rep(names[3],nrow(motion_yaw_roll_pitch))

motion_options <- as.vector(as.matrix(motionYaw.rad.,motionRoll.rad.,motionPitch.rad.))
column_names <- sort(rep(names,nrow(motion_yaw_roll_pitch)))

final <- data.frame(x=data$loggingSample.N.,val = data_1,
                    variable = column_names)

ggplot(data = final, aes(x=x, y=val,)) + geom_line(aes(colour=variable))+xlim(c(0,500)) + ggtitle("Yaw Pitch Roll")


#TO DO MOTION QUARTERNIONX.R
#Motion Rotation
motion_quaternion <- select(data,-1:-21,-26:-41)

data_1 <- as.vector(as.matrix(motion_quaternion))
names <- colnames(data[22:25])
column_names <- sort(rep(names,nrow(motion_quaternion)))

final <- data.frame(x=data$loggingSample.N.,val = data_1,
                    variable = column_names)


ggplot(data = final, aes(x=x, y=val,)) + geom_line(aes(colour=variable))+xlim(c(0,500))+  ggtitle("Motion Quaternion")