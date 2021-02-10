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
data <- read.csv("apple_watch_1_direita.csv", header = TRUE,sep = '|')

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
  day = data$accelerometerTimestamp_sinceReboot.s.,
  value = data$accelerometerAccelerationX.G.)


# Most basic bubble plot
p <- ggplot(acelo, aes(x=day, y=value)) +
  geom_line() +
  xlab("")
p


# Format 3: Several variables for each date
info_acel <- data.frame(
  time=data$accelerometerTimestamp_sinceReboot.s.,
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
data_1 <- as.vector(as.matrix(data[,c(3,4,5)]))
names <- colnames(data[3:5])
column_names <- sort(rep(names,nrow(data)))

final <- data.frame(x=data$accelerometerTimestamp_sinceReboot.s.,val = data_1,
                    variable = column_names)


ggplot(data = final, aes(x=x, y=val)) + geom_line(aes(colour=variable))+ggtitle("Raw Acce Data")

#Accelarometer core motion
core_motion <- select(data,-1:-12,-16:-29)

data_1 <- as.vector(as.matrix(core_motion))
names <- colnames(data[13:15])
column_names <- sort(rep(names,nrow(data)))

final <- data.frame(x=data$accelerometerTimestamp_sinceReboot.s.,val = data_1,
                    variable = column_names)


ggplot(data = final, aes(x=x, y=val,)) + geom_line(aes(colour=variable))+  ggtitle("Core Motion Acc Data")


#Gyroscope rotation
gyroscope <- select(data,-1:-9,-13:-29)

data_1 <- as.vector(as.matrix(gyroscope))
names <- colnames(data[10:12])
column_names <- sort(rep(names,nrow(gyroscope)))

final <- data.frame(x=data$motionTimestamp_sinceReboot.s.,val = data_1,
                    variable = column_names)


ggplot(data = final, aes(x=x, y=val,)) + geom_line(aes(colour=variable))+  ggtitle("Gyroscope")


#Motion Rotation
motion_gyroscope <- select(data,-1:-14,-18:-41)

data_1 <- as.vector(as.matrix(motion_gyroscope))
names <- colnames(data[15:17])
column_names <- sort(rep(names,nrow(motion_gyroscope)))

final <- data.frame(x=data$loggingSample.N.,val = data_1,
                    variable = column_names)


ggplot(data = final, aes(x=x, y=val,)) + geom_line(aes(colour=variable))+xlim(c(0,500))+  ggtitle("Motion rotation gyroscope")



#Motion Yaw,Roll,Pitch
motion_yaw_roll_pitch <- select(data,-1:-6,-10:-29)

data_1 <- as.vector(as.matrix(motion_yaw_roll_pitch))
names <- colnames(data[7:9])

motionYaw.rad. <- rep(names[1],nrow(motion_yaw_roll_pitch))
motionRoll.rad. <- rep(names[2],nrow(motion_yaw_roll_pitch))
motionPitch.rad. <- rep(names[3],nrow(motion_yaw_roll_pitch))

motion_options <- as.vector(as.matrix(motionYaw.rad.,motionRoll.rad.,motionPitch.rad.))
column_names <- sort(rep(names,nrow(motion_yaw_roll_pitch)))

final <- data.frame(x=data$motionTimestamp_sinceReboot.s.,val = data_1,
                    variable = column_names)

ggplot(data = final, aes(x=x, y=val,)) + geom_line(aes(colour=variable)) + ggtitle("Yaw Pitch Roll")


#TO DO MOTION QUARTERNIONX.R
#Motion Rotation
motion_quaternion <- select(data,-1:-16,-21:-29)

data_1 <- as.vector(as.matrix(motion_quaternion))
names <- colnames(data[17:20])
column_names <- sort(rep(names,nrow(motion_quaternion)))

final <- data.frame(x=data$motionTimestamp_sinceReboot.s.,val = data_1,
                    variable = column_names)


ggplot(data = final, aes(x=x, y=val,)) + geom_line(aes(colour=variable))+  ggtitle("Motion Quaternion")
