library(ggplot2)
library(tidyverse)
library(dplyr)
library(anytime)
library(nanotime)
library(dygraphs)
path <- '/Users/nuno/IdeaProjects/project_R/'
#path <- "C:/Users/Nuno/IdeaProjects/project_R/"
setwd(paste(path, "data/thesis/movements", sep = ""))
# Load dataset from github
files <- dir()
for (file in files) {
  next_directory <- paste("data/thesis/movements", file, sep = '/')
  setwd(paste(path, next_directory, sep = ""))
  classes <- dir()
  for (class in classes) {
    next_class_directory <- paste(next_directory, class, sep = '/')
    setwd(paste(path, next_class_directory, sep = ""))
    excel_files <- dir()
    index_to_save <- 1
    for (excel in excel_files) {
      data <- read.csv(excel, header = TRUE, sep = '|')
      # Number of digits for time omilliseconds
      my_options <- options(digits.secs = 3)
      #get first timestamp record to compare vs others
      first_time <- data$loggingTime.txt.[1]


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


      #returns the difference between first timestamp and the others timestamps

      calculate_time_start <- function(time_string) {
        time <- strptime(time_string, "%Y-%m-%d %H:%M:%OS")
        return(as.numeric(difftime(time, first_time, units = "secs")))
        #df1$ diff_in_milliseconds= as.numeric(difftime(df1$Logout_time, df1$Login_time, units ="secs")) *1000

      }

      data$loggingTime.txt. <- sapply(data$loggingTime.txt., calculate_time_start)


      acelo <- data.frame(
        day = data$loggingTime.txt.,
        value = data$accelerometerAccelerationX.G.)

      # Most basic bubble plot
      p <- ggplot(acelo, aes(x = day, y = value)) +
        geom_line() +
        xlab("") +
        xlim(c(0, 10))
      #print(p)

      #cut x seconds front and back
      data <- data[data$loggingTime.txt. > min(data$loggingTime.txt. + 4),]
      data <- data[data$loggingTime.txt. < max(data$loggingTime.txt.) - 4,]

      acelo <- data.frame(
        day = data$loggingTime.txt.,
        value = data$accelerometerAccelerationX.G.)

      p <- ggplot(acelo, aes(x = day, y = value)) +
        geom_line() +
        xlab("") +
        xlim(c(2, 10))
      #print(p)

      # Format 3: Several variables for each date
      info_acel <- data.frame(
        time = data$loggingTime.txt.,
        accX = data$accelerometerAccelerationX.G.,
        accY = data$accelerometerAccelerationY.G.,
        accZ = data$accelerometerAccelerationZ.G.
      )

      # Chart
      p <- dygraph(info_acel) #%>% dySeries(color = "white")
      #print(p)


      #x <- data$accelerometerAccelerationX.G.
      #y <- data$accelerometerAccelerationY.G.
      #z <- data$accelerometerAccelerationZ.G.
      ## Plot the bar chart.
      #plot(x, type = "o", col = "red", xlab = "HZ", ylab = "Valores",
      #     main = "Acelarometro 3 eixos") +
      #  lines(y, type = "o", col = "blue") +
      #  lines(z, type = "o", col = "green") +
      #  geom_line(aes(colour = variable))

      # Accelarometer raw
      data_1 <- as.vector(as.matrix(data[, c(3, 4, 5)]))
      names <- colnames(data[3:5])
      column_names <- sort(rep(names, nrow(data)))

      final <- data.frame(x = data$loggingTime.txt., val = data_1,
                          variable = column_names)


      g <- ggplot(data = final, aes(x = x, y = val)) +
        geom_line(aes(colour = variable)) +
        ggtitle(paste("Raw Acce Data", file, class, excel, sep = " "))

      directory_to_save <- paste("data/thesis/saved", file, "photos", class, "acelarometro_raw", sep = "/")
      dir.create(file.path(paste(path, "data/thesis/saved", file, "photos", class, sep = '/')))
      dir.create(file.path(paste(path, directory_to_save, sep = '')))
      setwd(paste(path, directory_to_save, sep = ""))
      ggsave(filename = paste("Acelarometro_raw", "-", class, "-", excel, ".jpeg", sep = ""), g, device = "jpeg")
      dev.off()
      print(g)

      #Accelarometer core motion
      data_1 <- as.vector(as.matrix(data[, c(13, 14, 15)]))
      names <- colnames(data[13:15])
      column_names <- sort(rep(names, nrow(data)))

      final <- data.frame(x = data$loggingTime.txt., val = data_1,
                          variable = column_names)


      g <- ggplot(data = final, aes(x = x, y = val,)) +
        geom_line(aes(colour = variable)) +
        ggtitle(paste("Core Motion Acc Data", file, class, excel, sep = " "))

      directory_to_save <- paste("data/thesis/saved", file, "photos", class, "acelarometro", sep = "/")
      dir.create(file.path(paste(path, directory_to_save, sep = '')))
      setwd(paste(path, directory_to_save, sep = ""))
      ggsave(filename = paste("Acelerômetro", "-", class, "-", excel, ".jpeg", sep = ""), g, device = "jpeg")
      dev.off()
      print(g)


      #Gyroscope rotation
      #gyroscope <- select(data,-1:-9,-13:-33)
      #
      #data_1 <- as.vector(as.matrix(gyroscope))
      #names <- colnames(dazta[10:12])
      #column_names <- sort(rep(names,nrow(gyroscope)))
      #
      #final <- data.frame(x=data$loggingTime.txt.,val = data_1,
      #                    variable = column_names)

      #ggplot(data = final, aes(x=x, y=val,)) + geom_line(aes(colour=variable))+  ggtitle("Gyroscope")


      #Motion Rotation
      motion_gyroscope <- select(data, -1:-9, -13:-ncol(data))

      data_1 <- as.vector(as.matrix(motion_gyroscope))
      names <- colnames(data[10:12])
      column_names <- sort(rep(names, nrow(motion_gyroscope)))

      final <- data.frame(x = data$loggingTime.txt., val = data_1,
                          variable = column_names)


      g <- ggplot(data = final, aes(x = x, y = val,)) +
        geom_line(aes(colour = variable)) +
        ggtitle(paste("Motion rotation gyroscope", file, class, excel, sep = " "))

      directory_to_save <- paste("data/thesis/saved", file, "photos", class, "giroscopio", sep = "/")
      dir.create(file.path(paste(path, directory_to_save, sep = '')))
      setwd(paste(path, directory_to_save, sep = ""))

      ggsave(filename = paste("Giroscopio", "-", class, "-", excel, ".jpeg", sep = ""), g, device = "jpeg")
      dev.off()
      print(g)


      #Motion Yaw,Roll,Pitch
      motion_yaw_roll_pitch <- select(data, -1:-6, -10:-ncol(data))

      data_1 <- as.vector(as.matrix(motion_yaw_roll_pitch))
      names <- colnames(data[7:9])

      motionYaw.rad. <- rep(names[1], nrow(motion_yaw_roll_pitch))
      motionRoll.rad. <- rep(names[2], nrow(motion_yaw_roll_pitch))
      motionPitch.rad. <- rep(names[3], nrow(motion_yaw_roll_pitch))

      motion_options <- as.vector(as.matrix(motionYaw.rad., motionRoll.rad., motionPitch.rad.))
      column_names <- sort(rep(names, nrow(motion_yaw_roll_pitch)))

      final <- data.frame(x = data$loggingTime.txt., val = data_1,
                          variable = column_names)

      g <- ggplot(data = final, aes(x = x, y = val,)) +
        geom_line(aes(colour = variable)) +
      ggtitle(paste("Yaw Pitch Roll", file, class, excel, sep = " "))

      directory_to_save <- paste("data/thesis/saved", file, "photos", class, "motion_yaw", sep = "/")
      dir.create(file.path(paste(path, directory_to_save, sep = '')))
      setwd(paste(path, directory_to_save, sep = ""))
      ggsave(filename = paste("Motion Yaw, Roll, Pitch", "-", class, "-", excel, ".jpeg", sep = ""), g, device = "jpeg")
      dev.off()
      print(g)


      #TO DO MOTION QUARTERNIONX.R
      #Motion Rotation
      motion_quaternion <- select(data, -1:-16, -21:-ncol(data))

      data_1 <- as.vector(as.matrix(motion_quaternion))
      names <- colnames(data[17:20])
      column_names <- sort(rep(names, nrow(motion_quaternion)))

      final <- data.frame(x = data$loggingTime.txt., val = data_1,
                          variable = column_names)


      g <- ggplot(data = final, aes(x = x, y = val,)) +
        geom_line(aes(colour = variable)) +
        ggtitle(paste("Motion Quaternion", file, class, excel, sep = " "))

      directory_to_save <- paste("data/thesis/saved", file, "photos", class, "quartz", sep = "/")
      dir.create(file.path(paste(path, directory_to_save, sep = '')))
      setwd(paste(path, directory_to_save, sep = ""))

      ggsave(filename = paste("Quartz", "-", class, "-", excel, ".jpeg", sep = ""), g, device = "jpeg")
      dev.off()

      print(g)



      test <- data.frame(data$motionRotationRateX.rad.s., data$motionRotationRateY.rad.s.,
                         data$motionRotationRateZ.rad.s., data$accelerometerAccelerationX.G.,
                         data$accelerometerAccelerationY.G., data$accelerometerAccelerationZ.G.,
                         data$motionQuaternionX.R., data$motionQuaternionY.R., data$motionQuaternionZ.R.,
                         data$motionQuaternionW.R.)
      colnames(test) <- c('rotation_x', 'rotation_y', 'rotation_z', 'acceleration_x', 'acceleration_y', 'acceleration_z', 'motionQuaternionX',
                          'motionQuaternionY', 'motionQuaternionZ', 'motionQuaternionW')
      #Join path to save directory
      directory_to_save <- paste("data/thesis/saved", file, class, sep = "/")
      dir.create(file.path(paste(path, directory_to_save, sep = '/')))
      setwd(paste(path, directory_to_save, sep = ""))
      write.csv(test, paste(index_to_save, "csv", sep = "."), row.names = FALSE)
      index_to_save <- index_to_save + 1
      #Restore current working directory
      next_class_directory <- paste(next_directory, class, sep = '/')
      setwd(paste(path, next_class_directory, sep = ""))
    }
  }
}



