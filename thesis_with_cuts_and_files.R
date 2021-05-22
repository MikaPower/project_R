# Title     : TODO
# Objective : TODO
# Created by: nuno
# Created on: 29/03/2021

library(ggplot2)
library(tidyverse)
library(dplyr)
library(anytime)
library(nanotime)
library(dygraphs)
path <- '/Users/nuno/IdeaProjects/project_R/data/thesis/'
#path <- "C:/Users/Nuno/IdeaProjects/project_R/"
setwd(path)
# Load dataset from github
files <- dir(paste(path, 'movements/', sep = ""))
for (file in files) {
  next_directory <- paste("/movements", file, sep = '/')
  classes <- dir(paste0(path, next_directory, sep = ''))
  dir.create(paste0(path, '/saved/', file))
  dir.create(paste0(path, '/saved/', file, '/photos'))
  for (class in classes) {
    next_class_directory <- paste(next_directory, class, sep = '/')
    dir.create(paste0(path, '/saved/', file, '/photos/', class))
    dir.create(paste(path, 'saved', file, class, sep = '/'))
    excel_files <- dir(paste0(path, next_class_directory))
    index_to_save <- 1
    for (excel in excel_files) {

      path_to_csv <- paste0(path, next_class_directory, '/', excel)
      data <- read.csv(path_to_csv, header = TRUE, sep = '|')
      data <-na.omit(data)
      print(excel)
      # Number of digits for time omilliseconds
      my_options <- options(digits.secs = 3)
      #get first timestamp record to compare vs others
      first_time <- data$loggingTime.txt.[1]


      #returns the difference between first timestamp and the others timestamps

      calculate_time_start <- function(time_string) {
        time <- strptime(time_string, "%Y-%m-%d %H:%M:%OS")
        return(as.numeric(difftime(time, first_time, units = "secs")))
        #df1$ diff_in_milliseconds= as.numeric(difftime(df1$Logout_time, df1$Login_time, units ="secs")) *1000

      }

      data$loggingTime.txt. <- sapply(data$loggingTime.txt., calculate_time_start)
      temporary_df <- data.frame(
        teste = character()
      )
      row <- 1

      limit_val <-switch(class, "bloco" = 1, "flip_direita" = 1, "flip_esquerda" = 1,"top_spin_esquerda" = 1,"top_spin_direita" = 1)
      limit_axis <- switch(class, "bloco" = 'motionUserAccelerationZ.G.', "flip_direita" = 'motionUserAccelerationX.G.', "flip_esquerda" = 'motionUserAccelerationZ.G.',"top_spin_esquerda" = 'motionUserAccelerationZ.G.',"top_spin_direita" = 'motionUserAccelerationX.G.')



      calculate_graph <- function(table, names, title_graph, title_dir, sort = TRUE,df,count) {

        if (sort) {
          column_names <- sort(rep(names, nrow(df)))
        }
        else {
          column_names <- names
        }

        final <- data.frame(x = df$loggingTime.txt., val = table,
                            variable = column_names)


        g <- ggplot(data = final, aes(x = x, y = val)) +
          geom_line(aes(colour = variable)) +
          ggtitle(paste(title_graph, file, class, excel, sep = " "))

        directory_to_save <- paste("/saved", file, "photos", class, title_dir, sep = "/")
        dir.create(paste(path, directory_to_save, sep = ''))
        ggsave(filename = paste(title_dir, "-", class, "-", excel,count, ".jpeg", sep = ""), path = paste0(path, directory_to_save), g, device = "jpeg")
        show(g)
        dev.off()

      }
      count <- 0
      while (row <= nrow(data)) {
        axxis <- data[row+10, limit_axis]
        time <- data[row+10, "loggingTime.txt."]
        if (!is.na(axxis) && abs(axxis) > limit_val) {
          mini_sub <- data[row:(row+45),]
          row <- row+45


          #raw accelarometer
          func_data <- as.vector(as.matrix(mini_sub[, c(13, 14, 15)]))
          names <- colnames(mini_sub[3:5])

          #calculate_graph(func_data, names, 'Acce Data', 'Acelarometro', TRUE,mini_sub,count)
          count <- count+1

          test <- data.frame(mini_sub$motionRotationRateX.rad.s., mini_sub$motionRotationRateY.rad.s.,
                             mini_sub$motionRotationRateZ.rad.s., mini_sub$motionUserAccelerationX.G.,
                             mini_sub$motionUserAccelerationY.G., mini_sub$motionUserAccelerationZ.G.,
                             mini_sub$motionQuaternionX.R., mini_sub$motionQuaternionY.R., mini_sub$motionQuaternionZ.R.,
                             mini_sub$motionQuaternionW.R.)
          colnames(test) <- c('rotation_x', 'rotation_y', 'rotation_z', 'acceleration_x', 'acceleration_y', 'acceleration_z', 'motionQuaternionX',
                              'motionQuaternionY', 'motionQuaternionZ', 'motionQuaternionW')
          #Join path to save directory
          directory_to_save <- paste(path, "saved", file, class, sep = "/")
          write.csv(test, paste0(directory_to_save, '/', index_to_save, ".csv"), row.names = FALSE)
          index_to_save <- index_to_save + 1

        }
        else{
          data[row,2:ncol(data)]<- 0
          row <- row+1
        }
      }






      #cut x seconds front and back
      #data <- data[data$loggingTime.txt. > min(data$loggingTime.txt. + 4),]
      #data <- data[data$loggingTime.txt. < max(data$loggingTime.txt.) - 4,]



      #print(p)

     # # Format 3: Several variables for each date
     # info_acel <- data.frame(
     #   time = data$loggingTime.txt.,
     #   accX = data$accelerometerAccelerationX.G.,
     #   accY = data$accelerometerAccelerationY.G.,
     #   accZ = data$accelerometerAccelerationZ.G.
     # )

     # # Chart
     # p <- dygraph(info_acel) #%>% dySeries(color = "white")
     # #show(p)







     # #raw accelarometer
     # data_1 <- as.vector(as.matrix(data[, c(3, 4, 5)]))
     # names <- colnames(data[3:5])
     # calculate_graph(data_1, names, 'Raw Acce Data', 'Acelarometro_raw')

     # #Core motion acc
     # data_1 <- as.vector(as.matrix(data[, c(13, 14, 15)]))
     # names <- colnames(data[13:15])
     # calculate_graph(data_1, names, 'Core Motion Acc Data', 'acelarometro')


     # #Motion gyroscope
     # data_1 <- as.vector(as.matrix(select(data, -1:-9, -13:-ncol(data))))
     # names <- colnames(data[10:12])
     # calculate_graph(data_1, names, 'Motion rotation gyroscope', 'giroscopio')


     # #Motion Yaw,Roll,Pitch
     # motion_yaw_roll_pitch <- select(data, -1:-6, -10:-ncol(data))

     # data_1 <- as.vector(as.matrix(motion_yaw_roll_pitch))
     # names <- colnames(data[7:9])
     # motionYaw.rad. <- rep(names[1], nrow(motion_yaw_roll_pitch))
     # motionRoll.rad. <- rep(names[2], nrow(motion_yaw_roll_pitch))
     # motionPitch.rad. <- rep(names[3], nrow(motion_yaw_roll_pitch))
     # names <- c(motionYaw.rad., motionRoll.rad., motionPitch.rad.)
     # calculate_graph(data_1, names, 'Yaw Pitch Roll', 'giroscopio', FALSE)


     # #Motion Rotation
     # motion_quaternion <- select(data, -1:-16, -21:-ncol(data))
     # data_1 <- as.vector(as.matrix(motion_quaternion))
     # names <- colnames(data[17:20])
     # motionQuaternionX.R. <- rep(names[1], nrow(data))
     # motionQuaternionY.R. <- rep(names[2], nrow(data))
     # motionQuaternionZ.R. <- rep(names[3], nrow(data))
     # motionQuaternionW.R. <- rep(names[4], nrow(data))
     # names <- c(motionQuaternionX.R., motionQuaternionY.R., motionQuaternionZ.R., motionQuaternionW.R.)

     # calculate_graph(data_1, names, 'Motion Quaternion', 'quartz', FALSE)
     # #test <- data.frame(data$motionRotationRateX.rad.s., data$motionRotationRateY.rad.s.,
     # #                   data$motionRotationRateZ.rad.s., data$accelerometerAccelerationX.G.,
     # #                   data$accelerometerAccelerationY.G., data$accelerometerAccelerationZ.G.,
     # #                   data$motionQuaternionX.R., data$motionQuaternionY.R., data$motionQuaternionZ.R.,
     # #                   data$motionQuaternionW.R.)

     # test <- data.frame(data$motionRotationRateX.rad.s., data$motionRotationRateY.rad.s.,
     #                    data$motionRotationRateZ.rad.s., data$motionUserAccelerationX.G.,
     #                    data$motionUserAccelerationY.G., data$motionUserAccelerationZ.G.,
     #                    data$motionQuaternionX.R., data$motionQuaternionY.R., data$motionQuaternionZ.R.,
     #                    data$motionQuaternionW.R.)
     # colnames(test) <- c('rotation_x', 'rotation_y', 'rotation_z', 'acceleration_x', 'acceleration_y', 'acceleration_z', 'motionQuaternionX',
     #                     'motionQuaternionY', 'motionQuaternionZ', 'motionQuaternionW')
     # #Join path to save directory
     # directory_to_save <- paste(path, "saved", file, class, sep = "/")
     # write.csv(test, paste0(directory_to_save, '/', index_to_save, ".csv"), row.names = FALSE)
     # index_to_save <- index_to_save + 1
    }
  }
}



