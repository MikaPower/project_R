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


full_df <- matrix(ncol = 46)
labels_matrix <- matrix('bloco')
rest_df <- data_frame()
dir.create(paste0(path, '/full_df/'))

df_accX <- matrix(ncol = 46)
df_accY <- matrix(ncol = 46)
df_accZ <- matrix(ncol = 46)
df_gyroX <- matrix(ncol = 46)
df_gyroY <- matrix(ncol = 46)
df_gyroZ <- matrix(ncol = 46)


for (file in files) {
  next_directory <- paste("/movements", file, sep = '/')
  classes <- dir(paste0(path, next_directory, sep = ''))
  dir.create(paste0(path, '/saved/', file))
  dir.create(paste0(path, '/saved/', file, '/photos/'))
  for (class in classes) {
    next_class_directory <- paste(next_directory, class, sep = '/')

    dir.create(paste0(path, '/saved/', file, '/photos/', class))
    dir.create(paste(path, 'saved', file, class, sep = '/'))
    excel_files <- dir(paste0(path, next_class_directory))
    index_to_save <- 1
    for (excel in excel_files) {
      rest_count <- 0
      path_to_csv <- paste0(path, next_class_directory, '/', excel)
      data <- read.csv(path_to_csv, header = TRUE, sep = '|')
      data <- na.omit(data)
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
      count <- 0
      limit_val <- switch(class, "bloco" = 1, "flip_direita" = 1, "flip_esquerda" = 1, "top_spin_esquerda" = 1, "top_spin_direita" = 1)
      limit_axis <- switch(class, "bloco" = 'motionUserAccelerationZ.G.', "flip_direita" = 'motionUserAccelerationX.G.', "flip_esquerda" = 'motionUserAccelerationZ.G.', "top_spin_esquerda" = 'motionUserAccelerationZ.G.', "top_spin_direita" = 'motionUserAccelerationX.G.')
      label_name <- switch(class, "bloco" = 2, "flip_direita" = 3, "flip_esquerda" = 4, "top_spin_esquerda" = 1, "top_spin_direita" = 0)

      if (class == 'rest') {
        #Rest set
        test <- data.frame(data$motionRotationRateX.rad.s., data$motionRotationRateY.rad.s.,
                           data$motionRotationRateZ.rad.s., data$motionUserAccelerationX.G.,
                           data$motionUserAccelerationY.G., data$motionUserAccelerationZ.G.,
                           data$motionQuaternionX.R., data$motionQuaternionY.R., data$motionQuaternionZ.R.,
                           data$motionQuaternionW.R.)
        colnames(test) <- c('rotation_x', 'rotation_y', 'rotation_z', 'acceleration_x', 'acceleration_y', 'acceleration_z', 'motionQuaternionX',
                            'motionQuaternionY', 'motionQuaternionZ', 'motionQuaternionW')


        small_matrix <- matrix(test$acceleration_x, ncol = 46)
        df_accX <- rbind(df_accX, small_matrix)

        small_matrix <- matrix(test$acceleration_y, ncol = 46)
        df_accY <- rbind(df_accY, small_matrix)

        small_matrix <- matrix(test$acceleration_z, ncol = 46)
        df_accZ <- rbind(df_accZ, small_matrix)

        small_matrix <- matrix(test$rotation_x, ncol = 46)
        df_gyroX <- rbind(df_gyroX, small_matrix)

        small_matrix <- matrix(test$rotation_y, ncol = 46)
        df_gyroY <- rbind(df_gyroY, small_matrix)

        small_matrix <- matrix(test$rotation_z, ncol = 46)
        df_gyroZ <- rbind(df_gyroZ, small_matrix)

        label <- matrix(rep(5, 1), nrow(small_matrix))
        labels_matrix <- rbind(labels_matrix, label)
      }
      else {
        while (row <= nrow(data)) {
          axxis <- data[row + 10, limit_axis]
          time <- data[row + 10, "loggingTime.txt."]
          if (!is.na(axxis) && abs(axxis) > limit_val) {
            mini_sub <- data[row:(row + 45),]
            row <- row + 45


            count <- count + 1
            label <- matrix(rep(label_name, 1), 1)
            test <- data.frame(mini_sub$motionRotationRateX.rad.s., mini_sub$motionRotationRateY.rad.s.,
                               mini_sub$motionRotationRateZ.rad.s., mini_sub$motionUserAccelerationX.G.,
                               mini_sub$motionUserAccelerationY.G., mini_sub$motionUserAccelerationZ.G.,
                               mini_sub$motionQuaternionX.R., mini_sub$motionQuaternionY.R., mini_sub$motionQuaternionZ.R.,
                               mini_sub$motionQuaternionW.R., label)
            colnames(test) <- c('rotation_x', 'rotation_y', 'rotation_z', 'acceleration_x', 'acceleration_y', 'acceleration_z', 'motionQuaternionX',
                                'motionQuaternionY', 'motionQuaternionZ', 'motionQuaternionW', 'label')

            labels_matrix <- rbind(labels_matrix, label)
            small_matrix <- matrix(test$acceleration_x, ncol = 46)
            df_accX <- rbind(df_accX, small_matrix)

            small_matrix <- matrix(test$acceleration_y, ncol = 46)
            df_accY <- rbind(df_accY, small_matrix)

            small_matrix <- matrix(test$acceleration_z, ncol = 46)
            df_accZ <- rbind(df_accZ, small_matrix)

            small_matrix <- matrix(test$rotation_x, ncol = 46)
            df_gyroX <- rbind(df_gyroX, small_matrix)

            small_matrix <- matrix(test$rotation_y, ncol = 46)
            df_gyroY <- rbind(df_gyroY, small_matrix)

            small_matrix <- matrix(test$rotation_z, ncol = 46)
            df_gyroZ <- rbind(df_gyroZ, small_matrix)
          }
          else {
            if (rest_count < 500) {
              rest_df <- rbind(rest_df, data[row,])
              rest_count <- rest_count + 1
              #print(nrow(rest_df))
            }
            row <- row + 1
          }
        }
      }
    }
  }
}


#Rest set
test <- data.frame()
test <- data.frame(rest_df$motionRotationRateX.rad.s., rest_df$motionRotationRateY.rad.s.,
                   rest_df$motionRotationRateZ.rad.s., rest_df$motionUserAccelerationX.G.,
                   rest_df$motionUserAccelerationY.G., rest_df$motionUserAccelerationZ.G.,
                   rest_df$motionQuaternionX.R., rest_df$motionQuaternionY.R., rest_df$motionQuaternionZ.R.,
                   rest_df$motionQuaternionW.R.)
colnames(test) <- c('rotation_x', 'rotation_y', 'rotation_z', 'acceleration_x', 'acceleration_y', 'acceleration_z', 'motionQuaternionX',
                    'motionQuaternionY', 'motionQuaternionZ', 'motionQuaternionW')


small_matrix <- matrix(test$acceleration_x, ncol = 46)
df_accX <- rbind(df_accX, small_matrix)

small_matrix <- matrix(test$acceleration_y, ncol = 46)
df_accY <- rbind(df_accY, small_matrix)

small_matrix <- matrix(test$acceleration_z, ncol = 46)
df_accZ <- rbind(df_accZ, small_matrix)

small_matrix <- matrix(test$rotation_x, ncol = 46)
df_gyroX <- rbind(df_gyroX, small_matrix)

small_matrix <- matrix(test$rotation_y, ncol = 46)
df_gyroY <- rbind(df_gyroY, small_matrix)

small_matrix <- matrix(test$rotation_z, ncol = 46)
df_gyroZ <- rbind(df_gyroZ, small_matrix)

label <- matrix(rep(5, 1), nrow(small_matrix))
labels_matrix <- rbind(labels_matrix, label)


#Join path to save directory
directory_to_save <- paste(path, "full_df", sep = "/")
df_accX <- cbind(df_accX, labels_matrix)
df_accY <- cbind(df_accY, labels_matrix)
df_accZ <- cbind(df_accZ, labels_matrix)
df_gyroX <- cbind(df_gyroX, labels_matrix)
df_gyroY <- cbind(df_gyroY, labels_matrix)
df_gyroZ <- cbind(df_gyroZ, labels_matrix)


df_accX <- df_accX[-1,]
df_accY <- df_accY[-1,]
df_accZ <- df_accZ[-1,]
df_gyroX <- df_gyroX[-1,]
df_gyroY <- df_gyroY[-1,]
df_gyroZ <- df_gyroZ[-1,]


df_accX<- na.omit(df_accX)
df_accY<- na.omit(df_accY)
df_accZ<- na.omit(df_accZ)
df_gyroX<- na.omit(df_gyroX)
df_gyroY<- na.omit(df_gyroY)
df_gyroZ<- na.omit(df_gyroZ)

write.csv(df_accX, paste0(directory_to_save, '/', 'final_df_accX_only', ".csv"), row.names = FALSE)
write.csv(df_accY, paste0(directory_to_save, '/', 'final_df_accyY_only', ".csv"), row.names = FALSE)
write.csv(df_accZ, paste0(directory_to_save, '/', 'final_df_accZ_only', ".csv"), row.names = FALSE)
write.csv(df_gyroX, paste0(directory_to_save, '/', 'final_df_gyroX_only', ".csv"), row.names = FALSE)
write.csv(df_gyroY, paste0(directory_to_save, '/', 'final_df_gyroY_only', ".csv"), row.names = FALSE)
write.csv(df_gyroZ, paste0(directory_to_save, '/', 'final_df_gyroZ_only', ".csv"), row.names = FALSE)



