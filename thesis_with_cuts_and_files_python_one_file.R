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

full_df <- data_frame('rotation_x' = 0, 'rotation_y' = 0, 'rotation_z' = 0, 'acceleration_x'=0, 'acceleration_y'=0, 'acceleration_z'=0, 'motionQuaternionX'=0,
                      'motionQuaternionY'=0, 'motionQuaternionZ'=0, 'motionQuaternionW'=0,'label'=1)

dir.create(paste0(path, '/full_df/'))
for (file in files) {
  next_directory <- paste("/movements", file, sep = '/')
  classes <- dir(paste0(path, next_directory, sep = ''))
  dir.create(paste0(path, '/saved/', file))
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
      limit_val <-switch(class, "bloco" = 1, "flip_direita" = 1, "flip_esquerda" = 1,"top_spin_esquerda" = 1,"top_spin_direita" = 1)
      limit_axis <- switch(class, "bloco" = 'motionUserAccelerationZ.G.', "flip_direita" = 'motionUserAccelerationX.G.', "flip_esquerda" = 'motionUserAccelerationZ.G.',"top_spin_esquerda" = 'motionUserAccelerationZ.G.',"top_spin_direita" = 'motionUserAccelerationX.G.')
      label_name <-switch(class, "bloco" = 3, "flip_direita" = 4, "flip_esquerda" = 5,"top_spin_esquerda" = 2,"top_spin_direita" = 1)
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
          label <- rep(label_name , nrow(mini_sub))
          test <- data.frame(mini_sub$motionRotationRateX.rad.s., mini_sub$motionRotationRateY.rad.s.,
                             mini_sub$motionRotationRateZ.rad.s., mini_sub$motionUserAccelerationX.G.,
                             mini_sub$motionUserAccelerationY.G., mini_sub$motionUserAccelerationZ.G.,
                             mini_sub$motionQuaternionX.R., mini_sub$motionQuaternionY.R., mini_sub$motionQuaternionZ.R.,
                             mini_sub$motionQuaternionW.R.,label)
          colnames(test) <- c('rotation_x', 'rotation_y', 'rotation_z', 'acceleration_x', 'acceleration_y', 'acceleration_z', 'motionQuaternionX',
                              'motionQuaternionY', 'motionQuaternionZ', 'motionQuaternionW','label')

          full_df <- rbind(full_df,test)
        }
        else{
          data[row,2:ncol(data)]<- 0
          row <- row+1
        }
      }
    }
  }
}

#Join path to save directory
directory_to_save <- paste(path, "full_df" , sep = "/")
full_df <- full_df[-1,]
write.csv(full_df, paste0(directory_to_save, '/', 'final_df', ".csv"), row.names = TRUE)



