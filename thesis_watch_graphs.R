# Title     : TODO
# Objective : TODO
# Created by: nuno
# Created on: 04/03/2021
library(ggplot2)
library(dplyr)


calculate_time_start <- function(time_string) {
  time <- strptime(time_string, "%Y-%m-%d %H:%M:%OS")
  return(as.numeric(difftime(time, first_time, units = "secs")))
  #df1$ diff_in_milliseconds= as.numeric(difftime(df1$Logout_time, df1$Login_time, units ="secs")) *1000
}

path <- '/Users/nuno/IdeaProjects/project_R/'
#path <- "C:/Users/Nuno/IdeaProjects/project_R/"
setwd(paste(path, "data/thesis/misc", sep = ""))
# Load dataset from github
dir.create('graphs')
# Number of digits for time omilliseconds
my_options <- options(digits.secs = 3)

classes <- dir('modes')
for (class in classes) {
  index_to_save <- 1
  files <- dir(paste('modes', class, sep = '/'))
  dir.create(paste('graphs', class, sep = '/'))
  for (file in files) {
    data <- read.csv(paste('modes', class, file, sep = '/'), header = TRUE, sep = '|')
    #get first timestamp record to compare vs others
    first_time <- data$loggingTime.txt.[1]
    data$loggingTime.txt. <- sapply(data$loggingTime.txt., calculate_time_start)

    #cut x seconds front and back
    data <- data[data$loggingTime.txt. > min(data$loggingTime.txt. + 4),]
    data <- data[data$loggingTime.txt. < max(data$loggingTime.txt.) - 4,]


    calculate_graph <- function(table, names, title_graph, title_dir, sort = TRUE) {
      if (sort) {
        column_names <- sort(rep(names, nrow(data)))
      }
      else {
        column_names <- names
      }

      final <- data.frame(x = data$loggingTime.txt., val = table,
                          variable = column_names)


      g <- ggplot(data = final, aes(x = x, y = val)) +
        geom_line(aes(colour = variable)) +
        ggtitle(paste(title_graph, class, file, sep = " "))#+xlim(c(4,10))
      show(g)

      directory_to_save <- paste("graphs", class, title_dir, sep = "/")
      dir.create(directory_to_save)
      ggsave(filename = paste(title_dir, "-", class, "-", file, ".jpeg", sep = ""), path = paste0(directory_to_save), g, device = "jpeg")

    }

    #raw accelarometer
    data_1 <- as.vector(as.matrix(data[, c(3, 4, 5)]))
    names <- colnames(data[3:5])
    calculate_graph(data_1, names, 'Raw Acce Data', 'Acelarometro_raw')

    #Core motion acc
    data_1 <- as.vector(as.matrix(data[, c(13, 14, 15)]))
    names <- colnames(data[13:15])
    calculate_graph(data_1, names, 'Core Motion Acc Data', 'acelarometro')


    #Motion gyroscope
    data_1 <- as.vector(as.matrix(select(data, -1:-9, -13:-ncol(data))))
    names <- colnames(data[10:12])
    calculate_graph(data_1, names, 'Motion rotation gyroscope', 'giroscopio')


    #Motion Yaw,Roll,Pitch
    motion_yaw_roll_pitch <- select(data, -1:-6, -10:-ncol(data))

    data_1 <- as.vector(as.matrix(motion_yaw_roll_pitch))
    names <- colnames(data[7:9])
    motionYaw.rad. <- rep(names[1], nrow(motion_yaw_roll_pitch))
    motionRoll.rad. <- rep(names[2], nrow(motion_yaw_roll_pitch))
    motionPitch.rad. <- rep(names[3], nrow(motion_yaw_roll_pitch))
    names <- c(motionYaw.rad., motionRoll.rad., motionPitch.rad.)
    calculate_graph(data_1, names, 'Yaw Pitch Roll', 'giroscopio', FALSE)


    #Motion Rotation
    motion_quaternion <- select(data, -1:-16, -21:-ncol(data))
    data_1 <- as.vector(as.matrix(motion_quaternion))
    names <- colnames(data[17:20])
    calculate_graph(data_1, names, 'Motion Quaternion', 'quartz')
  }
}


calculate_time_start <- function(time_string) {
  time <- strptime(time_string, "%Y-%m-%d %H:%M:%OS")
  return(as.numeric(difftime(time, first_time, units = "secs")))
  #df1$ diff_in_milliseconds= as.numeric(difftime(df1$Logout_time, df1$Login_time, units ="secs")) *1000
}

#https://ibb.co/album/bHrqCV
#https://ibb.co/album/ZSkn1X