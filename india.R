# Title     : TODO
# Objective : TODO
# Created by: Nuno
# Created on: 11/17/2020
library(ggplot2)

library(forecast)






###########################################################
##  Set working directory
##  Load data
##  Create time series
###########################################################
#mac path
path <-'/Users/nuno/IdeaProjects/project_R/'
#path <- "C:/Users/Nuno/IdeaProjects/project_R/"
setwd(paste(path, "data/", sep = ""))

gt <- read.csv("indian_food.csv",header = TRUE)

summary(gt)

# Create data


# Calculate histogram, but do not draw it
my_hist=hist(gt$prep_time, breaks = 20,plot=F)

# Color vector
my_color= ifelse(gt$prep_time < 10, rgb(0.2,0.8,0.5,0.5) , ifelse (gt$prep_time >=10, "purple", rgb(0.2,0.2,0.2,0.2) ))

# Final plot
plot(my_hist , border=F , main="" , col = my_color, xlab="Tempo para cozinhar (minutos)", ylab = 'Frequencia',, xlim=c(-20,500) )
title(main = "Frequencias tempo de cozedura")



#Pratos vegetarios vs não vegetarianos

my_table <- table(gt$diet)


names <- row.names(my_table)
values <- as.vector(my_table)
# Create Data
data <- data.frame(
  group=names,
  value=values
)


# Compute percentages
data$fraction <- data$value / sum(data$value)

# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
data$label <- paste0(data$group, "\n value: ", data$value)

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=group)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
  scale_fill_brewer(palette=8) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")


#Comidas mais pedidas
par(mfrow=c(1,1))

library(RColorBrewer)
coul <- brewer.pal(5, "Set2")
barplot(table(gt$course), col = coul, ylim = c(0,max(table(gt$course))+30),
        xlab = "tipos", ylab = "Proportion", main = "Tipo de comida",
        names = row.names(table(gt$course)))


barplot(table(gt$course), col=coul)

box()


#Vegans e nao vegans por regiao
# eliminar outras variaveis

agg <-gt[,c(-1,-2,-4,-5,-6,-7,-8)]

test <- data.frame(table(agg$diet,agg$region))
#colnames(test) <- c('tipo de comeder','cidade','freq')

# Barplot vertical

ggplot(test, aes(x=Var2, y=Freq, fill = Var1), xlib = 'teste' ,ylib = 'Frequencia') +
  geom_bar(stat = "identity")
