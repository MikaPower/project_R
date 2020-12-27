# Title     : TODO
# Objective : TODO
# Created by: Nuno
# Created on: 11/17/2020
library(ggplot2)
library(forecast)
library(wordcloud2)
library(RColorBrewer)
library(dplyr)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)
library(treemap)
library(webshot)
#webshot::install_phantomjs()

################################################
##  Set working directory
##  Load data
##  Create time series
###########################################################
#mac path
#path <-'/Users/nuno/IdeaProjects/project_R/'
path <- "C:/Users/Nuno/IdeaProjects/project_R/"
setwd(paste(path, "data/", sep = ""))

gt <- read.csv("indian_food.csv", header = TRUE)

summary(gt)


#fix errors
subset_indices = (gt[, "state"] == -1 | gt[, "state"] == '')
gt$state[subset_indices] <- 'Unknow'

subset_indices <- (gt[, "region"] == -1 | gt[, "region"] == "")
gt$region[subset_indices] <- 'Unknow'

#Fix timing errors
subset_indices = (gt[, "prep_time"] == -1)
gt$prep_time[subset_indices] <- 0

subset_indices = (gt[, "cook_time"] == -1)
gt$prep_time[subset_indices] <- 0

# Create data


# Calculate histogram, but do not draw it
my_hist = hist(gt$prep_time, breaks = 50, plot = F)

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
  group = names,
  value = values
)


# Compute percentages
data$fraction <- data$value / sum(data$value)

# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n = -1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
data$label <- paste0(data$group, "\n value: ", data$value)

# Make the plot
ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = group)) +
  geom_rect() +
  geom_label(x = 3.5, aes(y = labelPosition, label = label), size = 6) +
  scale_fill_brewer(palette = 8) +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")


  #Comidas mais pedidas
  par(mfrow = c(1, 1))


coul <- brewer.pal(5, "Set2")
barplot(table(gt$course), col = coul, ylim = c(0, max(table(gt$course)) + 30),
        xlab = "Tipo de comida", ylab = "Frequência", main = "Tipo de comida relacionadas com a sua frequencia",
        names = row.names(table(gt$course)))


barplot(table(gt$course), col = coul)

box()


#Vegans e nao vegans por regiao
# eliminar outras variaveis

agg <- gt[, c(-1, -2, -4, -5, -6, -7, -8)]

test <- data.frame(table(agg$diet, agg$region))
colnames(test) <- c('Type', 'Estado', 'Freq')

# Barplot vertical

ggplot(test, aes(x = Estado, y = Freq, fill = Type)) +
  xlab('State') +
  geom_bar(stat = "identity")


#Tipo de comidas por regiao
myPalette <- brewer.pal(7, "Set2")
pie(table(gt$region), border = "white", col = myPalette, main = "Pratos por região")

ingridients <- gt$ingredients
ingridients <- unlist(strsplit(ingridients, ","))


pie(table(ingridients), border = "white", col = myPalette)

ing_data <- data.frame(table(ingridients))

##	Extract data from a spatial subset:  -17.75 deg. S, and 23.25 deg. E:
subset_indices <- (ing_data[, "Freq"] > 10)
subset <- ing_data[subset_indices,]


# treemap
treemap(subset,
        index = "ingridients",
        vSize = "Freq",
        type = "index",
)

#pie(subset$Freq, labels = subset$ingridients, border = "white", col = myPalette)


my_words <- wordcloud2(data = ing_data, size = 1.6)
library("htmlwidgets")
saveWidget(my_words, "tmp.html", selfcontained = F)

# and in png or pdf
webshot("tmp.html", "graph_images/word_that.png", delay = 5, vwidth = 600, vheight = 600)

#Meals que demoram mais tempo
meals_prep <- gt[, c(1, 4)]

meals_prep_avg <- aggregate(meals_prep, by = list(meals_prep$name), FUN = mean)
meals_prep_avg <- meals_prep_avg[order(meals_prep_avg$prep_time),]
meals_prep_avg <- tail(meals_prep_avg, 10)

meals_prep_avg$Group.1 <- factor(meals_prep_avg$Group.1, levels = meals_prep_avg$Group.1[order(meals_prep_avg$prep_time)])

ggplot(meals_prep_avg, aes(x = meals_prep_avg$Group.1, y = meals_prep_avg$prep_time, fill = meals_prep_avg$prep_time), xlib = 'teste', ylib = 'Frequencia') +
  xlab('Nome do prato') +
  ylab('Tempo (m)') +
  geom_bar(stat = "identity") +
  labs(fill = "Tempo (m)")

meals_cook <- gt[, c(1, 5)]
meals_cook <- aggregate(meals_cook, by = list(meals_cook$name), FUN = mean)
meals_cook <- meals_cook[order(meals_cook$cook_time),]
meals_cook <- tail(meals_cook, 10)

meals_cook$Group.1 <- factor(meals_cook$Group.1, levels = meals_cook$Group.1[order(meals_cook$cook_time)])

ggplot(meals_cook, aes(x = Group.1, y = cook_time, fill = cook_time), xlib = 'teste', ylib = 'Frequencia') +
  xlab('Nome do prato') +
  ylab('Tempo (m)') +
  geom_bar(stat = "identity") +
  labs(fill = "Tempo a ser cozinhado (m)")+
geom_bar(stat = "identity")

#distribuição de cooking time por regiao
cook_region <- gt[, c(5, 9)]
ggplot(cook_region, aes(x = cook_time, group = region, fill = region)) +
  geom_density(adjust = 1.5) +
  labs(fill = "Região") +
  theme_ipsum()


#distribuição de prep time por regiao
prep_region <- gt[, c(4, 9)]
ggplot(prep_region, aes(x = prep_time, group = region, fill = region)) +
  xlab("Tempo a ser cozinhado (m)") +
  geom_density(adjust = 1.5) +
  theme_ipsum()

#Numero de pratos por regiao
plates_region_hist <- data.frame(table(gt$region))
plates_region_hist$Var1 <- factor(plates_region_hist$Var1, levels = plates_region_hist$Var1[order(plates_region_hist$Freq)])

ggplot(plates_region_hist, aes(x = Var1, y = Freq, fill = Var1)) +
  scale_fill_brewer(palette = "Set4") +
  geom_bar(stat = "identity") +
  ggtitle("Distribuição de pratos por região") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Região") +
  ylab("Numero de pratos") +
  theme(legend.position = "none")

#Top 10 snacks/courses,dessserts with longest cooking time
##  Set the graphics device to plot two figures in a 1 row by 2 column configuration
library(ggpubr)
theme_set(theme_pubr())

snaks_longest_cook <- gt[, c(1, 5)]
subset_index <- (gt[, "course"] == 'snack')
snaks_longest_cook <- snaks_longest_cook[subset_index,]

snaks_longest_cook <- snaks_longest_cook[order(snaks_longest_cook$cook_time),]
snaks_longest_cook <- tail(snaks_longest_cook, 10)

snaks_longest_cook$name <- factor(snaks_longest_cook$name, levels = snaks_longest_cook$name[order(snaks_longest_cook$cook_time)])


plot_snaks <- ggplot(snaks_longest_cook, aes(x = name, y = cook_time, fill = name)) +
  scale_fill_brewer(palette = (1)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Nomes do prato") +
  ylab("Tempo a ser cozinhado") +
  theme(legend.position = "none")

#dessert
dessert_longest_cook <- gt[, c(1, 5)]
subset_index <- (gt[, "course"] == 'dessert')
dessert_longest_cook <- dessert_longest_cook[subset_index,]

dessert_longest_cook <- dessert_longest_cook[order(dessert_longest_cook$cook_time),]
dessert_longest_cook <- tail(dessert_longest_cook, 10)

dessert_longest_cook$name <- factor(dessert_longest_cook$name, levels = dessert_longest_cook$name[order(dessert_longest_cook$cook_time)])


plot_dessert <- ggplot(dessert_longest_cook, aes(x = name, y = cook_time, fill = name)) +
  scale_fill_brewer(palette = (2)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Nomes do prato") +
  ylab("Tempo a ser cozinhado") +
  theme(legend.position = "none")

#main
main_longest_cook <- gt[, c(1, 5)]
subset_index <- (gt[, "course"] == 'main course')
main_longest_cook <- main_longest_cook[subset_index,]

main_longest_cook <- main_longest_cook[order(main_longest_cook$cook_time),]
main_longest_cook <- tail(main_longest_cook, 10)

main_longest_cook$name <- factor(main_longest_cook$name, levels = main_longest_cook$name[order(main_longest_cook$cook_time)])


plot_main <- ggplot(main_longest_cook, aes(x = name, y = cook_time, fill = name)) +
  scale_fill_brewer(palette = (3)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Nomes do prato") +
  ylab("Tempo a ser cozinhado") +
  theme(legend.position = "none")


#starter
starter_longest_cook <- gt[, c(1, 5)]
subset_index <- (gt[, "course"] == 'starter')
starter_longest_cook <- starter_longest_cook[subset_index,]

starter_longest_cook <- starter_longest_cook[order(starter_longest_cook$cook_time),]
starter_longest_cook <- tail(starter_longest_cook, 10)

starter_longest_cook$name <- factor(starter_longest_cook$name, levels = starter_longest_cook$name[order(starter_longest_cook$cook_time)])


plot_start <- ggplot(starter_longest_cook, aes(x = name, y = cook_time, fill = name)) +
  scale_fill_brewer(palette = (4)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Nomes do prato") +
  ylab("Tempo a ser cozinhado") +
  theme(legend.position = "none")


figure <- ggarrange(plot_dessert, plot_main, plot_snaks, plot_start,
                    labels = c("Snaks", "Dessert", "Main Course", "Starter"),
                    ncol = 2, nrow = 2)
figure


#scatter_plot gr course
scatter_course <- gt[, c(4, 5, 7)]

ggplot(scatter_course, aes(x = prep_time, y = cook_time, color = course, main = 'Relação entre tempo de cozedura e tempo de preparação')) +
  geom_point(size = 6) +
  xlab("Tempo de preparação (m)") +
  ylab("Tempo de cozedura (m)") +
  labs(fill = "Tempo (m)") +
  theme_ipsum()


snaks_median <- median(snaks_longest_cook$cook_time)
dessert_median <- median(dessert_longest_cook$cook_time)
main_course_median <- median(main_longest_cook$cook_time)
start_median <- median(starter_longest_cook$cook_time)


type_course <- data.frame(names = c("Snaks","Dessert","Main Course","Starter"),
                          values = c(snaks_median,dessert_median,main_course_median,start_median))

type_course$names <- factor(type_course$names, levels = type_course$names[order(type_course$values)])

ggplot(type_course, aes(x = names, y = values, fill = names))+ylim(c(0,100)) +
  scale_fill_brewer(palette = (4)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Nomes do prato") +
  ylab("Tempo a ser cozinhado") +
  theme(legend.position = "none")