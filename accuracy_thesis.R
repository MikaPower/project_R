# Title     : TODO
# Objective : TODO
# Created by: nuno
# Created on: 10/11/21

# load the library
library(forcats)


# Library
library(ggplot2)
library(dplyr)

# Dataset 1: one value per group
data <- data.frame(
  name = c("Athlete1", "Athlete2", "Athlete3", "Athlete4", "Athlete5"),
  val = c(0.821428571,0.814814815,0.814814815,0.785714286,0.8))
# Reorder following the value of another column:
data %>%
  mutate(name = fct_reorder(name, val)) %>%
  ggplot(aes(x = name, y = val * 100)) +
  geom_bar(stat = "identity", fill = "#f68060", alpha = .6, width = .4) +
  coord_flip() +
  xlab("Athetes") +
  ylab("Movement Accuracy") +
  theme_bw() +
  ggtitle("Application accuracy per movement") +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20))
#print()

# Reverse side
#data %>%
#  mutate(name = fct_reorder(name, desc(val))) %>%
#  ggplot( aes(x=name, y=val)) +
#  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
#  coord_flip() +
#  xlab("") +
#  theme_bw()

# library
library(ggplot2)

# create a dataset
specie <- c(rep("Top spin forehand", 2), rep("Top spin backhand", 2), rep("Block", 2), rep("Flip forehand", 2), rep("Flip backhand", 2))
condition <- rep(c("Positive Cases", "Negative Cases"), 5)
value <- c(0.857142857,0.142857143,0.72,0.28,0.785714286,0.214285714,0.793103448,0.206896552,0.884615385,0.115384615)
data <- data.frame(specie, condition, value)

# Grouped
ggplot(data, aes(fill = condition, y = value * 100, x = specie)) +
  geom_bar(position = "dodge", stat = "identity") +
  xlab("Movements Types") +
  ylab("Percentage %") +
  ggtitle("Movement Detection Positive vs Negative count ") +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(fill = "Condition type (%)")