##############################################################################
# Title: Coding Club Workshop: Linear Modeling Challenge
# Description: Using the ToothGrowth built-in dataset describing tooth growth
# in guinea pigs under different vitamin C treatments, answer the following questions:
# 1. Are higher doses of vitamin C beneficial for tooth growth?
# 2. Does the method of administration (orange juice, OJ, or ascorbic acid, VS)
# influence the effect of the dose?
# 3. What would be the predicted tooth length of a guinea pig given 1 mg of 
# vitamin C as ascorbic acid?
# Author: Molly Johnson, Bradley University/Oregon State University
# Date: 7/24/20
##############################################################################

# Libraries ----
library(dplyr)
library(tidyr)
library(ggplot2)

# Functions ----
theme.clean <- function(){
  theme_bw()+
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "plain"),             
          axis.title.y = element_text(size = 14, face = "plain"),             
          panel.grid.major.x = element_blank(),                                          
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 12, face = "italic"),          
          legend.position = "right")
}

# load the dataset ----
ToothGrowth <- datasets::ToothGrowth
head(ToothGrowth)  # first few observations
tail(ToothGrowth)  # last few observations
str(ToothGrowth)  # types of variables
summary(ToothGrowth)  # summary of ToothGrowth dataset

# Analysis ----
# convert dose into a categorical variable
ToothGrowth$dose <- as.factor(ToothGrowth$dose)

head(ToothGrowth)  # first few observations
tail(ToothGrowth)  # last few observations
str(ToothGrowth)  # types of variables
summary(ToothGrowth)  # summary of ToothGrowth dataset

# run a model (ANOVA) using two interacting terms
tooth.m <- lm(len ~ dose*supp, data = ToothGrowth)
summary(tooth.m)

# Question answers
# The model is highly significant. together, dose and method explain
# around 77% of the variation in tooth growth (see adjusted R^2)

# 1. Are higher doses of vitamin C beneficial for tooth growth?
# -higher doses of vitamin C do promote tooth growth, but (see answer to question 2)

# 2. Does the method of administration (orange juice, OJ, or ascorbic acid, VS)
# influence the effect of the dose?
# the effect of dose on growth depends on the administration method

# 3. What would be the predicted tooth length of a guinea pig given 1 mg of 
# vitamin C as ascorbic acid?
# A guinea pig given 1 mg a day as ascorbic acid would be predicted to have a
# tooth growth of:
# 13.23 (growth for dose 0.5, orange juice)
#   9.47 (extra growth for 1.0 orange juice)
#   -5.25 (difference in growth linked to the ascorbic acid treatment for dose 0.5)
#   -0.68 (difference in growth for the interaction between dose 1.0 
#    and ascorbic acid treatment) = 16.77

# visualize the differences with a box plot:
ggplot(ToothGrowth, aes(x = dose, y = len))+
  geom_boxplot(aes(colour = supp)) +
  theme.clean()