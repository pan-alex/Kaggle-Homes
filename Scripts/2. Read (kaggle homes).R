# Dependencies: None

rm(list = ls())
library(dplyr)
library(ggplot2)
library(Amelia)
library(mice)
library(lattice)

# 1. Read in the Data

train.data <- read.csv("../Data/kaggle.homes-train.csv")

test.data <- read.csv("../Data/kaggle.homes-test.csv")

# Merge the test data and training data. As long as no observations are added or removed, and the data frame is not re-ordered, there will be 1460 observations in the training set and 1459 in the test set.
# Just for certainty, I'll annotate each data set.
temp <- mutate(train.data, Dataset = "train")
temp2 <- mutate(test.data, Dataset = "test", SalePrice = NA)
full.data <- rbind(temp, temp2)
