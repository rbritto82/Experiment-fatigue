#Script to prepare and analyze data from the fatigue experiment. Note that this script calls to other scripts. 
#loading required libraries
library(xlsx)
library(dplyr)
library(epiR)
library(ggplot2)
#loading raw data
rawData <- tbl_df(read.xlsx("./Data/data_17012017.xlsx", sheetIndex=1, stringsAsFactors = FALSE))
rawData <- rawData[-c(1)] #removing timestemp column 
rawData <- rawData[-c(1, 4, 5, 16),]


#remove people without fatigue
rawData = rawData[-c(3:5, 7, 10, 11),]

#loading benchmark data
benchmark <- tbl_df(read.xlsx("./Data/level_1_cross_validation.xlsx", sheetIndex=7, colIndex = 6))

#creating the new data frame 
names <- vector()
for (i in 1:dim(rawData)[1]) {
  temp <- rep(rawData[[1]][i], dim(benchmark)[1])
  names <- c(names, temp)
}
tidyData <- data.frame(name = names, result = rep(TRUE, length(names)),  benchmark = rep(benchmark$Decision, dim(rawData)[1]))

source("./Analysis/effectSize.R")
source("./Analysis/prepare_data.R")
source("./Analysis/analysis.R")
