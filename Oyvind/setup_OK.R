rm(list=ls())

install.packages('dplyr')
install.packages('plyr')
install.packages('corrplot')
install.packages('arules')
install.packages('tidyverse')

library(readr)
library(plyr)
library(dplyr)
library(corrplot)
library(stats)
library(arules)
library(MASS)

data_split <- 0.1

df <- read_csv("C:/Users/Bruker/Downloads/Data Mining VU data/Data Mining VU data/training_set_VU_DM_2014.csv",na=c("NULL"))
df$comp1_rate <- as.integer(df$comp1_rate)
df$comp1_inv <- as.integer(df$comp1_inv)
df$comp1_rate_percent_diff <- as.integer(df$comp1_rate_percent_diff)

srch_ids <- df$srch_id
srch_ids.unique <- unique(srch_ids)
n_train <- floor(length(srch_ids.unique)*data_split)
n_test <- length(srch_ids.unique) - n_train
  
train_ids <- sample(srch_ids.unique, n_train)
test_ids <- subset(srch_ids.unique, !srch_ids.unique %in% train_ids)

train_df <- subset(df, srch_id %in% train_ids)
identical(sort(unique(train_df$srch_id)), sort(train_ids))

test_df <- subset(df, srch_id %in% test_ids)

rm(df)
df <- train_df
rm(train_df)