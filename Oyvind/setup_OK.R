rm(list=ls())

install.packages('dplyr')
install.packages('plyr')
install.packages('corrplot')

require(readr)
require(dplyr)
require(plyr)
require(corrplot)

df <- read_csv(paste("C:/Users/Øyvind Klåpbakken/Downloads/Data Mining VU data/",
                 "Data Mining VU data/training_set_VU_DM_2014.csv",sep=''),na=c("NULL"))
df$comp1_rate <- as.integer(df$comp1_rate)
df$comp1_inv <- as.integer(df$comp1_inv)
df$comp1_rate_percent_diff <- as.integer(df$comp1_rate_percent_diff)
