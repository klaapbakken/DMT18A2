rm(list=ls())

install.packages('dplyr')
install.packages('plyr')
install.packages('corrplot')

require(readr)
require(dplyr)
require(plyr)
require(corrplot)

df <- read_csv("C:/Users/Øyvind Klåpbakken/Downloads/Data Mining VU data/Data Mining VU data/training_set_VU_DM_2014.csv")
