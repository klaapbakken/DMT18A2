load("processed_missing_value.rda")



library(corrplot)
library(tidyr)
library(dplyr)



corrplot(as.matrix(final.table[,sapply(final.table,is.numeric)]),is.corr = FALSE, method = "square")

