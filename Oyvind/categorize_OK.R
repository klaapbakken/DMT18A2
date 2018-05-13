# Setup

rm(list=ls())

load("../data/naive_preprocessed.rda")
df <- training_process_subsampled
rm(training_process_subsampled)
features <- names(df)
prop_features <- features[10:15]
prop_df <- df[, prop_features]


#zero_history <- which(df$prop_log_historical_price == 0)
#prop_df <- df[-zero_history, prop_features]
#prop_df <- unique(prop_df)

#n <- 100
#ss_list <- numeric(n)
#for (i in 1:n){
#  prop_cluster <- kmeans(prop_df, i)
#  ss_list[i] <- prop_cluster$tot.withinss
#}
#plot(seq(1,n), ss_list)
#abline(v=8)

prop_cluster <- kmeans(prop_df, 8)
prop_cluster$cluster[0:50]

library(cluster)
library('fpc')


plotcluster(prop_df[, c(2, 6)], prop_cluster$cluster)

predict.kmeans(prop_cluster, data=df[, prop_features])
