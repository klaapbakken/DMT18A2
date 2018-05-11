rm(list = ls())
load("../data/training_processed.rda")
features <- names(training)
df <- training
df$comp1_rate <- as.integer(df$comp1_rate)
df$comp1_inv <- as.integer(df$comp1_inv)
df$comp1_rate_percent_diff <- as.integer(df$comp1_rate_percent_diff)
rm(training)

hotel_attributes <- c(features[8], features[10:15], features[17:18])
hotel_df <- na.omit(df[, hotel_attributes])
cluster <- kmeans(hotel_df, 5)
