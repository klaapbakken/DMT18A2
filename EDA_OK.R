rm(list=ls())

summary(df)

features <- variable.names(df)
comp_features <- features[28:51]

srch_ids <- df$srch_id
srch_ids.unique <- unique(srch_ids)


srch_df <- subset(df, srch_ids == srch_ids.unique[1])




ltd_df <- subset(df, srch_id %in% srch_ids.unique[1:100])
red_ltd_df <- ltd_df[, which(!(features %in% comp_features))]
red_ltd_matrix <- data.matrix(red_ltd_df)
red_ltd_matrix[is.nan(red_ltd_matrix)] <- 0
corr <- cor(red_ltd_matrix)
corrplot(corr, method="color")
logreg <- glm(booking_bool ~ (.),  data=limited_df)


correlations_ranked <- function(n, corr){
  ranking <- list(n)
  max = 1
  for (i in 1:n){
    ranking[[i]] <- c(rownames(which(corr == max(corr[is.finite(corr) & corr < max]), arr.ind=T)),
                      max <- max(corr[is.finite(corr) & corr < max]))
    max <- max(corr[is.finite(corr) & corr < max])
  }
return(ranking)
}

ranked_corr <- correlations_ranked(10, corr)
