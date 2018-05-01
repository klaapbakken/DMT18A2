rm(list=ls())

summary(df)

features <- variable.names(df)
comp_features <- features[28:51]

srch_ids <- df$srch_id
srch_ids.unique <- unique(srch_ids)


srch_df <- subset(df, srch_ids == srch_ids.unique[1])

na_count <- colSums(is.na(df))
no_na_features <- names(na_count[na_count == 0])
no_na_indices <- which(features %in% no_na_features)

priority_indices <- c(no_na_indices)

no_na_df <- df[, priority_indices]
no_na_matrix <- data.matrix(no_na_df)
corr <- cor(no_na_matrix)
logreg <- glm(booking_bool ~ (.),  data=no_na_df, family=binomial)

significant_features <- c("booking_bool", names(summary(logreg)$coefficients[summary(logreg)$coefficients[,4] < 0.05, 4]))
significant_indices <- which(features %in% significant_features)
sign_df <- df[, significant_indices]

red_logreg <- glm(booking_bool ~ (.), data=sign_df, family=binomial)



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


