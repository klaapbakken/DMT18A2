

summary(df)

features <- variable.names(df)
comp_features <- features[28:51]


na_count <- colSums(is.na(df))
no_na_features <- names(na_count[na_count == 0])
no_na_indices <- which(features %in% no_na_features)

priority_indices <- c(no_na_indices)

no_na_df <- df[, priority_indices]
no_na_matrix <- data.matrix(no_na_df)
corr <- cor(no_na_matrix)
corrplot(corr, method="color")

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

srch_ids <- df$srch_id
srch_ids.unique <- unique(srch_ids)
srch_df <- subset(df, srch_ids == srch_ids.unique[1])

