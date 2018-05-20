rm(list=ls())

load("../data/naive_preprocessed.rda")
df <- training_process_subsampled
remove <- c(seq(1,4), seq(29,52), seq(55,58), 9)
df <- df[, -remove]
rm(training_process_subsampled)

summary(df)

features <- variable.names(df)


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

ranked_corr <- correlations_ranked(20, corr)

