test = FALSE

features <- names(df)

na_count <- colSums(is.na(df))
no_na_features <- names(na_count[na_count == 0])
no_na_indices <- which(features %in% no_na_features)

no_na_df <- df[, no_na_indices]

if (test){
  srch_ids <- no_na_df$srch_id
  srch_ids.unique <- unique(srch_ids)
  no_na_df <- subset(no_na_df, srch_id %in% srch_ids.unique[1:100])
}

iterated_logreg_reduction <- function(df){
  feat <- names(df)
  p_s_feat <- feat
  s_feat <- NULL
  s_i <- NULL
  s_df <- df
  while (!identical(p_s_feat, s_feat)){
    logreg <- glm(booking_bool ~ (.),  data=s_df, family=binomial)
    p_s_feat <- s_feat
    s_feat <- c("booking_bool", names(summary(logreg)$coef[,4][summary(logreg)$coef[,4] < 0.05]))
    s_i <- which(feat %in% s_feat)
    s_df <- df[, s_i]
  }
  return(logreg)
}

logreg <- iterated_logreg_reduction(no_na_df)

summary <- summary(logreg)
Rsq <- 1 - summary$deviance/summary$null.deviance
