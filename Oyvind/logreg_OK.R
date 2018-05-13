rm(list=ls())

load("../data/naive_preprocessed.rda")
df <- training_process_subsampled
rm(training_process_subsampled)

iterated_logreg_reduction <- function(df){
  feat <- names(df)
  p_s_feat <- feat
  s_feat <- NULL
  s_i <- NULL
  s_df <- df
  while (!identical(p_s_feat, s_feat)){
    logreg <- glm(booking_bool ~ (.), data=s_df, family=binomial)
    p_s_feat <- s_feat
    s_feat <- c("booking_bool", names(summary(logreg)$coef[,4][summary(logreg)$coef[,4] < 0.05]))
    s_i <- which(feat %in% s_feat)
    s_df <- data.frame(df[, s_i, drop=FALSE])
  }
  return(logreg)
}

logreg <- iterated_logreg_reduction(df)

summary <- summary(logreg)
Rsq <- 1 - summary$deviance/summary$null.deviance
