rm(list=ls())

test = FALSE
require(readr)

df <- read_csv(paste("C:/Users/Øyvind Klåpbakken/Downloads/Data Mining VU data/",
                     "Data Mining VU data/training_set_VU_DM_2014.csv",sep=''),na=c("NULL"))
df$comp1_rate <- as.integer(df$comp1_rate)
df$comp1_inv <- as.integer(df$comp1_inv)
df$comp1_rate_percent_diff <- as.integer(df$comp1_rate_percent_diff)

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
