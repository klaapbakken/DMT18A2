rm(list=ls())

load("../data/naive_preprocessed.rda")
df <- training_process_subsampled
rm(training_process_subsampled)

#Clustering

features <- names(df)
prop_features <- features[10:15]
prop_df <- df[, prop_features]
prop_cluster <- kmeans(prop_df, 8)
prop_category <- data.frame(prop_category = as.factor(prop_cluster$cluster))
df <- cbind(df, prop_category)

#Balancing

booked_i <- which(df$booking_bool == TRUE)
booked_srch_ids <- unique(df$srch_id[booked_i])
df_booked <- df[booked_i, ]

clicked_i <- which(df$click_bool == TRUE)
clicked_srch_ids <- unique(df$srch_id[clicked_i])
df_clicked <- df[clicked_i, ]

clicked_booked_i <- c(booked_i, clicked_i)
ncb_df <- df[-clicked_booked_i, ]

n <- length(clicked_i)
m <- length(booked_i)

features <- names(df)

click_df <- rbind(df_clicked, dplyr::sample_n(ncb_df, n))
click_remove <- c(seq(1,4), seq(29,52), seq(55,58), 5, 8, 9, 19, 54)
click_df <- click_df[, -click_remove]

booked_df <- rbind(df_booked, dplyr::sample_n(ncb_df, m))
booked_remove <- c(seq(1,4), seq(29,52), seq(55,58), 5, 8, 9, 19, 53)
booked_df <- booked_df[, -booked_remove]

altered_click_df <- click_df
altered_click_df$click_bool <- 1/5
combined_df <- rbind(booked_df, altered_click_df)

# Logistic regression

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

#click_logreg <- glm(click_bool ~ (.), data=click_df, family=binomial)
#book_logreg <- glm(booking_bool ~ (.), data=booked_df, family=binomial)

# book_logreg_iter <- iterated_logreg_reduction(booked_df)

#corrplot(cor(data.matrix(df)), method='color')
