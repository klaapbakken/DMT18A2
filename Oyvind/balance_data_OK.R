rm(list=ls())

load("../data/naive_preprocessed.rda")
df <- training_process_subsampled
rm(training_process_subsampled)

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
click_remove <- c(seq(1,5), seq(29,52), seq(55,58), 8, 9, 19, 54)
click_df <- click_df[, -click_remove]

booked_df <- rbind(df_booked, dplyr::sample_n(ncb_df, m))
booked_remove <- c(seq(1,5), seq(29,52), seq(55,58), 8, 9, 19, 53)
booked_df <- booked_df[, -booked_remove]

# Logistic regression

click_logreg <- glm(click_bool ~ (.), data=click_df, family=binomial)

