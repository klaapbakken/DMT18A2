rm(list=ls())
set.seed(0)

## - - - - -
## LOAD
## - - - - -

source("ndcg.R")
source("evaluate.R")
source("popularity.R")

load("data/preprocessed.rda")
df <- full_table
rm(full_table)

## - - - - - 
## ADAPTING
## - - - - -

df$new_visitor <- as.factor(df$new_visitor)
df$lowest_price <- as.factor(df$lowest_price)
df$only_avail <- as.factor(df$only_avail)
df$srch_id <- as.numeric(df$srch_id)
df$prop_id <- as.numeric(df$prop_id)

df$booking_bool <- df$booking_bool*1
df$click_bool <- df$click_bool - df$booking_bool
df$relevance <- apply(df[, c("click_bool", "booking_bool")], 1, max)

## - - - - -
## TRAIN/TEST SPLIT
## - - - - -


features <- names(df)

data_split <- 0.90

srch_ids <- df$srch_id
srch_ids.unique <- unique(srch_ids)
n_train <- floor(length(srch_ids.unique)*data_split)
n_test <- length(srch_ids.unique) - n_train

train_ids <- sample(srch_ids.unique, n_train)
test_ids <- subset(srch_ids.unique, !srch_ids.unique %in% train_ids)

train_df <- subset(df, srch_id %in% train_ids)
test_df <- subset(df, srch_id %in% test_ids)


## - - - - - 
## TRAINING DATA
## - - - - -

df <- train_df

pop_df <- extract_popularity(df)

df <- apply_popularity(df, pop_df)

rm_feat_by_names <- c("srch_id", "date_time", "ymd", "site_id", 
                      "visitor_location_country_id", "prop_country_id",
                      "prop_id", "position", "srch_destination_id", "week", "day",
                      "wday", "month", "visitor_hist_starrating_lm",
                      "click_bool", "booking_bool", "comp_rate_neutral",
                      "visitor_hist_adr_usd_lm", "srch_query_affinity_estimate")
rm_feat_by_indices <- c(seq(29,52), seq(67,81))
rm_feat <- c(rm_feat_by_indices, which(features %in% rm_feat_by_names))

df <- df[, -rm_feat]

formula <- as.formula(relevance ~ (.) + price_usd*(.) + prop_starrating*(.) + prop_review_score*(.) + prop_brand_bool*(.) +
                                 prop_location_score1*(.) + prop_log_historical_price*(.) + 
                                 prop_location_score2*(.) + promotion_flag*(.) + popularity*new_property)
logreg <- glm(formula, data=df, family=binomial)

## - - - - - 
## TEST DATA
## - - - - -

df <- test_df

df <- apply_popularity(df, pop_df)

df$booking_bool <- df$booking_bool*5
df$relevance <- apply(df[, c("click_bool", "booking_bool")], 1, max)

rm_feat_by_names <- c("date_time", "ymd", "site_id", 
                      "visitor_location_country_id", "prop_country_id",
                      "position", "srch_destination_id", "week", "day",
                      "wday", "month", "comp_rate_neutral",
                      "click_bool", "booking_bool",
                      "visitor_hist_starrating_lm", "new_visitor_hist_adr_usd_lm",
                      "srch_query_affinity_estimate")

rm_feat_by_indices <- c(seq(29,52), seq(67,81))
rm_feat <- c(rm_feat_by_indices, which(features %in% rm_feat_by_names))

df <- df[, -rm_feat]

df$predicted_relevance <- predict(logreg, newdata = df, type="response")

evaluate(df)
