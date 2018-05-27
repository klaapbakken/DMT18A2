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

load("data/preprocessed_corrected.rda")
supp_df <- full_table
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

supp_df$srch_id <- as.numeric(supp_df$srch_id)
supp_df <- supp_df[order(supp_df$srch_id), ]

## - - - - -
## TRAIN/TEST SPLIT
## - - - - -


features <- names(df)

data_split <- 0.9

srch_ids <- df$srch_id
srch_ids.unique <- unique(srch_ids)
n_train <- floor(length(srch_ids.unique)*data_split)
n_test <- length(srch_ids.unique) - n_train

train_ids <- sample(srch_ids.unique, n_train)
test_ids <- subset(srch_ids.unique, !srch_ids.unique %in% train_ids)

train_df <- subset(df, srch_id %in% train_ids)
test_df <- subset(df, srch_id %in% test_ids)

train_supp_df <- subset(supp_df, srch_id %in% train_ids)
test_supp_df <- subset(supp_df, srch_id %in% test_ids)

## - - - - - 
## TRAINING DATA
## - - - - -

df <- train_df

pop_df <- extract_popularity(df)
wbf_df <- extract_week_book_frequency(df)

df <- apply_popularity(df, pop_df)
df <- apply_week_book_frequency(df, wbf_df)

rm_feat_by_names <- c("srch_id", "date_time", "ymd", "site_id", 
                      "visitor_location_country_id", "prop_country_id",
                      "prop_id", "position", "srch_destination_id", "week", "day",
                      "wday", "month", "visitor_hist_starrating_lm",
                      "click_bool", "booking_bool", "comp_rate_neutral",
                      "visitor_hist_adr_usd_lm", "srch_query_affinity_estimate",
                      "srch_query_affinity_score", "orig_destination_distance",
                      "srch_saturday_night_bool", "comp_rate_na", "comp_inv_na",
                      "visitor_hist_starrating", "visitor_hist_adr_usd")
rm_feat_by_indices <- c(seq(29,52), seq(67,81))
rm_feat <- c(rm_feat_by_indices, which(features %in% rm_feat_by_names))

df <- df[, -rm_feat]
numeric_cols <- unlist(lapply(df, is.numeric))
numeric_cols[23] <- FALSE
numeric_cols <- which(numeric_cols)
df[, numeric_cols] <- scale(df[, numeric_cols])

df <- cbind(df, train_supp_df[, c("group_size_norm", "family_factor", "center_dist")])
df <- df[!df$price_usd %in% boxplot.stats(df$price_usd)$out,]

gc()

formula <- as.formula(relevance ~ (.)
                      + price_usd*prop_starrating +
                        prop_location_score1*price_usd +
                        price_usd*prop_log_historical_price +
                        family_factor*prop_starrating +
                        lowest_price*popularity + 
                        only_avail*popularity)
logreg <- glm(formula, data=df, family=binomial)

## - - - - - 
## TEST DATA
## - - - - -

df <- test_df


df <- apply_popularity(df, pop_df)
df <- apply_week_book_frequency(df, wbf_df)

df$booking_bool <- df$booking_bool*5
df$relevance <- apply(df[, c("click_bool", "booking_bool")], 1, max)

rm_feat_by_names <- c("date_time", "ymd", "site_id", 
                      "visitor_location_country_id", "prop_country_id",
                      "position", "srch_destination_id", "week", "day",
                      "wday", "month", "visitor_hist_starrating_lm",
                      "click_bool", "booking_bool", "comp_rate_neutral",
                      "visitor_hist_adr_usd_lm", "srch_query_affinity_estimate",
                      "srch_query_affinity_score", "orig_destination_distance",
                      "srch_saturday_night_bool", "comp_rate_na", "comp_inv_na",
                      "visitor_hist_starrating", "visitor_hist_adr_usd")
rm_feat_by_indices <- c(seq(29,52), seq(67,81))
rm_feat <- c(rm_feat_by_indices, which(features %in% rm_feat_by_names))

df <- df[, -rm_feat]

numeric_cols <- unlist(lapply(df, is.numeric))
numeric_cols[c(1, 4, 25)] <- FALSE
numeric_cols <- which(numeric_cols)
df[, numeric_cols] <- scale(df[, numeric_cols])
df <- cbind(df, test_supp_df[, c("group_size", "group_size_norm", "family_factor", "center_dist")])

df$predicted_relevance <- predict(logreg, newdata = df, type="response")

evaluate(df, save_ranking=FALSE)
