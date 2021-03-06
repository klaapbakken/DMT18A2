rm(list = ls())
# Pre-processing function for Expedia data

# Load required libraries
library(tidyverse)
library(purrrlyr)
library(MASS)

# Function to return preprocessed dataframe
preprocess_data = function(input_data){
  
  # - - - - - - - - - - - - - - - 
  # Fix competitor missing values
  # - - - - - - - - - - - - - - -
  message("Working on competitor features...")
  # Count the number of NaN/-1/+1 values across compX_rate
  comp_rate_missing = input_data %>% 
    dplyr::select(ends_with("rate")) %>% 
    by_row(
      ..f = function(x) {
        sum(is.na(x[1:8]))
      },
      .to = "comp_rate_na",
      .collate = "cols"
    ) %>% 
    by_row(
      ..f = function(x) {
        sum(x[1:8] == 1, na.rm = T)
      },
      .to = "comp_rate_positive",
      .collate = "cols"
    ) %>% 
    by_row(
      ..f = function(x) {
        sum(x[1:8] == -1, na.rm = T)
      },
      .to = "comp_rate_negative",
      .collate = "cols"
    ) %>% 
    by_row(
      ..f = function(x) {
        sum(x[1:8] == 0, na.rm = T)
      },
      .to = "comp_rate_neutral",
      .collate = "cols"
    ) %>% 
    dplyr::select(starts_with("comp_rate"))
  
  # Count the number of NaN/0/+1 values across compX_inv
  comp_inv_missing = input_data %>% 
    dplyr::select(ends_with("inv")) %>% 
    by_row(
      ..f = function(x) {
        sum(is.na(x[1:8]))
      },
      .to = "comp_inv_na",
      .collate = "cols"
    ) %>% 
    by_row(
      ..f = function(x) {
        sum(x[1:8] == 1, na.rm = T)
      },
      .to = "comp_inv_positive",
      .collate = "cols"
    ) %>% 
    by_row(
      ..f = function(x) {
        sum(x[1:8] == 0, na.rm = T)
      },
      .to = "comp_inv_neutral",
      .collate = "cols"
    ) %>% 
    dplyr::select(starts_with("comp_inv"))
  
  # Count the number of NaN/mean(abs) values across compX_rate_percent
  comp_rate_percent_missing = input_data %>% 
    dplyr::select(ends_with("rate_percent_diff")) %>% 
    by_row(
      ..f = function(x) {
        sum(is.na(x[1:8]))
      },
      .to = "comp_rate_percent_diff_na",
      .collate = "cols"
    ) %>% 
    dplyr::select(starts_with("comp_rate_percent"))
  
  # Cbind the existing data
  input_data = input_data %>% 
    #dplyr::select(-ends_with("inv")) %>% 
    #dplyr::select(-ends_with("rate")) %>% 
    #dplyr::select(-ends_with("rate_percent_diff")) %>% 
    cbind(comp_rate_missing) %>% 
    cbind(comp_inv_missing) %>% 
    cbind(comp_rate_percent_missing)
  
  # Message
  message("Finished working on competitor features...")
  
  
  # - - - - - - - - - - - - - - - 
  # Fix origin distance values
  # - - - - - - - - - - - - - - -
  # Replace NA values with the median/mean distances across the origin
  message("Working on origin distance feature...")
  input_data = input_data %>% 
    replace_na(list(orig_destination_distance = median(.$orig_destination_distance, na.rm = T)))
  
  
  # - - - - - - - - - - - - - - - 
  # Fix location score #2 values
  # - - - - - - - - - - - - - - -
  # Set the location score #2 to the minimum value across all scores
  message("Working on location score #2 feature...")
  input_data = input_data %>% 
    group_by(srch_destination_id) %>% 
    replace_na(list(prop_location_score2 = min(.$prop_location_score2, na.rm = T))) %>% 
    ungroup()
  
  # - - - - - - - - - - - - - - - 
  # Setup
  # - - - - - - - - - - - 
  
  features <-names(input_data)
  na_count <- colSums(is.na(input_data))
  na_features <- names(na_count[na_count > 0])
  na_indices <- which(features %in% na_features)
  na_df <- input_data[, na_indices]
  
  # - - - - - - - - - - - - - - - 
  # Fix visitor values
  # - - - - - - - - - - - - - - -
  message("Working on visitor values features...")
  # Flag new customers, impute by sampling from appropriate distribution
  
  visitor_indices <- which(na_features %in% features[5:7])
  visitor_features <- names(na_df[, visitor_indices])
  visitor_df <- na_df[, visitor_indices]
  
  all_na <- function(x) all(is.na(x))
  new_visitor <- apply(visitor_df[, visitor_indices], 1, all_na)
  
  cutoff <- function(x){
    lower <- which(x < 0)
    higher <- which(x > 5)
    ret_x <- x
    ret_x[lower] <- 0
    ret_x[higher] <- 5
    return (ret_x)
  }
  
  visitor_stars <- visitor_df$visitor_hist_starrating[which(!is.na(visitor_df$visitor_hist_starrating))]
  stars_fit <- fitdistr(visitor_stars, 'normal')
  norm_para <- stars_fit$estimate
  
  visitor_stars_complete <- visitor_df$visitor_hist_starrating
  vsc_nans <- sum(is.na(visitor_stars_complete))
  star_samples <- rnorm(vsc_nans, norm_para[1], norm_para[2])
  visitor_stars_complete[is.na(visitor_stars_complete)] <- cutoff(star_samples)
  
  visitor_usd <- visitor_df$visitor_hist_adr_usd[which(!is.na(visitor_df$visitor_hist_adr_usd) & visitor_df$visitor_hist_adr_usd != 0)]
  usd_fit <- fitdistr(visitor_usd, 'weibull')
  weib_para <- usd_fit$estimate
  
  visitor_usd_complete <- visitor_df$visitor_hist_adr_usd
  vuc_nans <- sum(is.na(visitor_usd_complete))
  usd_samples <- rweibull(vuc_nans, weib_para[1], weib_para[2])
  visitor_usd_complete[is.na(visitor_usd_complete)] <- usd_samples
  
  add_visitor_df <- data.frame(new_visitor)
  
  # - - - - - - - - - - - - - - - 
  # Fix competition flags
  # - - - - - - - - - - - - - - -
  message("Working on competition values features...")
  # Flags for lowest price and only with availability
  
  comp_indices <- which(na_features %in% features[29:52])
  comp_features <- names(na_df[, comp_indices])
  comp_df <- na_df[, comp_indices]
  
  rate_i <- which(comp_features %in% comp_features[seq(1, length(comp_features), 3)])
  inv_i <- which(comp_features %in% comp_features[seq(2, length(comp_features), 3)])
  rpd_i <- which(comp_features %in% comp_features[seq(3, length(comp_features), 3)])
  
  n <- nrow(comp_df)
  lowest_price <- logical(n)
  only_avail <- logical(n)
  
  lp_func <- function(x) (min(x, na.rm=TRUE) > -1 && any(!is.na(x)))
  oa_func <- function(x) (min(x, na.rm=TRUE) == 1 && any(!is.na(x)))
  lowest_price <- apply(comp_df[, rate_i], 1, lp_func)
  only_avail <- apply(comp_df[, inv_i], 1, oa_func)
  
  add_comp_df <- data.frame(lowest_price, only_avail)
  
  # - - - - - - - - - - - - - - - 
  # Fix review score
  # - - - - - - - - - - - - - - -
  message("Working on review score features...")
  # Imputing NA review score by value corresponding to 10% quantile
  
  property_indices <- which(na_features %in% features[9:15])
  property_features <- names(na_df[, property_indices])
  
  low_review_score <- quantile(na_df$prop_review_score, probs = c(0.1), na.rm=TRUE)
  
  # - - - - - - - - - - - - - - - 
  # Fix competition flags
  # - - - - - - - - - - - - - - -
  message("Working on affinity feature")
  # Temporary method for imputing affinity, imputing with min
  
  affinity <- input_data$srch_query_affinity_score
  input_data$srch_query_affinity_score[
    is.na(affinity)] <- min(affinity, na.rm=TRUE)
  
  # - - - - - - - - - - - - - - - 
  # Altering original data, adding new columns
  # - - - - - - - - - - - - - - -
  
  input_data$prop_review_score[is.na(input_data$prop_review_score)] <- low_review_score
  input_data$visitor_hist_starrating <- visitor_stars_complete
  input_data$visitor_hist_adr_usd <- visitor_usd_complete
  input_data <- cbind(input_data, add_visitor_df, add_comp_df)
  
  # - - - - - - - - - - - - - - - - - -
  # Remove gross_bookings_usd
  # - - - - - - - - - - - - - - - - - -
  # Feature is not included in the testing set
  input_data = dplyr::select(input_data, -gross_bookings_usd)
  
  
  # - - - - - - - - - - - - - - - - - -
  # Fix NA values for competitors
  # - - - - - - - - - - - - - - - - - -
  # Setting every NA to 0
  
  imputed_comp_df <- comp_df
  imputed_comp_df[is.na(comp_df)] <- 0
  input_data[, seq(29,52)] <- imputed_comp_df
  
  return(input_data)
} 


# Run the function to process entire training data and save to new object
load("../data/training_processed.rda")

features <- names(training)
df <- training
df$comp1_rate <- as.integer(df$comp1_rate)
df$comp1_inv <- as.integer(df$comp1_inv)
df$comp1_rate_percent_diff <- as.integer(df$comp1_rate_percent_diff)
rm(training)

#Split
srch_ids <- df$srch_id
srch_ids.unique <- unique(srch_ids)
data_split <- 0.01
n_train <- floor(length(srch_ids.unique)*data_split)
n_test <- length(srch_ids.unique) - n_train

train_ids <- sample(srch_ids.unique, n_train)
test_ids <- subset(srch_ids.unique, !srch_ids.unique %in% train_ids)

train_df <- subset(df, srch_id %in% train_ids)
identical(sort(unique(train_df$srch_id)), sort(train_ids))

test_df <- subset(df, srch_id %in% test_ids)

rm(df)
training <- train_df
rm(train_df)

training_process_subsampled = preprocess_data(training)
save(file = "additional_data/naive_preprocessed.rda", training_process_subsampled)


