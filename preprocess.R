# Pre-processing function for Expedia data

# Load required libraries
library(tidyverse)
library(purrrlyr)

# Function to return preprocessed dataframe
preprocess_data = function(input_data, subsample = 0.10){
  
  # - - - - - - - - - - - - - - - 
  # Fix competitor missing values
  # - - - - - - - - - - - - - - -
  # Count the number of NaN/0/+1 values across compX_inv
  comp_inv_missing = input_data %>% 
    select(ends_with("inv")) %>% 
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
    select(starts_with("comp_inv"))
  
  # Count the number of NaN/0/+1 values across compX_inv
  comp_inv_missing = input_data %>% 
    select(ends_with("inv")) %>% 
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
    select(starts_with("comp_inv"))
  
  # Count the number of NaN/mean(abs) values across compX_rate_percent
  comp_rate_percent_missing = input_data %>% 
    select(ends_with("rate_percent_diff")) %>% 
    by_row(
      ..f = function(x) {
        sum(is.na(x[1:8]))
      },
      .to = "comp_rate_percent_diff_na",
      .collate = "cols"
    ) %>% 
    select(starts_with("comp_rate_percent"))
  
  # Cbind the existing data
  input_data = input_data %>% 
    select(-ends_with("inv")) %>% 
    select(-ends_with("rate")) %>% 
    select(-ends_with("rate_percent_diff")) %>% 
    cbind(comp_inv_missing) %>% 
    cbind(comp_rate_percent_missing) %>% 
    cbind(comp_inv_missing)
  
  
  # - - - - - - - - - - - - - - - 
  # Fix origin distance values
  # - - - - - - - - - - - - - - -
  # Replace NA values with the median/mean distances across the origin
  input_data = input_data %>% 
    replace_na(list(orig_destination_distance = median(.$orig_destination_distance, na.rm = T)))
  
  
  # - - - - - - - - - - - - - - - 
  # Fix location score #2 values
  # - - - - - - - - - - - - - - -
  # Set the location score #2 to the minimum value across all scores
  input_data = input_data %>% 
    group_by(srch_destination_id) %>% 
    replace_na(list(prop_location_score2 = min(.$prop_location_score2, na.rm = T))) %>% 
    ungroup()
  
  
  # - - - - - - - - - - - -
  # Subsample the dataframe
  # - - - - - - - - - - - -
  # Get the number of search queries
  subsample_size = length(levels(as.factor(input_data$srch_id))) * subsample
  
  # Gather X% of training queries
  subsample_idx = sample(levels(as.factor(input_data$srch_id)), size = ceiling(subsample_size))
  
  # Keep only sampled queries
  input_data_subsampled = input_data %>% 
    filter(srch_id %in% subsample_idx)
  
  # Return dataframe with imputed values
  return(input_data_subsampled)
}


# Run the function to process entire training data and save to new object
load("data/training_processed.rda")
training_process_subsampled = preprocess_data(training)
save(file = "data/preprocessed.rda", training_data_subsampled)


