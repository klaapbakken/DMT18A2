library(tidyverse)
library(purrrlyr)
library(MASS)
library(gbm)
source("../preprocess.R")
source("../ndcg.R")

subsample_size = length(levels(as.factor(training$srch_id))) * 0.05
subsample_idx = sample(levels(as.factor(training$srch_id)), size = ceiling(subsample_size))
input_data_subsampled = training %>% 
  filter(srch_id %in% subsample_idx)
testtable = preprocess_data(input_data_subsampled)


# Modify datatypes
datatable = testtable$input_data %>% 
  mutate(relevance = case_when(.$click_bool == 0 & .$booking_bool == 0 ~ 0,
                               .$click_bool == 1 & .$booking_bool == 0 ~ 1,
                               .$click_bool == 1 & .$booking_bool == 1 ~ 1)) %>% 
  mutate(relevance = case_when(.$click_bool == 0 & .$booking_bool == 0 ~ 0,
                               .$click_bool == 1 & .$booking_bool == 0 ~ 1,
                               .$click_bool == 1 & .$booking_bool == 1 ~ 1)) %>% 
  mutate(relevance = as.factor(relevance),
         srch_id = as.numeric(srch_id),
         prop_id = as.numeric(prop_id),
         srch_destination_id = as.numeric(srch_destination_id),
         comp1_rate = as.numeric(comp1_rate),
         comp1_inv = as.numeric(comp1_inv),
         comp1_rate_percent_diff = as.numeric(comp1_rate_percent_diff),
         srch_destination_id = as.numeric(srch_destination_id),
         new_visitor = as.integer(new_visitor),
         lowest_price = as.integer(lowest_price)) %>% 
  dplyr::select(-click_bool, -booking_bool, -date_time, -ymd, 
                -position, -only_avail, -lowest_price, -responses, 
                -relative_click, -clicks_counts_by_prop, -relative_bool, -affinity,
                -bool_counts_by_prop, -normalised_trick3)

preprocessParams <- preProcess(datatable, method=c("scale"))
transformed <- predict(preprocessParams, datatable)

# Subsample into testing and training sets
subsample_size = length(levels(as.factor(datatable$srch_id))) * 0.01
subsample_idx = sample(levels(as.factor(datatable$srch_id)), size = ceiling(subsample_size))

training_set = datatable %>% 
  filter(srch_id %in% subsample_idx)

# View training set
validation_set = datatable %>% 
  filter(!(srch_id %in% subsample_idx))

# gbm
library(caret)

# Train model
modelFit <- train(relevance ~ ., data = training_set, 
                  method = "gbm")

varImp(modelFit)

# Predict validation set
predicted_relevance = predict(modelFit, newdata = validation_set)
validation_set$predicted_relevance = predicted_relevance
validation_set$prop_id = as.numeric(validation_set$prop_id )

# Evaluate
source("../evaluate.R")
source("../ndcg.R")
evaluate(validation_set)

