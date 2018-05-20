# Code for running each step 


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Step 1. Preprocess the training data to impute missing values
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Source function
source('preprocess.R')

# Run the function to process entire training data and save to new object (run once)
load("data/training_processed.rda")
training_process = preprocess_data(training)
full_table = training_process$input_data
stratified_table = training_process$stratified
save(full_table, file = "data/preprocessed.rda")
save(stratified_table, file = "data/preprocessed_subsampled.rda")

