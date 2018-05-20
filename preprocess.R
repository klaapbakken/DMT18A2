# Pre-processing function for Expedia data

# Load required libraries
library(tidyverse)
library(purrrlyr)
library(MASS)
#library(parallel)

# Function to return preprocessed dataframe
preprocess_data = function(input_data, subsample = 0.10){
  
  # Calculate the number of cores
  #no_cores <- detectCores() - 1
  
  # Initiate cluster
  #cl <- makeCluster(no_cores)
  
  # - - - - - - - - - - - - - - - 
  # Fix competitor missing values
  # - - - - - - - - - - - - - - -
  message("Working on competitor features...")
  start.time <- Sys.time()
  
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
  message(Sys.time() - start.time)
  
  # Stop cluster
  #stopCluster(cl)
  
  
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
  
  # impute visitor history features via linear modelling
  summaried <- input_data %>%
    group_by(srch_id) %>%
    summarise(hist_usd = mean(price_usd),
              med_star= median(visitor_hist_starrating),
              hist_usd_sqrt = sqrt(mean(price_usd)),
              med_adr = median(visitor_hist_adr_usd,na.rm=T),
              med_adr_sqrt=sqrt(med_adr))
  
  model_star <- lm(med_star ~ hist_usd_sqrt+I(hist_usd_sqrt^2),  data = summaried[summaried$hist_usd_sqrt<50,])
  model_usd <- lm(med_adr_sqrt ~ hist_usd,  data = summaried[summaried$hist_usd_sqrt<2000,])
  summaried$idx <- seq(from=1, to=dim(summaried)[1]) 
  
  indextopredit_star <- summaried[is.na(summaried$med_star),]
  indextopredit_usr <- summaried[is.na(summaried$med_adr_sqrt ),]
  
  indextopredit_star$med_star_predicted <- predict.lm(model_star,sqrt(summaried[is.na(summaried$med_star),"hist_usd_sqrt"]))
  indextopredit_usr$med_usd_sqrt_predicted <- predict.lm(model_usd,summaried[is.na(summaried$med_adr_sqrt),"hist_usd"])
  
  indextopredit_usr$med_usd_sqrt_predicted <- indextopredit_usr$med_usd_sqrt_predicted^2
  predicted_valued.summary <- left_join(summaried,indextopredit_star, by="idx")
  predicted_valued.summary <- left_join(predicted_valued.summary,indextopredit_usr, by="idx")
  
  add_to_visitor <- left_join(input_data, predicted_valued.summary[,c("srch_id","med_usd_sqrt_predicted","med_star_predicted")],by="srch_id")
  
  # create the two new variable
  add_visitor_df$visitor_hist_starrating_lm <- apply(add_to_visitor[,c("visitor_hist_starrating","med_star_predicted")],1,FUN=sum,na.rm = T)
  add_visitor_df$visitor_hist_adr_usd_lm <- apply(add_to_visitor[,c("visitor_hist_adr_usd","med_usd_sqrt_predicted")],1,FUN=sum,na.rm = T)
  
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
  input_data$prop_review_score_n <- input_data$prop_review_score
  
  property_indices <- which(na_features %in% features[9:15])
  property_features <- names(na_df[, property_indices])
  
  low_review_score <- quantile(na_df$prop_review_score, probs = c(0.1), na.rm=TRUE)
  
  
  #imputing in alterative way the prop_score review
  #summary by hotels
  summary.hotels <- input_data %>%
    group_by(prop_country_id,prop_id) %>%
    summarise(count=n(), bool_counts_by_prop  = sum(booking_bool),relative_bool= sum(booking_bool)/n(),  clicks_counts_by_prop = sum(click_bool), relative_click= sum(click_bool)/n())
  

  input_data <- left_join(input_data,summary.hotels,by=c('prop_country_id','prop_id'))
  
  input_data <- input_data %>% group_by(prop_review_score) %>% mutate(normalised_trick3=(clicks_counts_by_prop*bool_counts_by_prop)-mean(clicks_counts_by_prop*bool_counts_by_prop)/sd( clicks_counts_by_prop*bool_counts_by_prop))
  
  summary_for_prop_score <- input_data %>%
    group_by(prop_review_score_n)%>%
    summarise(number=n(),mean_trick=mean(clicks_counts_by_prop*bool_counts_by_prop),
              sd_trick=sd(clicks_counts_by_prop*bool_counts_by_prop),
              mean_trick=mean(clicks_counts_by_prop*bool_counts_by_prop),
              count_book=sum(booking_bool),count_click=sum(click_bool),
              book_freq =sum(booking_bool)/number ,click_freq=sum(click_bool)/number,mean_byrscore_group=mean(normalised_trick3,na.rm=T))
  #print(summary_for_prop_score)
  combinations <- (combn(seq(1,5,0.5),2))
  
  
  # handle missing values of prop_review_score
  dista <- function(x){
    min_dist = abs(x^2-groups_centers[1,2]^2)
    winner=1
    
    for (i in seq(2,dim(groups_centers)[1])){
      if (abs(x-groups_centers[i,2]) < min_dist){
        min_dist = abs(x^2-groups_centers[i,2]^2)
        winner=i
      }
    }
    return(as.integer(groups_centers[winner,1]))
  }
  
  groups_centers <- summary_for_prop_score[1:10,c("prop_review_score_n","mean_byrscore_group")]
  to_substitute <-  apply(input_data[is.na(input_data$prop_review_score_n),"normalised_trick3"],1,FUN =dista)
  
  input_data[is.na(input_data$prop_review_score_n),"prop_review_score_n"] <- to_substitute
  

  # - - - - - - - - - - - - - - - 
  # Fix competition flags
  # - - - - - - - - - - - - - - -
  message("Working on affinity feature")
  # Temporary method for imputing affinity, imputing with min
  input_data$srch_query_affinity_score_2 <- input_data$srch_query_affinity_score
  
  
  affinity <- input_data$srch_query_affinity_score
  input_data$srch_query_affinity_score[
    is.na(affinity)] <- min(affinity, na.rm=TRUE)
  
  #alternative to impute NA affinity value
  message("Compute new features to impute NA affinity via fitting hyperbole...")
  
  input_data <- input_data %>%
    mutate(responses = case_when(click_bool == 0 & booking_bool == 0 ~ 0,
                                 click_bool == 1 & booking_bool == 0 ~ 0.5,
                                 click_bool == 1 & booking_bool == 1 ~ 1)) %>%
    mutate(group_size = srch_adults_count + srch_children_count,
           srch_room_count = srch_room_count, 
           group_size_normalized_by_room = (srch_adults_count+srch_children_count)/srch_room_count)
  
  input_data <- input_data %>% 
    mutate(trick = ((case_when(promotion_flag == '0' ~ 1, promotion_flag == '1' ~ 2)*case_when(srch_saturday_night_bool == '0' ~ 1, srch_saturday_night_bool == '1' ~ 2))*(srch_length_of_stay*srch_booking_window*group_size))) %>%
    mutate(affinity = relative_bool * relative_click, affinity_log =  ((relative_bool * relative_click) + count)) %>%
    mutate(trick2 = ((srch_length_of_stay*srch_booking_window*group_size)+count))
  
  fun.1 <- function(x){
    return ((-350*(1/(sqrt(x))) -12) +rnorm(1))
  }
  
  input_data <- input_data %>%
    mutate( srch_query_affinity_estimate = fun.1(trick2))
  
  input_data$srch_query_affinity_score_2[is.na(input_data$srch_query_affinity_score_2)] <- input_data$srch_query_affinity_estimate[is.na(input_data$srch_query_affinity_score_2)]
  
  # - - - - - - - - - - - - - - - 
  # Altering original data, adding new columns
  # - - - - - - - - - - - - - - -
  
  input_data$prop_review_score[is.na(input_data$prop_review_score)] <- low_review_score[[1]]
  input_data$visitor_hist_starrating <- visitor_stars_complete
  input_data$visitor_hist_adr_usd <- visitor_usd_complete
  input_data <- cbind.data.frame(input_data, add_visitor_df, add_comp_df)
  
  # - - - - - - - - - - - - - - - - - -
  # Remove gross_bookings_usd
  # - - - - - - - - - - - - - - - - - -
  # Feature is not included in the testing set
  input_data = dplyr::select(input_data, -gross_bookings_usd)
  
  
  # - - - - - - - - - - - - - - - - - -
  # Fix NA values for competitors
  # - - - - - - - - - - - - - - - - - -
  # Setting every NA to 2
  
  imputed_comp_df <- comp_df
  imputed_comp_df[is.na(comp_df)] <- 2
  input_data[, seq(29,52)] <- imputed_comp_df
  
  
  # - - - - - - - - - - - -
  # Subsample the dataframe
  # - - - - - - - - - - - -
  #message("Subsampling stratified the data...")
  # Get the number of search queries
  #subsample_size = length(levels(as.factor(input_data$srch_id))) * subsample
  
  # Gather X% of training queries
  #subsample_idx = sample(levels(as.factor(input_data$srch_id)), size = ceiling(subsample_size))
  
  # Keep only sampled queries
  #input_data_subsampled = input_data %>% 
  #  filter(srch_id %in% subsample_idx)
  
  # Return dataframe with imputed values
  
  message("Subsampling stratified the data...")
  
  input_data.click <- input_data[input_data$click_bool==1 & input_data$booking_bool == 0,]
  input_data.book <- input_data[input_data$click_bool==1 & input_data$booking_bool == 1,] 
  input_data.notbook <- input_data[input_data$click_bool==0 & input_data$booking_bool == 0,]
  N <- dim(input_data.click)[1]
  stratified <- rbind(sample_n(input_data.click,size =N,replace=T),sample_n(input_data.book,size=N,replace=T),sample_n(input_data.notbook,size=N,replace=T))
    
  return(list(input_data = input_data, 
              stratified = stratified))
  #return(input_data_subsampled)
} 
