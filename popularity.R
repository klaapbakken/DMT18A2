library(dplyr)

#Creates a data frame containing "popularity" for each prop_id
#Popularity can thought of as "average interaction" a user has with a property.
#1 is "every user books it", 0 is "no user clicks or books".
extract_popularity <- function(train_df){
  data_split <- 0.99
  srch_ids <- train_df$srch_id
  srch_ids.unique <- unique(srch_ids)
  n_train <- floor(length(srch_ids.unique)*data_split)
  train_ids <- sample(srch_ids.unique, n_train)
  train_df <- subset(train_df, srch_id %in% train_ids)
  
  responses <- group_by(train_df, prop_id) %>%
    summarise(total = n(), clicks = sum(click_bool), bookings = sum(booking_bool))
  popularity <- group_by(responses, prop_id) %>%
    summarise(popularity = (clicks/6 + bookings*5/6)/total) %>% as.data.frame
  return(popularity)
}

#Popularity of property is added to the data frame
apply_popularity <- function(df, pop_df){
  new_df <- left_join(test_df, pop_df, by = "prop_id") %>%
    mutate(new_property = is.na(popularity)) %>%
    replace(., is.na(.), 0)
  return(new_df)
}

#train_df <- data.frame(srch_id = c(rep(2, 5), rep(3,5)), prop_id = seq(1,10),
#                       click_bool = sample(c(0,1), 10, replace=TRUE),
#                       booking_bool = sample(c(0,1), 10, replace=TRUE))

#pop_df <- extract_popularity(train_df)

#test_df <- data.frame(prop_id = seq(3,7))

#apply_popularity(test_df, pop_df)
