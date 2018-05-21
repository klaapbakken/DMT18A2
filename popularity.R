library(dplyr)

#Creates a data frame containing "popularity" for each prop_id
#Popularity can thought of as "average interaction" a user has with a property.
#1 is "every user books it", 0 is "no user clicks or books".
extract_popularity <- function(train_df){
  responses <- group_by(train_df, prop_id) %>%
    summarise(total = n(), clicks = sum(click_bool), bookings = sum(booking_bool))
  popularity <- group_by(responses, prop_id) %>%
    summarise(popularity = (clicks/6 + bookings*5/6)/total) %>% as.data.frame
  return(popularity)
}

#Popularity of property is added to the data frame
apply_popularity <- function(df, pop_df){
  new_df <- left_join(test_df, pop_df, by = "prop_id") %>% replace(., is.na(.), 0)
  return(new_df)
}