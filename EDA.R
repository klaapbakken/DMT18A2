## EDA clustering and profiling.

## load table
setwd("~/DMT18A2")
library(Rtsne)
library(tidyverse)
library(caret)
library(broom)
library(mice)
library(mitools)
library(BaylorEdPsych)

## clustering
load("data/training_processed.rda")

training.outliers <- training %>%
  filter(price_usd>=2000)%>%
  summarise(number=n(), book_c=sum(booking_bool), click_c=sum(click_bool))

## summary saturaday bool
create_summary_saturday <-  function(x1,x2)
  {
  as.data.frame(t(training.saturday_bool <- training[,] %>%
  filter(price_usd>= x1 & price_usd<= x2) %>%
  group_by(srch_saturday_night_bool) %>%
  summarise(group_entry = n(),
            count_book=sum(booking_bool),
            count_click=sum(click_bool),
            relative_book_percentage=(count_book/group_entry)*100,
            relative_clicked_percentage=(count_click/group_entry)*100,
            median_price=median(price_usd),
            sd_price=sd(price_usd),
            mean_starring=median(prop_starrating),
            sd_starring=sd(prop_starrating),
            mean_adult=mean(srch_adults_count),
            sd_adult=sd(srch_adults_count),
            mean_children=mean(srch_children_count),
            sd_children=sd(srch_children_count),
            mean_lenght=mean(srch_length_of_stay),
            sd_lenght=sd(srch_length_of_stay),
            mean_booking_window= mean(srch_booking_window),
            sd_booking_window= sd(srch_booking_window),
            mean_room = mean(srch_room_count),
            sd_room = sd(srch_room_count),
            mean_day=mean(day),
            sd_day= sd(day),
            mean_week=mean(week),
            sd_week=sd(week)))
)}

create_summary_promotion_flag <-  function(x1,x2)
{
#summary promotion flag
  as.data.frame(t(training.promotion_flag <- training %>%
  filter(price_usd>= x1 & price_usd<=x2)%>%
  group_by(promotion_flag) %>%
  summarise(group_entry = n(),
            count_book=sum(booking_bool),
            count_click=sum(click_bool),
            relative_book_percentage=(count_book/group_entry)*100,
            relative_clicked_percentage=(count_click/group_entry)*100,
            median_price=median(price_usd),
            sd_price=sd(price_usd),
            mean_starring=median(prop_starrating),
            sd_starring=sd(prop_starrating),
            mean_adult=mean(srch_adults_count),
            sd_adult=sd(srch_adults_count),
            mean_children=mean(srch_children_count),
            sd_children=sd(srch_children_count),
            mean_lenght=mean(srch_length_of_stay),
            sd_lenght=sd(srch_length_of_stay),
            mean_booking_window= mean(srch_booking_window),
            sd_booking_window= sd(srch_booking_window),
            mean_room = mean(srch_room_count),
            sd_room = sd(srch_room_count),
            mean_day=mean(day),
            sd_day= sd(day),
            mean_week=mean(week),
            sd_week=sd(week)
            )))
}

create_summary_random_bol <-  function(x1,x2)
{
## random bool
  as.data.frame(t(training.random_bool <- training %>%
  filter(price_usd>= x1 & price_usd<=x2)%>%
  group_by(random_bool) %>%
  summarise(group_entry = n(),
            count_book=sum(booking_bool),
            count_click=sum(click_bool),
            relative_book_percentage=(count_book/group_entry)*100,
            relative_clicked_percentage=(count_click/group_entry)*100,
            median_price=median(price_usd),
            sd_price=sd(price_usd),
            mean_starring=median(prop_starrating),
            sd_starring=sd(prop_starrating),
            mean_adult=mean(srch_adults_count),
            sd_adult=sd(srch_adults_count),
            mean_children=mean(srch_children_count),
            sd_children=sd(srch_children_count),
            mean_lenght=mean(srch_length_of_stay),
            sd_lenght=sd(srch_length_of_stay),
            mean_booking_window= mean(srch_booking_window),
            sd_booking_window= sd(srch_booking_window),
            mean_room = mean(srch_room_count),
            sd_room = sd(srch_room_count),
            mean_day=mean(day),
            sd_day= sd(day),
            mean_week=mean(week),
            sd_week=sd(week)
  )))
}

create_summary_prop_brand <-  function(x1,x2)
{
##prop_brand_bool
as.data.frame(t(training.prop_brand_bool <- training %>%
  filter(price_usd>= x1 & price_usd<=x2)%>%
  group_by(prop_brand_bool) %>%
  summarise(group_entry = n(),
            count_book=sum(booking_bool),
            count_click=sum(click_bool),
            relative_book_percentage=(count_book/group_entry)*100,
            relative_clicked_percentage=(count_click/group_entry)*100,
            median_price=median(price_usd),
            sd_price=sd(price_usd),
            mean_starring=median(prop_starrating),
            sd_starring=sd(prop_starrating),
            mean_adult=mean(srch_adults_count),
            sd_adult=sd(srch_adults_count),
            mean_children=mean(srch_children_count),
            sd_children=sd(srch_children_count),
            mean_lenght=mean(srch_length_of_stay),
            sd_lenght=sd(srch_length_of_stay),
            mean_booking_window= mean(srch_booking_window),
            sd_booking_window= sd(srch_booking_window),
            mean_room = mean(srch_room_count),
            sd_room = sd(srch_room_count),
            mean_day=mean(day),
            sd_day= sd(day),
            mean_week=mean(week),
            sd_week=sd(week)))
  )

} 

summary.saturday <- summary <- cbind(create_summary_saturday(0,10000),create_summary_saturday(10000,100000),create_summary_saturday(100000,100000000000))
summary.promotion <- summary <- cbind(create_summary_promotion_flag(0,10000),create_summary_promotion_flag(10000,100000),create_summary_promotion_flag(100000,100000000000))
summary.random_bool <- summary <- cbind(create_summary_random_bol(0,10000),create_summary_random_bol(10000,100000),create_summary_random_bol(100000,100000000000))
summary.prop_brand <- summary <- cbind(create_summary_prop_brand(0,10000),create_summary_prop_brand(10000,100000),create_summary_prop_brand(100000,100000000000))


## family.
training.family <- training %>%
  group_by(srch_id)%>%
  summarise(number_of_access=n(),
            adults=mean(srch_adults_count), 
            children= mean(srch_children_count), 
            clicks=sum(click_bool),
            bookings=sum(booking_bool),
            median_price= median(price_usd),
            median_stars= median(prop_starrating),
            family_factor= case_when(adults==0 & children>0~-2,adults==0~-1,
                                     adults<=1 & adults>0 & children<1 ~0,
                                     adults>1 & children<1 ~1, 
                                     adults>0 & children>0 & children<2 ~ 2,
                                     adults>0 & children>1  ~ 3
                ))

family.summary <- training.family %>%
  group_by(family_factor)%>%
  summarise(tot_number_of_access=sum(number_of_access),
            mean_numb_acc_id=mean(number_of_access),
            mean_adults=mean(adults),
            mean_children=mean(children),
            tot_click=sum(clicks),
            tot_book=sum(bookings),
            med_price =median(median_price),
            med_stars= median(median_stars),
            rel_click =(tot_click/tot_number_of_access)*100,
            rel_book =(tot_book/tot_number_of_access)*100
            )
## summary for prop starring
training.starring <- training %>%
  group_by(prop_id) %>%
  summarise(count_research=n(),count_click = sum(click_bool),count_book = sum(booking_bool), mean_star=mean(prop_starrating))%>%
  ungroup()%>%
  group_by(mean_star) %>%
  summarise(count_researches =sum(count_research),
            total_number_hotels =n(),
            clicks = sum(count_click),
            books = sum(count_book), 
            relative_click=(clicks/count_researches)*100,
            relative_book=(books/count_researches)*100)
## save table
write.csv(training.starring, file = "Starhotelstab.csv")

training.search.summary.usr <- training %>%
  group_by(srch_id) %>%
  summarise(n_accesses = n(),book_s = sum(booking_bool),click_s=sum(click_bool))

missing_values = training%>% 
   ungroup()%>% 
   summarise_all(funs(100 * mean(is.na(.) ))) %>% 
   gather(Variable, Value) %>% 
   arrange(desc(Value)) %>% 
   mutate(Variable = as_factor(Variable)) 

create_family_features <- function(expedia_tab){
  expedia_tab <- expedia_tab %>%
    mutate(family_factor = (case_when(srch_adults_count==0 & srch_children_count>0~-2,srch_adults_count==0~-1,
                                     srch_adults_count<=1 & srch_adults_count>0 & srch_children_count<1 ~0,
                                     srch_adults_count>1 & srch_children_count<1 ~1, 
                                     srch_adults_count>0 & srch_children_count>0 & srch_children_count<2 ~ 2,
                                     srch_adults_count>0 & srch_children_count>1  ~ 3) ))
 # we have tested that there are not oscillation in the research
 #           group_by(srch_id,visitor_location_country_id,srch_destination_id) %>%
 #    mutate(fam_fact_median=(median(family_factor)))
}

training <- create_family_features(training)

all_equal(training$family_factor,training$fam_fact_median)


