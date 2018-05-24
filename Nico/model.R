source("evaluate.R")

load("data/preprocessed.rda")

library(tidyverse)
library(broom)


create_center_distance <- function(expedia_tab){
  expedia_tab <- expedia_tab%>%
  group_by(srch_id,visitor_country_id,search_destination_id) %>%
  mutate(center_dist=orig_destination_distance- mean(orig_destination_distance)) %>%
  mutate(group_size=srch_adults_count+srch_children_count,group_size_norm=(srch_adults_count+srch_children_count)/srch_room_count)
  
}

clean_expedia <- function(expedia_table,to_exclude) {
  to_ext <- which(names(full_table) %in% to_exclude)
  expedia_table[,-to_ext]
} 

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

to_exclude <-  as.character(c("visitor_hist_adr_usd_lm",
                              "visitor_hist_starrating_lm",
                              "visitor_hist_adr_usd",
                              "visitor_hist_starrating",
                              "srch_query_affinity_estimate",
                              "trick2",
                              "affinity_log",
                              "affinity",
                              "trick",
                              "srch_query_affinity_score_2",
                              "normalised_trick3",
                              "position",
                              "srch_query_affinity_score",
                              "srch_query_affinity_score_n",
                              "prop_review_score_n",
                              "count",
                              "clicks_counts_by_prop",
                              "bool_counts_by_prop",
                              "relative_click",
                              "relative_bool",
                              "wday",
                              "month",
                              "responses"
))

full_table <- clean_expedia(full_table,to_exclude)

missing_values_2 = full_table%>% 
  ungroup()%>% 
  summarise_all(funs(100 * mean(is.na(.) ))) %>% 
  gather(Variable, Value) %>% 
  arrange(desc(Value)) %>% 
  mutate(Variable = as_factor(Variable)) 

training <- full_table[,]
training <- create_family_features(training)
training <- mutate(training,relevance = case_when(click_bool == 0 & booking_bool == 0 ~ 0,click_bool == 1 & booking_bool == 0 ~ 1,click_bool == 1 & booking_bool == 1 ~ 5))
training.search.summary.usr <- training %>%
  group_by(srch_id) %>%
  summarise(n_accesses = n(),book_s = sum(booking_bool),click_s=sum(click_bool))

summary(training.search.summary.usr)
total <- sample_n(training.search.summary.usr[training.search.summary.usr$book_s>0,],10000,replace = T)
test <- sample(seq_len(nrow(total)),2000,replace=T)

training.set <- training[training$srch_id %in% total$srch_id[-test],]
test.set <- training[training$srch_id %in% total$srch_id[test],]

training.search.summary.hotels <- training.set[-(training$srch_id %in% total$srch_id[test]),] %>%
  group_by(prop_id) %>%
  summarise(count=n(), bool_counts_by_prop  = sum(booking_bool),relative_bool= sum(booking_bool)/n(),  clicks_counts_by_prop = sum(click_bool), relative_click= sum(click_bool)/n())


training.set <- left_join(training.set,training.search.summary.hotels,by=c('prop_id'))
test.set <- left_join(test.set,training.search.summary.hotels,by=c('prop_id'),fill = 0) 


training.set <- training.set %>%
  mutate_at(vars(c(68,69,70,71,72)),funs(ifelse(is.na(.),0,.)))

test.set <- test.set %>%
  mutate_at(vars(c(68,69,70,71,72)),funs(ifelse(is.na(.),0,.)))

not_get <- c(2,3,4,5,6,16)
subset_var_train <- training.set[,-not_get]
subset_var_test <- test.set[,-not_get]

subset_var_train$comp1_rate_percent_diff <- as.numeric(subset_var_train$comp1_rate_percent_diff)
subset_var_train$comp1_inv <- as.numeric(subset_var_train$comp1_inv)
subset_var_train$comp1_rate <- as.numeric(subset_var_train$comp1_rate)


subset_var_test$comp1_rate_percent_diff <- as.numeric(subset_var_test$comp1_rate_percent_diff)
subset_var_test$comp1_inv <- as.numeric(subset_var_test$comp1_inv)
subset_var_test$comp1_rate <- as.numeric(subset_var_test$comp1_rate)


model.lm <- lm(relevance~0+.,data=subset_var_train[,-c(1,2)])


predicted <- subset_var_test[,c("srch_id","prop_id","relevance")]
predicted$predicted_relevance <- predict(model.lm,subset_var_test[,-c(1,2,14,76)])
predicted$srch_id <- as.numeric(predicted$srch_id)
predicted$prop_id <- as.numeric(predicted$prop_id)
sapply(predicted,class)
evaluate(predicted)
summary(model.lm)


summaries <-tidy(model.lm)
glance(model.lm)

summaries <- summaries %>%
  filter(p.value<0.01)

toform <- summaries$term %>%
  

Formula <- as.formula(paste("relevance~0+",paste(as.character(summaries$term),collapse="+")))
write.csv(summaries,"importancevar.csv")

library(gbm)
#reduced gm
gbmModel = gbm(Formula,
               distribution = "gaussian",
data = subset_var_train[,-c(57,56,55)],
n.trees = 2500,
shrinkage = .01,
n.minobsinnode = 20)

predicted_gbm <- subset_var_test[,c("srch_id","prop_id","relevance")]
predicted_gbm$predicted_relevance <-predict(object = gbmModel,
                                            newdata =  subset_var_test[,-c(57,55,76)],
                                            n.trees = 2000,
                                            type = "response")

predicted_gbm$srch_id <- as.numeric(predicted$srch_id)
predicted_gbm$prop_id <- as.numeric(predicted$prop_id)
evaluate(predicted_gbm)



#reduced linear model
model.lm.reduced <- lm(Formula,data=subset_var_train[,-c(1,2)])
predicted_lm <- subset_var_test[,c("srch_id","prop_id","relevance")]
predicted_lm$predicted_relevance <- predict(model.lm.reduced,subset_var_test[,-c(1,2,76)])
predicted_lm$srch_id <- as.numeric(predicted$srch_id)
predicted_lm$prop_id <- as.numeric(predicted$prop_id)
evaluate(predicted_lm)
summary(model.lm.reduced)
