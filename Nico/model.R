source("evaluate.R")

load("data/preprocessed.rda")

library(tidyverse)
library(caret)
library(broom)
training <- full_table[,-c(68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86)]

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

training.set <- training.set %>%
  mutate(group_size=srch_adults_count+srch_children_count,group_size_norm=(srch_adults_count+srch_children_count)/srch_room_count)

training.set <- left_join(training.set,training.search.summary.hotels,by=c('prop_id'))

test.set <- test.set %>%
  mutate(group_size=srch_adults_count+srch_children_count,group_size_norm=(srch_adults_count+srch_children_count)/srch_room_count)


test.set <- left_join(test.set,training.search.summary.hotels,by=c('prop_id'),fill = 0) 


training.set <- training.set %>%
  mutate_at(vars(c(72,73,74,75,76)),funs(ifelse(is.na(.),0,.)))

test.set <- test.set %>%
  mutate_at(vars(c(72,73,74,75,76)),funs(ifelse(is.na(.),0,.)))

get <- c(1,9,6,7,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,56,57,58,59,60,61,62,63,64,65,66,67,68,70,71,72,73,74,75,76,69)
subset_var_train <- training.set[,get]
subset_var_test <- test.set[,get]



library(pls)
subset_var_train$comp1_rate_percent_diff <- as.numeric(subset_var_train$comp1_rate_percent_diff)
subset_var_train$comp1_inv <- as.numeric(subset_var_train$comp1_inv)
subset_var_train$comp1_rate <- as.numeric(subset_var_train$comp1_rate)


subset_var_test$comp1_rate_percent_diff <- as.numeric(subset_var_test$comp1_rate_percent_diff)
subset_var_test$comp1_inv <- as.numeric(subset_var_test$comp1_inv)
subset_var_test$comp1_rate <- as.numeric(subset_var_test$comp1_rate)


model.lm <- lm(relevance~0+prop_starrating + prop_location_score1+prop_location_score2+promotion_flag+relative_bool+relative_click,data=subset_var_train[,-c(1,2,14)])

predicted <- subset_var_test[,c("srch_id","prop_id","relevance")]
predicted$predicted_relevance <- predict(model.lm,subset_var_test[,-c(1,2,14,76)])
predicted$srch_id <- as.numeric(predicted$srch_id)
predicted$prop_id <- as.numeric(predicted$prop_id)
sapply(predicted,class)
evaluate(predicted)
summary(model.lm)

summaries <- tidy(model.lm)
glance(model.lm)
summaries <- summaries %>%
  filter(p.value<0.01)

library(gbm)


gbmModel = gbm(formula = relevance~0+prop_starrating + prop_location_score1+prop_location_score2+promotion_flag+relative_bool+relative_click,
               distribution = "gaussian",
               data = subset_var_train[,-c(1,2,14)],
               n.trees = 2500,
               shrinkage = .01,
               n.minobsinnode = 20)

predicted <- subset_var_test[,c("srch_id","prop_id","relevance")]
predicted$predicted_relevance <-predict(object = gbmModel,
                              newdata =  subset_var_test[,-c(1,2,14,76)],
                              n.trees = 1500,
                              type = "response")
predicted$srch_id <- as.numeric(predicted$srch_id)
predicted$prop_id <- as.numeric(predicted$prop_id)
evaluate(predicted)

View(predicted)
summary(model.pls)
