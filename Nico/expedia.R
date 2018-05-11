#load the data

load("../data/training_processed.rda")
library("igraph") 
install.packages("network") 
install.packages("sna")
install.packages("visNetwork")
install.packages("threejs")
install.packages("networkD3")
install.packages("ndtv")


#net <- graph_from_data_frame(d=cbind(training.search.summary.orig_prop$srch_id,training.search.summary.orig_prop$srch_destination_id), vertices=training.search.summary.orig_prop$orig_destination_distance, directed=T) 
#net <- simplify(net, remove.multiple = F, remove.loops = T) 
#plot(net, edge.arrow.size=.4,vertex.label=NA)


#library loading
library(tidyverse)
library(corrplot)

# missing values column analysis
missing_values = training %>% 
  summarise_all(funs(100 * mean(is.na(.) ))) %>% 
  gather(Variable, Value) %>% 
  arrange(desc(Value)) %>% 
  mutate(Variable = as_factor(Variable))

# store before process
training.set <- training


#create index to allow easy join
training.set$idx <- seq(from =1 ,to=dim(training.set)[1] ) 

#summary by hotels
training.search.summary.hotels <- training.set %>%
   group_by(prop_country_id,prop_id) %>%
   summarise(count=n(), bool_counts_by_prop  = sum(booking_bool),relative_bool= sum(booking_bool)/n(),  clicks_counts_by_prop = sum(click_bool), relative_click= sum(click_bool)/n())

#store the table
#write.csv(training.search.summary.hotels,'hotels_summary.csv')

# create new features to find correlation to fill missing values
training.search.new_features <- left_join(training.set,training.search.summary.hotels,by=c('prop_country_id','prop_id'))

training.search.new_features <- training.search.new_features %>%
  mutate(responses = case_when(click_bool == 0 & booking_bool == 0 ~ 0,click_bool == 1 & booking_bool == 0 ~ 0.5,click_bool == 1 & booking_bool == 1 ~ 1)) %>%
  mutate(group_size = srch_adults_count+srch_children_count, group_size_normalized_by_room = (srch_adults_count+srch_children_count)/srch_room_count)


training.search.new_features <- training.search.new_features %>% 
  mutate(trick = ((case_when(promotion_flag == '0' ~ 1, promotion_flag == '1' ~ 2)*case_when(srch_saturday_night_bool == '0' ~ 1, srch_saturday_night_bool == '1' ~ 2))*(srch_length_of_stay*srch_booking_window*group_size))) %>%
  mutate(affinity = relative_bool * relative_click, affinity_log =  ((relative_bool * relative_click) + count)) %>%
  mutate(trick2 = ((srch_length_of_stay*srch_booking_window*group_size)+count))

#COMP MISSING VALUE
names(training.search.new_features)

sapply(training.search.new_features,class)
training.search.new_features$comp1_inv <- as.intager(training.search.new_features$comp1_inv)
training.search.new_features$comp1_rate <- as.integer(training.search.new_features$comp1_rate)
training.search.new_features$comp1_rate_percent_diff <- as.integer(training.search.new_features$comp1_rate_percent_diff)

for (i in 29:52) {
  training.search.new_features[is.na(training.search.new_features[,i]),i] <- 2
}

#summary by starting_point ending point.
training.search.summary <- training.search.new_features %>%
  group_by(visitor_location_country_id,prop_country_id,srch_destination_id,prop_id) %>%
  summarise(mean_distance = mean(orig_destination_distance, na.rm = T),median_distance = median(orig_destination_distance, na.rm = T),count = n())
  


#divide in the 3 subgroup and raw downsampling
training.search.new_features.click <- training.search.new_features[training.search.new_features$click_bool==1 & training.search.new_features$booking_bool == 0,]
training.search.new_features.clickbook <- training.search.new_features[training.search.new_features$click_bool==1 & training.search.new_features$booking_bool == 1,] 
training.search.new_features.notbook <- training.search.new_features[training.search.new_features$click_bool==0 & training.search.new_features$booking_bool == 0,]

stratified <- rbind(training.search.new_features.click,training.search.new_features.notbook[1:83489,],training.search.new_features.clickbook[1:83489,]) %>%
  mutate(responses = case_when(click_bool == 0 & booking_bool == 0 ~ 0,click_bool == 1 & booking_bool == 0 ~ 0.5,click_bool == 1 & booking_bool == 1 ~ 1)) %>%
  mutate(responses2 = (responses+relative_click+relative_bool)/3 ) %>%
  mutate(responses3 = (responses*relative_click*relative_bool) )

dev.off()


names(stratified)

# handle missing values of srch_query_affinity_score

fun.1 <- function(x){
  return (-350*(1/(sqrt(x))) -12 )
}


ggplot(stratified,aes(y=srch_query_affinity_score ,x=trick2)) + geom_point(aes(colour=responses)) + stat_function(fun = fun.1) 

training.search.new_features <- training.search.new_features %>%
  mutate( srch_query_affinity_estimate = fun.1(trick2))

training.search.new_features$srch_query_affinity_score[is.na(training.search.new_features$srch_query_affinity_score)] <- training.search.new_features$srch_query_affinity_estimate[is.na(training.search.new_features$srch_query_affinity_score)]

# missing values column analysis
missing_values = training.search.new_features %>% 
  summarise_all(funs(100 * mean(is.na(.) ))) %>% 
  gather(Variable, Value) %>% 
  arrange(desc(Value)) %>% 
  mutate(Variable = as_factor(Variable))
View(missing_values)

stratified$prop_country_id <- as.numeric(stratified$prop_country_id)
stratified$srch_id <- as.numeric(stratified$srch_id)


#handling missing values of orig_distance
ggplot(stratified,aes(y=orig_destination_distance ,x=(srch_id-prop_country_id) )) + geom_point(aes(colour=as.factor(responses)))

summaried <- training.search.new_features %>%
  group_by(srch_id) %>%
  summarise(rating = mean(prop_starrating),hist_usd_log = log(mean(price_usd)+1),hist_usd = mean(price_usd),hist_usd_gross = mean(gross_bookings_usd,na.rm=T),
            med_star= median(visitor_hist_starrating), count_book= sum(booking_bool), count_click= sum(click_bool),
            med_adr= median(visitor_hist_adr_usd), hist_log= log(hist_usd_gross+1), med_adr_log = log(med_adr+1),
            hist_gross_sqrt = sqrt(mean(gross_bookings_usd,na.rm=T)),hist_usd_sqrt = sqrt(mean(price_usd)),med_adr_sqrt=sqrt(med_adr), hist_demean=hist_usd-mean(hist_usd))

ggplot(summaried,aes(x=hist_usd,y=med_adr_sqrt )) + geom_point(aes(colour=as.factor(count_book))) + geom_abline(slope=linear.model2$coefficients[1],intercept=linear.model2$coefficients[2]) + xlim(0,2000 )
ggplot(summaried,aes(x=hist_usd_sqrt,y=med_star )) + geom_point(aes(colour=as.factor(count_book))) + geom_abline(slope=linear.model$coefficients[1],intercept=linear.model$coefficients[2]) + xlim(0,30 )


ggplot(summaried[summaried$count_book==1,],aes(x=hist_usd,y=med_star )) + geom_point(aes(colour=as.factor(count_book))) + geom_abline(slope=linear.model$coefficients[1],intercept=linear.model$coefficients[2]) + xlim(0,2000 )
ggplot(summaried[summaried$count_book==0,],aes(x=hist_usd,y=med_star )) + geom_point(aes(colour=as.factor(count_book))) + geom_abline(slope=linear.model$coefficients[1],intercept=linear.model$coefficients[2]) + xlim(0,2000 )


linear.model <- lm(med_star ~ hist_usd_sqrt,  data = summaried[summaried$hist_log<50,])
linear.model2 <- lm(med_adr_sqrt ~ hist_usd,  data = summaried[summaried$hist_usd_sqrt<1000,])


summaried$idx <- seq(from=1, to=dim(summaried)[1]) 

indextopredit_star <- summaried[is.na(summaried$med_star),]
indextopredit_usr <- summaried[is.na(summaried$med_adr_sqrt ),]

indextopredit_star$med_star_predicted <- predict.lm(linear.model,sqrt(summaried[is.na(summaried$med_star),"hist_usd_sqrt"]))
indextopredit_usr$med_usd_sqrt_predicted <- predict.lm(linear.model2,summaried[is.na(summaried$med_adr_sqrt),"hist_usd"])

indextopredit_usr$med_usd_sqrt_predicted <- indextopredit_usr$med_usd_sqrt_predicted^2
predicted_valued.summary <- left_join(summaried,indextopredit_star, by="idx")
predicted_valued.summary <- left_join(predicted_valued.summary,indextopredit_usr, by="idx")


final.table <- left_join(training.search.new_features, predicted_valued.summary,by="srch_id")
apply(final.table[,c("visitor_hist_starrating","med_star_predicted")],1,FUN=sum,na.rm = T)

final.table$visitor_hist_starrating <- apply(final.table[,c("visitor_hist_starrating","med_star_predicted")],1,FUN=sum,na.rm = T)
final.table$visitor_hist_adr_usd <- apply(final.table[,c("visitor_hist_adr_usd","med_usd_sqrt_predicted")],1,FUN=sum,na.rm = T)

#test missing value

missing_values = final.table %>% 
  summarise_all(funs(100 * mean(is.na(.) ))) %>% 
  gather(Variable, Value) %>% 
  arrange(desc(Value)) %>% 
  mutate(Variable = as_factor(Variable))
View(missing_values)


# plot
ggplot(stratified,aes(x=orig_destination_distance)) + geom_density(aes(colour=as.factor(responses)))

ggplot(stratified,aes(x=prop_location_score2)) + geom_density(aes(colour=as.factor(responses)))

ggplot(stratified,aes(x=prop_review_score)) + geom_density(aes(colour=as.factor(responses)))

ggplot(stratified,aes(x=orig_destination_distance)) + geom_point(aes(colour=as.factor(responses)))

ggplot(stratified,aes(x=prop_location_score2,y=clicks_counts_by_prop+bool_counts_by_prop)) + geom_point(aes(colour=as.factor(responses)))

ggplot(stratified,aes(x=clicks_counts_by_prop*bool_counts_by_prop)) + geom_density(aes(colour=as.factor(responses),group=prop_review_score))+xlim(0,10)

ggplot(stratified,aes(x=prop_review_score,y=clicks_counts_by_prop*bool_counts_by_prop)) + geom_boxplot(aes(group=prop_review_score,colour=as.factor(prop_review_score)))


tabletotest <- final.table %>% group_by(prop_review_score) %>% mutate(normalised_trick3=(clicks_counts_by_prop*bool_counts_by_prop)-mean(clicks_counts_by_prop*bool_counts_by_prop)/sd( clicks_counts_by_prop*bool_counts_by_prop))

summary <- final.table %>%
       group_by(prop_review_score)%>%
       summarise(number=n(),mean_trick=mean(clicks_counts_by_prop*bool_counts_by_prop),
                 sd_trick=sd(clicks_counts_by_prop*bool_counts_by_prop),
                 mean_trick=mean(clicks_counts_by_prop*bool_counts_by_prop),
                 count_book=sum(booking_bool),count_click=sum(click_bool),
                 book_freq =sum(booking_bool)/number ,click_freq=sum(click_bool)/number,mean_byrscore_group=mean(normalised_trick3))

combinations <- (combn(seq(1,5,0.5),2))

# testing all the group different
for (j in seq(1,dim(combinations)[2])){
  
  print(t.test(normalised_trick3~as.factor(prop_review_score), 
                 data=tabletotest[tabletotest$prop_review_score==combinations[1,j] | tabletotest$prop_review_score==combinations[2,j],])
)
}


dista <- function(x,groups_centers){
  
  min_dist = groups_centers[1,2]
  winner=1
  for (i in seq(2,dim(group_centers)[1])){
    
   if (groups_centers[i,2] < min_dist){
     min_dist = gruops_centers[i,2]
     winner=i
   }
  }
 return(groups_centers[i,1]) 
}
# handling prop_review_score nearest to a group center.

final.table[is.na(final.table$prop_review_score),"prop_review_score"] <- lappaly(data=summary[,c("prop_review_score","mean_byrscore_group")],FUN =dista())


View(summary)
write.csv(summary,"justify_prop_review_score.csv")      