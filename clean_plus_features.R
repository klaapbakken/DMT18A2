source("evaluate.R")

load("data/preprocessed.rda")
load("data/preprocessed_subsampled.rda")
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
full_table <- create_center_distance(full_table)
full_table <- create_family_features(full_table)

stratified_table <- clean_expedia(stratified_table,to_exclude)
stratified_table <- create_center_distance(stratified_table)
stratified_table <- create_family_features(stratified_table)

save(full_table, file = "data/preprocessed_corrected.rda")
save(stratified_table, file = "data/preprocessed_subsampled_corrected.rda")