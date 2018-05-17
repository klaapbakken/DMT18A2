rm(list=ls())

ndcg <- function(ranking, result){
  #Ranking is a vector of prop_ids,
  #sorted from highest predicted relevance to lowest predicted relevance
  
  #Result is a data frame containing a column "prop_id", consisting of prop_ids,
  #and a column, "relevance", with actual relevance score,
  
  #Prop_ids in ranking and result should match. Each prop_id should only appear once
  
  #Vector of relevance score, corresponding to sequence of prop_id in ranking
  relevance_of <- function (prop_id_i) result$relevance[result$prop_id == prop_id_i]
  rel <- sapply(ranking, relevance_of)
  
  #Discounted cumulative gain
  dcg_of <- function (rel, i) (2^rel[i] - 1)/log2(i+1)
  dcg_terms <- sapply(seq(1, length(rel)), dcg_of, rel=rel)
  DCG <- sum(dcg_terms)
  
  #Ideal discounted cumulative gain
  ordered_result <- result[order(result$relevance, decreasing=TRUE), ]
  idcg_term <- sapply(seq(1, length(ordered_result$prop_id)),
                      dcg_of, rel=ordered_result$relevance)
  IDCG <- sum(idcg_term)
  
  #Normalized discounted cumulative gain
  nDCG <- DCG/IDCG
  
  return(nDCG)
}

load("additional_data/naive_preprocessed.rda")
df <- training_process_subsampled
rm(training_process_subsampled)

#Train, test split

data_split <- 0.8

srch_ids <- df$srch_id
srch_ids.unique <- unique(srch_ids)
n_train <- floor(length(srch_ids.unique)*data_split)
n_test <- length(srch_ids.unique) - n_train

train_ids <- sample(srch_ids.unique, n_train)
test_ids <- subset(srch_ids.unique, !srch_ids.unique %in% train_ids)

train_df <- subset(df, srch_id %in% train_ids)
identical(sort(unique(train_df$srch_id)), sort(train_ids))

test_df <- subset(df, srch_id %in% test_ids)

df <- train_df

#Balancing

booked_i <- which(df$booking_bool == 1)
booked_srch_ids <- unique(df$srch_id[booked_i])
df_booked <- df[booked_i, ]

clicked_i <- which(df$click_bool == 1)
clicked_i <- clicked_i[which(!clicked_i %in% booked_i)]
clicked_srch_ids <- unique(df$srch_id[clicked_i])
df_clicked <- df[clicked_i, ]

#Removing rows that are clicked and booked

#booked_pairs <- df[df$booking_bool == 1, c("srch_id", "prop_id")]
#clicked_pairs <- df[df$click_bool == 1, c("srch_id", "prop_id")]
#all_pairs <- rbind(booked_pairs, clicked_pairs)

#clicked_and_booked <- duplicated(all_pairs)[-seq(0,nrow(booked_pairs))]

#df_clicked <- df_clicked[!clicked_and_booked, ]

clicked_booked_i <- c(booked_i, clicked_i)
ncb_df <- df[-clicked_booked_i, ]

n <- length(clicked_i)
m <- length(booked_i)

features <- names(df)

click_df <- rbind(df_clicked, dplyr::sample_n(ncb_df, n+m))
click_remove <- c(seq(1,4), seq(29,52), seq(55,58), 5, 8, 9, 16, 19, 54)
click_df <- click_df[, -click_remove]

booked_df <- df_booked
booked_remove <- c(seq(1,4), seq(29,52), seq(55,58), 5, 8, 9, 16, 19, 53)
booked_df <- booked_df[, -booked_remove]

#Combining

altered_click_df <- click_df
altered_click_df$click_bool <- altered_click_df$click_bool
acdf_names <- names(altered_click_df)
colnames(altered_click_df)[acdf_names == "click_bool"] <- "relevance"


altered_booked_df <- booked_df
abdf_names <- names(altered_booked_df)
colnames(altered_booked_df)[abdf_names == "booking_bool"] <- "relevance"

combined_df <- rbind(altered_booked_df, altered_click_df)

logreg <- glm(relevance ~ (.), data=combined_df, family=binomial)

#Balance test data
# - - - - - - - - - - -
# - - - - - - - - - - -

df <- test_df 

#Balancing

booked_i <- which(df$booking_bool == 1)
booked_srch_ids <- unique(df$srch_id[booked_i])
df_booked <- df[booked_i, ]

clicked_i <- which(df$click_bool == 1)
clicked_i <- clicked_i[which(!clicked_i %in% booked_i)]
clicked_srch_ids <- unique(df$srch_id[clicked_i])
df_clicked <- df[clicked_i, ]

#Removing rows that are clicked and booked

#booked_pairs <- df[df$booking_bool == 1, c("srch_id", "prop_id")]
#clicked_pairs <- df[df$click_bool == 1, c("srch_id", "prop_id")]
#all_pairs <- rbind(booked_pairs, clicked_pairs)

#clicked_and_booked <- duplicated(all_pairs)[-seq(0,nrow(booked_pairs))]

#df_clicked <- df_clicked[!clicked_and_booked, ]

clicked_booked_i <- c(booked_i, clicked_i)
ncb_df <- df[-clicked_booked_i, ]

n <- length(clicked_i)
m <- length(booked_i)

features <- names(df)

click_df <- rbind(df_clicked, ncb_df)
click_remove <- c(seq(2,4), seq(29,52), seq(55,58), 5, 8, 16, 19, 54)
click_df <- click_df[, -click_remove]

booked_df <- df_booked
booked_remove <- c(seq(2,4), seq(29,52), seq(55,58), 5, 8, 16, 19, 53)
booked_df <- booked_df[, -booked_remove]

#Combining

altered_click_df <- click_df
altered_click_df$click_bool <- altered_click_df$click_bool
acdf_names <- names(altered_click_df)
colnames(altered_click_df)[acdf_names == "click_bool"] <- "relevance"


altered_booked_df <- booked_df
abdf_names <- names(altered_booked_df)
colnames(altered_booked_df)[abdf_names == "booking_bool"] <- "relevance"

combined_df <- rbind(altered_booked_df, altered_click_df)

test_df <- combined_df

predicted_relevance <- predict(logreg, newdata = test_df, type="response")

test_df$predicted_relevance <- predicted_relevance

queries <- unique(test_df$srch_id)

average_ndcg <- 0
r_a_ndcg <- 0

for (i in 1:length(queries)){
  print(i)
  query_i <- which(test_df$srch_id == queries[i])
  result <- test_df[query_i, c("srch_id", "prop_id", "relevance")]
  result$prop_id <- as.numeric(result$prop_id)
  test_df_i <- test_df[query_i, ]
  ranking <- as.numeric(test_df_i[order(predicted_relevance[query_i], decreasing=TRUE), ]$prop_id)
  ndcg_i <- ndcg(ranking, result)
  average_ndcg <- average_ndcg + ndcg_i
  
  random_ranking <- sample(ranking)
  random_ndcg_i <- ndcg(random_ranking, result)
  r_a_ndcg <- r_a_ndcg + random_ndcg_i
}
average_ndcg <- average_ndcg/length(queries)
r_a_ndcg <- r_a_ndcg/length(queries)



