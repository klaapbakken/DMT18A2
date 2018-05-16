rm(list=ls())

ndcg <- function(ranking, result){
  #Highest ranked prop_id first in ranking
  #Result is actual relevance of properties in search query
  #Result is data frame containg srch_id (constant), prop_id and score
  relevance_of <- function (prop_id_i) result$relevance[result$prop_id == prop_id_i]
  rel <- numeric(length(ranking))
  dcg_term <- numeric(length(ranking))
  for (i in 1:length(ranking)){
    rel[i] <- relevance_of(ranking[i])
    dcg_term[i] <- (2^rel[i] - 1)/log2(i+1)
  }
  CG <- sum(rel)
  DCG <- sum(dcg_term)
  
  ordered_result <- result[order(result$relevance, decreasing=TRUE), ]
  
  idcg_term <- numeric(length(result$relevance))
  for (i in 1:length(result$relevance)){
    idcg_term[i] <- (2^relevance_of(ordered_result$prop_id[i]) - 1)/log2(i+1)
  }
  
  IDCG <- sum(idcg_term)
  
  nDCG <- DCG/IDCG
  
  return(data.frame(CG, DCG, nDCG))
}

opt_ndcg <- function(ranking, result){
  #Highest ranked prop_id first in ranking
  #Result is actual relevance of properties in search query
  #Result is data frame containg srch_id (constant), prop_id and score
  relevance_of <- function (prop_id_i) result$relevance[result$prop_id == prop_id_i]
  rel <- sapply(ranking, relevance_of)
  
  dcg_of <- function (rel, i) (2^rel[i] - 1)/log2(i+1)
  dcg_terms <- sapply(seq(1, length(rel)), dcg_of, rel=rel)
  
  CG <- sum(rel)
  DCG <- sum(dcg_terms)
  
  ordered_result <- result[order(result$relevance, decreasing=TRUE), ]
  
  idcg_term <- sapply(seq(1, length(ordered_result$prop_id)),
                      dcg_of, rel=ordered_result$relevance)
  
  IDCG <- sum(idcg_term)
  
  nDCG <- DCG/IDCG
  
  return(data.frame(CG, DCG, nDCG))
}

load("../data/naive_preprocessed.rda")
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

#Clustering

features <- names(df)
prop_features <- features[10:15]
prop_df <- df[, prop_features]
prop_cluster <- kmeans(prop_df, 8)
prop_category <- data.frame(prop_category = as.factor(prop_cluster$cluster))
df <- cbind(df, prop_category)

#Balancing

booked_i <- which(df$booking_bool == TRUE)
booked_srch_ids <- unique(df$srch_id[booked_i])
df_booked <- df[booked_i, ]

clicked_i <- which(df$click_bool == TRUE)
clicked_srch_ids <- unique(df$srch_id[clicked_i])
df_clicked <- df[clicked_i, ]

#Removing rows that are clicked and booked

booked_pairs <- df[df$booking_bool == 1, c("srch_id", "prop_id")]
clicked_pairs <- df[df$click_bool == 1, c("srch_id", "prop_id")]
all_pairs <- rbind(booked_pairs, clicked_pairs)

clicked_and_booked <- duplicated(all_pairs)[-seq(0,nrow(booked_pairs))]

df_clicked <- df_clicked[!clicked_and_booked, ]

clicked_booked_i <- c(booked_i, clicked_i)
ncb_df <- df[-clicked_booked_i, ]

n <- length(clicked_i)
m <- length(booked_i)

features <- names(df)

click_df <- rbind(df_clicked, dplyr::sample_n(ncb_df, n))
click_remove <- c(seq(1,4), seq(29,52), seq(55,58), 5, 8, 9, 19, 54)
click_df <- click_df[, -click_remove]

booked_df <- rbind(df_booked, dplyr::sample_n(ncb_df, m))
booked_remove <- c(seq(1,4), seq(29,52), seq(55,58), 5, 8, 9, 19, 53)
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

#Clustering

features <- names(df)
prop_features <- features[10:15]
prop_df <- df[, prop_features]
prop_cluster <- kmeans(prop_df, 8)
prop_category <- data.frame(prop_category = as.factor(prop_cluster$cluster))
df <- cbind(df, prop_category)

#Balancing

booked_i <- which(df$booking_bool == TRUE)
booked_srch_ids <- unique(df$srch_id[booked_i])
df_booked <- df[booked_i, ]

clicked_i <- which(df$click_bool == TRUE)
clicked_srch_ids <- unique(df$srch_id[clicked_i])
df_clicked <- df[clicked_i, ]

#Removing rows that are clicked and booked

booked_pairs <- df[df$booking_bool == 1, c("srch_id", "prop_id")]
clicked_pairs <- df[df$click_bool == 1, c("srch_id", "prop_id")]
all_pairs <- rbind(booked_pairs, clicked_pairs)

clicked_and_booked <- duplicated(all_pairs)[-seq(0,nrow(booked_pairs))]

df_clicked <- df_clicked[!clicked_and_booked, ]

#Complete balancing

clicked_booked_i <- c(booked_i, clicked_i)
ncb_df <- df[-clicked_booked_i, ]

n <- length(clicked_i)
m <- length(booked_i)

features <- names(df)

click_df <- rbind(df_clicked, dplyr::sample_n(ncb_df, n))
click_remove <- c(seq(2,4), seq(29,52), seq(55,58), 5, 8, 19, 54)
click_df <- click_df[, -click_remove]

booked_df <- rbind(df_booked, dplyr::sample_n(ncb_df, m))
booked_remove <- c(seq(2,4), seq(29,52), seq(55,58), 5, 8, 19, 53)
booked_df <- booked_df[, -booked_remove]

#Combining

altered_click_df <- click_df
altered_click_df$click_bool <- altered_click_df$click_bool
acdf_names <- names(altered_click_df)
colnames(altered_click_df)[acdf_names == "click_bool"] <- "relevance"


altered_booked_df <- booked_df
altered_booked_df$booking_bool <- altered_booked_df$booking_bool*5
abdf_names <- names(altered_booked_df)
colnames(altered_booked_df)[abdf_names == "booking_bool"] <- "relevance"

combined_df <- rbind(altered_booked_df, altered_click_df)

test_df <- combined_df
predicted_relevance <- predict(logreg, newdata = test_df, type="response")

test_df$predicted_relevance <- predicted_relevance

queries <- unique(test_df$srch_id)

average_ndcg <- 0
for (i in 1:length(queries)){
  print(i)
  query_i <- which(test_df$srch_id == queries[i])
  result <- test_df[query_i, c("srch_id", "prop_id", "relevance")]
  result$prop_id <- as.numeric(result$prop_id)
  test_df_i <- test_df[query_i, ]
  ranking <- as.numeric(test_df_i[order(predicted_relevance[query_i], decreasing=TRUE), ]$prop_id)
  ndcg_i <- opt_ndcg(ranking, result)["nDCG"]
  average_ndcg <- average_ndcg + ndcg_i
}
average_ndcg <- average_ndcg/length(queries)



