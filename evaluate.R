source("ndcg.R")
library(foreach)
library(iterators)

query_extraction <- function(df, query){
  query_i <- which(df$srch_id == query)
  query_df <- df[query_i, c("prop_id", "relevance",
                             "predicted_relevance")]
  ranking <- query_df[order(query_df$predicted_relevance, decreasing=TRUE), ]$prop_id
  result <- query_df[, -which(colnames(query_df) == "predicted_relevance")]
  return(ndcg(ranking, result))
}

evaluate <- function(df){
  #All numeric columns for relevant columns in df
  queries <- iter(unique(df$srch_id))
  ndcg_vector <- foreach(query=queries, .combine='c', .inorder=FALSE) %do% {
    query_extraction(df, query)
  }
  return(sum(ndcg_vector)/length(unique(df$srch_id)))
}

test_df$srch_id <- as.numeric(test_df$srch_id)
test_df$prop_id <- as.numeric(test_df$prop_id)
evaluate(test_df)
