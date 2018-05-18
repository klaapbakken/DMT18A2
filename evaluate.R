source("ndcg.R")
library(foreach)
library(iterators)

query_evaluation <- function(df, query){
  #Extracting relevant columns from df, passing them to nDCG()
  query_i <- which(df$srch_id == query)
  query_df <- df[query_i, c("prop_id", "relevance",
                             "predicted_relevance")]
  ranking <- query_df[order(query_df$predicted_relevance, decreasing=TRUE), ]$prop_id
  result <- query_df[, -which(colnames(query_df) == "predicted_relevance")]
  return(ndcg(ranking, result))
}

evaluate <- function(df){
  #Columns of df should be as.numeric for "srch_id" and "prop_ids".
  #Getting unique queries
  queries <- iter(unique(df$srch_id))
  #Calculating nDCG for each query
  ndcg_vector <- foreach(query=queries, .combine='c', .inorder=FALSE) %do% {
    query_evaluation(df, query)
  }
  return(sum(ndcg_vector)/length(unique(df$srch_id)))
}
