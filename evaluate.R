source("ndcg.R")
library(foreach)
library(iterators)

query_evaluation <- function(df, query, save_ranking=FALSE){
  #Extracting relevant columns from df, passing them to nDCG()
  query_i <- which(df$srch_id == query)
  query_df <- df[query_i, c("prop_id", "relevance",
                             "predicted_relevance")]
  ranking <- query_df[order(query_df$predicted_relevance, decreasing=TRUE), ]$prop_id
  result <- query_df[, -which(colnames(query_df) == "predicted_relevance")]
  if (save_ranking==TRUE){
  write.table(data.frame(SearchId=rep(query, length(ranking)), PropertyId=ranking),
              "ranking.csv", sep = ",", col.names = F, row.names = F, append=T)
    return(TRUE)
  }
  else{
  return(ndcg(ranking, result))
  }
}

evaluate <- function(df, save_ranking = FALSE){
  if(save_ranking==TRUE){
    fn <- "ranking.csv"
    if (file.exists(fn)){
      file.remove(fn)
    }
    write.table(data.frame(SearchId=integer(), PropertyId=integer()),
                "ranking.csv", sep = " ,", col.names=T, row.names=F, append=F)
    queries <- unique(df$srch_id)
    for (i in 1:length(queries)){
      query_evaluation(df, queries[i], save_ranking = TRUE)
    }
    return(1)
  }
  else{
  #Columns of df should be as.numeric for "srch_id" and "prop_ids".
  #Getting unique queries
  queries <- iter(unique(df$srch_id))
  #Calculating nDCG for each query
  ndcg_vector <- foreach(query=queries, .combine='c', .inorder=FALSE) %do% {
    query_evaluation(df, query, save_ranking=FALSE)
  }
  return(sum(ndcg_vector)/length(unique(df$srch_id)))
  }
}
