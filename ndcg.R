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
