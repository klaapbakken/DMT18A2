test_result <- data.frame(whatever=rep(0, 10), srch_id=rep(7,10), prop_id=sample(1:100, 10),
                   relevance=sample(c(rep(0, 10), rep(1, 9), 5), 10))
test_ranking <- sample(test_result$prop_id, 10)


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

ndcg(test_ranking, test_result)

best_ranking <- test_result$prop_id[order(test_result$relevance, decreasing=TRUE)]

ndcg(best_ranking, test_result)

opt_ndcg(best_ranking, test_result)
