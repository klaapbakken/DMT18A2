test_result <- data.frame(srch=rep(7,10), prop_id=sample(1:200, 10),
                   relevance=sample(c(rep(0, 9), 1), 10))
test_ranking <- sample(test_result$prop_id, 10)


ndcg <- function(ranking, result){
  #Highest ranked prop_id first in ranking
  #Result is actual relevance of properties in search query
  #Result is data frame containg srch_id (constant), prop_id and score
  relevance_of <- function (prop_id) result$relevance[result$prop_id == prop_id]
  rel <- numeric(length(ranking))
  dcg_term <- numeric(length(ranking))
  for (i in 1:length(ranking)){
    rel[i] <- relevance_of(ranking[i])
    dcg_term[i] <- rel[i]/log2(i+1)
  }
  CG <- cumsum(rel)
  DCG <- cumsum(dcg_term)
  
  ordered_result <- test_result[order(test_result$relevance, decreasing=TRUE), ]
  
  idcg_term <- numeric(length(result$relevance))
  for (i in 1:length(result$relevance)){
    print(relevance_of(ordered_result$prop_id[i]))
    idcg_term[i] <- (2^relevance_of(ordered_result$prop_id[i]) - 1)/log2(i+1)
  }
  
  IDCG <- cumsum(idcg_term)
  
  nDCG <- DCG/IDCG
  
  return(data.frame(CG, DCG, nDCG))
}

ndcg(test_ranking, test_result)
