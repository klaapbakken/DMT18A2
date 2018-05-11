#LOad
rm(list = ls())
load("../data/training_processed.rda")
features <- names(training)
df <- training
df$comp1_rate <- as.integer(df$comp1_rate)
df$comp1_inv <- as.integer(df$comp1_inv)
df$comp1_rate_percent_diff <- as.integer(df$comp1_rate_percent_diff)
rm(training)

#Split
srch_ids <- df$srch_id
srch_ids.unique <- unique(srch_ids)
data_split <- 0.01
n_train <- floor(length(srch_ids.unique)*data_split)
n_test <- length(srch_ids.unique) - n_train

train_ids <- sample(srch_ids.unique, n_train)
test_ids <- subset(srch_ids.unique, !srch_ids.unique %in% train_ids)

train_df <- subset(df, srch_id %in% train_ids)
identical(sort(unique(train_df$srch_id)), sort(train_ids))

test_df <- subset(df, srch_id %in% test_ids)

rm(df)
df <- train_df
rm(train_df)

#Data frame with columns that need to treated

na_count <- colSums(is.na(df))
na_features <- names(na_count[na_count > 0])
na_indices <- which(features %in% na_features)

non_na_features <- setdiff(features, na_features)
non_na_indices <- which(features %in% non_na_features)

non_na_df <- df[, non_na_indices]
na_df <- df[, na_indices]

#Competitor data

comp_indices <- which(na_features %in% features[29:52])
comp_features <- names(na_df[, comp_indices])
comp_df <- na_df[, comp_indices]

rate_i <- which(comp_features %in% comp_features[seq(1, length(comp_features), 3)])
inv_i <- which(comp_features %in% comp_features[seq(2, length(comp_features), 3)])
rpd_i <- which(comp_features %in% comp_features[seq(3, length(comp_features), 3)])

n <- nrow(comp_df)
lowest_price <- logical(n)
only_avail <- logical(n)

#for (i in 1:n){
#  lowest_price[i] <- (min(comp_df[i, rate_i], na.rm=TRUE) > -1 && any(!is.na(comp_df[i, rate_i])))
#  only_avail[i] <- (min(comp_df[i, inv_i], na.rm=TRUE) == 1 && any(!is.na(comp_df[i, rate_i])))
#}

lp_func <- function(x) (min(x, na.rm=TRUE) > -1 && any(!is.na(x)))
oa_func <- function(x) (min(x, na.rm=TRUE) == 1 && any(!is.na(x)))
lowest_price <- apply(comp_df[, rate_i], 1, lp_func)
only_avail <- apply(comp_df[, inv_i], 1, oa_func)


add_comp_df <- data.frame(lowest_price, only_avail)
save(add_comp_df, file=".rda")
imputed_comp_df <- comp_df
imputed_comp_df[is.na(imputed_comp_df)] <- 0
new_comp_df <- cbind(imputed_comp_df, add_comp_df)


#Visitor history

visitor_indices <- which(na_features %in% features[5:7])
visitor_features <- names(na_df[, visitor_indices])
visitor_df <- na_df[, visitor_indices]

#visitor_means <- apply(visitor_df[, visitor_indices], 2, mean, na.rm=TRUE)
#visitor_df$visitor_hist_starrating[is.na(visitor_df$visitor_hist_starrating)] <- visitor_means[1]
#visitor_df$visitor_hist_adr_usd[is.na(visitor_df$visitor_hist_adr_usd)] <- visitor_means[2]

all_na <- function(x) all(is.na(x))
new_visitor <- apply(visitor_df[, visitor_indices], 1, all_na)

cutoff <- function(x){
  lower <- which(x < 0)
  higher <- which(x > 5)
  ret_x <- x
  ret_x[lower] <- 0
  ret_x[higher] <- 5
  return (ret_x)
}
  
  
visitor_stars <- visitor_df$visitor_hist_starrating[which(!is.na(visitor_df$visitor_hist_starrating))]
stars_fit <- fitdistr(visitor_stars, 'normal')
norm_para <- stars_fit$estimate

hist(visitor_stars, prob=TRUE)
curve(dnorm(x, norm_para[1], norm_para[2]), col = 2, add=TRUE)

visitor_stars_complete <- visitor_df$visitor_hist_starrating
vsc_nans <- sum(is.na(visitor_stars_complete))
star_samples <- rnorm(vsc_nans, norm_para[1], norm_para[2])
visitor_stars_complete[is.na(visitor_stars_complete)] <- cutoff(star_samples)

visitor_usd <- visitor_df$visitor_hist_adr_usd[which(!is.na(visitor_df$visitor_hist_adr_usd))]
usd_fit <- fitdistr(visitor_usd, 'weibull')
weib_para <- usd_fit$estimate

hist(visitor_usd, prob=TRUE)
curve(dweibull(x, weib_para[1], weib_para[2]), col = 2, add=TRUE)

visitor_usd_complete <- visitor_df$visitor_hist_adr_usd
vuc_nans <- sum(is.na(visitor_usd_complete))
usd_samples <- rweibull(vuc_nans, weib_para[1], weib_para[2])
visitor_usd_complete[is.na(visitor_usd_complete)] <- usd_samples

hist(visitor_usd, prob=TRUE)
hist(visitor_usd_complete, prob=TRUE)

hist(visitor_stars, prob=TRUE)
hist(visitor_stars_complete, prob=TRUE)

add_visitor_df <- data.frame(visitor_hist_adr_usd=visitor_usd_complete,
                             visitor_hist_starrating=visitor_stars_complete,
                             new_visitor)
save(add_visitor_df, file="add_visitor_df.rda")

#Property

property_indices <- which(na_features %in% features[9:15])
property_features <- names(na_df[, property_indices])

low_review_score <- quantile(na_df$prop_review_score, probs = c(0.1), na.rm=TRUE)

#hist(na_df$prop_review_score[!is.na(na_df$prop_review_score)])

#dist_func <- ecdf(na_df$prop_review_score[!is.na(na_df$prop_review_score)])

#hist(na_df$prop_location_score2)

na_score_indices <- which(is.na(na_df$prop_location_score2))
non_na_score_indices <- which(!is.na(na_df$prop_location_score2))

#Find location in non_na_indices with score1 similar to current na_index score1.
#Copy non_na score to current index 

#Create linear model of score1 with response score 2. Predict score 2

score1 <- non_na_df$prop_location_score1[non_na_score_indices]
score2 <- na_df$prop_location_score2[non_na_score_indices]
score_df <- data.frame(score1, score2)
score_lm <- lm(score2 ~ score1, data=score_df)

score_cov_df <- data.frame(score1 = non_na_df$prop_location_score1[na_score_indices])

score_predictions <- predict(score_lm, score_cov_df)

prop_df <- na_df[, property_indices]
prop_df$prop_review_score[is.na(prop_df$prop_review_score)] <- low_review_score
prop_df$prop_location_score2[is.na(prop_df$prop_location_score2)] <- score_predictions

imputed_review_score_df <- prop_df[,1, drop=FALSE]
save(imputed_review_score_df, file="imputed_review_score_df.rda")

#Distance

dist <- na_df$orig_destination_distance
dist[is.na(dist)] <- mean(dist, na.rm=TRUE)
dist_df <- data.frame(dist)

#Query affinity

no_na <- function(x) !any(is.na(x))
covariates <- c(features[10:15], features[17:18], features[26])
complete_i <- apply(df[, covariates], 1, no_na)
complete_i <- which(complete_i)
complete_rows <- df[complete_i, covariates]
complete_rows$prop_starrating <- as.factor(non_na_covs$prop_starrating)

am <- lm(exp(srch_query_affinity_score) ~ (.), data=complete_rows)

affinity <- exp(na_df$srch_query_affinity_score)
affinity[is.na(affinity)] <- 0
affinity_df = data.frame(affinity)

# Wrap up

complete_df <- cbind(non_na_df, new_comp_df, visitor_df, prop_df, dist_df, affinity_df)
