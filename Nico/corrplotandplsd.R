load("processed_missing_value.rda")

input_data <- final.table

library(corrplot)
library(tidyr)
library(dplyr)
library(caret)


corrplot(as.matrix(final.table[,sapply(final.table,is.numeric)]),is.corr = FALSE, method = "square")

input_data.click <- input_data[input_data$click_bool==1 & input_data$booking_bool == 0,]
input_data.book <- input_data[input_data$click_bool==1 & input_data$booking_bool == 1,] 
input_data.notbook <- input_data[input_data$click_bool==0 & input_data$booking_bool == 0,]
N <- dim(input_data.click)[1]

stratified <- rbind(sample_n(input_data.click,N,replace = TRUE),sample_n(input_data.book,N,replace = TRUE),sample_n(input_data.notbook,N,replace = TRUE))


myfolds <- createMultiFolds(stratified$responses, k = 5, times = 10)
control <- trainControl("repeatedcv", index = myfolds, selectionFunction = "oneSE")
data_mod1 <- stratified[,-c(1,2,3,4,5,8,9,14,16,53,54,55,56,59)]

mod1 <- train(responses ~ ., data = data_mod1[,sapply(data_mod1, is.numeric)],
              method = "pls",
              metric = "RMSE",
              tuneLength = 20,
              trControl = control,
              preProc = c("zv","center","scale"))


plot(mod1)
