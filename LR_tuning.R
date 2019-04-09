

#Grid search method for NN algorithm
bestNNLR <- function(training_LR,testing_LR,target_train_LR,target_test_LR){
  
  k_vals <- 1:20
  
  preds <- data.frame(
    k = k_vals,
    preds = NA,
    kendall = NA
  )
  
  for(i in k_vals){
    pred <- nn_rank(train.x = training_LR, test.x = testing_LR, n = 1, y = target_train_LR, k=i)
    preds[i,]$preds <- list(pred)
    preds[i,]$kendall <- Kendall(pred,target_test_LR)$tau
  }

  final <- unlist(preds[which(preds$kendall == max(preds$kendall)),][1,]$preds)
  final
  
}

#Grid search method for RT algorithm
bestRTLR <- function(training_LR,testing_LR,target_train_LR,target_test_LR){
  
  k_vals <- c(0.8,0.9,0.95)
  
  preds <- data.frame(
    k = k_vals,
    preds = NA,
    kendall = NA
  )
  
  for(pos in 1:length(k_vals)){
    
    i <- k_vals[pos]
    
    model <- rankTrees(training_LR,target_train_LR, gama=i)
    pred <- as.numeric(PredRTrees(model,testing_LR))
    preds[pos,]$preds <- list(pred)
    preds[pos,]$kendall <- Kendall(pred,target_test_LR)$tau
  }

  print(preds[which(preds$kendall == max(preds$kendall)),])
  final <- unlist(preds[which(preds$kendall == max(preds$kendall)),][1,]$preds)
  final
}

#Grid search method for RFR algorithm
bestRFRLR <- function(training_LR,testing_LR,target_train_LR,target_test_LR){
  
  gama <- c(0.8,0.9,0.95)
  sizeF <- c(50,100,200)
  
  preds <- expand.grid(gama,sizeF)
  colnames(preds) <- c("gama","size")
  
  preds$preds = NA
  preds$kendall = NA
  
  for(pos in 1:dim(preds)[1]){
    
    pred <- RFR2(dx=training_LR,target_train_LR,testing_LR,target_test_LR, sizeForest=preds[pos,]$size, gama=preds[pos,]$gama)  
    preds[pos,]$preds <- list(pred)
    preds[pos,]$kendall <- Kendall(pred,target_test_LR)$tau
  }
  
  print(preds[which(preds$kendall == max(preds$kendall)),])
  final <- unlist(preds[which(preds$kendall == max(preds$kendall)),][1,]$preds)
  final
}

#baseline algorithm (does not require tuning)
average_rankings <- function(targets_matrix,algorithms){
  result <- apply(targets_matrix, 2, mean)
  names(result) <- algorithms
  result
}

