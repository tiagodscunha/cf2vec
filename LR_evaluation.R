
#custom LOOCV procedure for LR framework
LOOCV <- function(data,targets, n=1, method, baseline){
  results <- list()
  
  for(i in 1:nrow(data)){  
    training <- as.matrix(data[-i,])
    testing <- as.matrix(data[i,])
    target_train <- targets[-i,]
    target_test <- targets[i,]
    
    if(method == "kNN"){
      prediction <- bestNNLR(training,testing,target_train,target_test)
    }
    else if(method == "RT"){
      prediction <- bestRTLR(training,testing,target_train,target_test)
    }
    else if(method == "RFR"){
      prediction <- bestRFRLR(training,testing,target_train,target_test)
    }
    else{ #baseline
      prediction <- average_rankings(target_train,baseline)
    }
    
    if(length(prediction) == length(target_test)){
      results <- append(results, Kendall(prediction,target_test)$tau)
    }
    else {
      print("ignored this instance")
      results <- append(results,-1)
    }
  }
  
  unlist(results)  
}

rank_evaluation <- function(dataset, ranking, strategy, onlyBaseline=F){
  
  #create dataset
  data <- merge(dataset,ranking, by.x="dataset",by.y="dataset")  
  
  
  #organize rankings - assume fixed order (alphabetical) and assign the corresponding rank value for each pair dataset-algorithm
  algorithms <- sort(unique(unlist(strsplit(levels(data$ranking), split = ","))))
  orders <- lapply(data$ranking, function(x){unlist(strsplit(as.character(x), split = ','))})
  levels_ranking <- lapply(data$levels, function(x){unlist(strsplit(as.character(x), split = ','))})
  
  targets <- lapply(1:length(orders), function(index1,rank2,levels_ranking){
    rank1 <- orders[[index1]]
    unlist(lapply(rank2, function(x, levels_ranking){
      positions <- unlist(which(rank1 == x))
      if(length(positions)>0){
        return (as.numeric(levels_ranking[[index1]][positions]))
      }
      else {
        return (length(algorithms)/2) #(length(algorithms)) #last position!  # (length(algorithms)/2) assigning mean ranking improves results for IR but decreases for RP
      }
    }, levels_ranking=levels_ranking))
  }, rank2=algorithms, levels_ranking=levels_ranking)
  
  targets_matrix <- as.matrix(do.call(rbind, targets))
  
  #experiments
  if(onlyBaseline){
    performance <- 
      LOOCV(
        data=dataset[,2:dim(dataset)[2]],
        targets=targets_matrix,
        method="baseline",
        baseline=algorithms)
    
    output <- data.frame(
      baseline=performance
    )
  }
  else{
    performance <- 
      LOOCV(
        data=dataset[,2:dim(dataset)[2]],
        targets=targets_matrix,
        method="kNN",
        baseline=NA)
    
    output <- data.frame(
      baseline=performance
    )
    colnames(output) <- c(strategy)
  }
  
  # performance_rankingTrees <- 
  #   LOOCV(
  #     data=dataset[,2:dim(dataset)[2]],
  #     targets=targets_matrix,
  #     method="RT",
  #     baseline=algorithms)
  # 
  # performance_RFR <- 
  #   LOOCV(
  #     data=dataset[,2:dim(dataset)[2]],
  #     targets=targets_matrix,
  #     method="RFR",
  #     baseline=algorithms)
  
  output
}