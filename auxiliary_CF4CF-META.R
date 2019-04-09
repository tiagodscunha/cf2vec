library(cvTools)
library(labelrank)
library(Kendall)
library(reshape2)
library(ggplot2)
library(StatRank)
library(recommenderlab) #, lib.loc ='/home/tiago-inesc/Dropbox/doutoramento/source_code/meta_level/CF4CF_empirical/libs')
library(data.table) ## 1.9.3
library(splitstackshape)
library(plyr)
library(BBmisc)
library(rrecsys)

source("labelrankingforests-master/RankTrees/RankTrees.R")
source("labelrankingforests-master/RankTrees/PredRTrees.R")
source("labelrankingforests-master/RFR.R")

#corrige um vector de ratings, começando de 5 e seguindo até 1
replaceRating<- function(original_rank,max_zeroes){
  tmp <- replace(original_rank,original_rank==0,NA)
  new_rank <- rank(tmp,na='keep')
  increase_factor <- max_zeroes - max(new_rank, na.rm = T)
  new_rank <- new_rank + increase_factor
  new_rank[which(is.na(new_rank))] = 0
  new_rank
}

#corrige um vector de rankings, começando de 1 e seguindo até 5
replaceRanking<- function(original_rank){
  tmp <- replace(original_rank,original_rank==0,NA)
  new_rank <- rank(tmp,na='keep')
  new_rank[which(is.na(new_rank))] = 0
  new_rank
}

#seleciona um numero "zeroes" de posições do array
#o procedimento repete até encontrar um sample que não seja completamente composto por zeros
sampleRanking <- function(original_rank, zeroes, maxzeroes){
  repeat{
    mask <- sample(rep(0:1, times=c(zeroes,maxzeroes-zeroes)))
    final <- original_rank * mask
    
    if(sum(final) > 0){
      break
    }
  }
  final
}

sampleRankingSequential <- function(original_rank, pos){
  final <- rep(0, length(original_rank))
  final[pos] <- original_rank[pos]
  final
}

#passo preliminar que usa MF para prever elementos em falta 
chaining <- function(model, test_matrix, tmp1){ 
  #print(as(test_matrix,"matrix"))
  
  #preliminary prediction
  prediction1 <- as(recommenderlab::predict(object=model,newdata=test_matrix,n=length(test_matrix),type="ratings"), "matrix")
  #print(prediction1)
  
  #replace input for model with previous prediction
  prediction1[which(is.na(prediction1))] = 0
  tmp1@data[1,] <- prediction1
  test_matrix <- as(tmp1,"realRatingMatrix")
  
  #final prediction
  prediction2 <- as(recommenderlab::predict(object=model,newdata=test_matrix,n=length(active_dataset),type="ratings"), "matrix")
  
  #print(prediction2)
  prediction2
}

#método para criar matrix de targets para LR
createTargetRankings <- function(data){
  #organize rankings - assume fixed order (alphabetical) and assign the corresponding rank value for each pair dataset-algorithm
  algorithms <- sort(unique(unlist(strsplit(levels(data$ranking), split = ","))))
  orders <- lapply(data$ranking, function(x){unlist(strsplit(as.character(x), split = ','))})
  targets <- lapply(orders, function(rank1,rank2){
    unlist(lapply(rank2, function(x){
      positions <- unlist(which(rank1 == x))
      if(length(positions)>0){
        return (unlist(positions))
      }
      else {
        return (length(algorithms)/2) #(length(algorithms)) #last position!  # (length(algorithms)/2) assigning mean ranking improves results for IR but decreases for RP
      }
    }))
  }, rank2=algorithms)
  targets_matrix <- as.matrix(do.call(rbind, targets))
  rownames(targets_matrix) <- data$dataset
  colnames(targets_matrix) <- algorithms
  targets_matrix
}


#método para criar matrix de targets para LR
createTargetRankingsPerf <- function(data,perf,targets_matrix){
  
  new_data <- merge(data,perf, by.x="dataset",by.y="dataset")[,c("dataset","ranking")]
  print(new_data)
  
  performance <- lapply(1:dim(targets_matrix)[1], function(index,perf,targets){
    new_order <- targets[index,]
    
    print(new_order)
    performance_vector <-  as.numeric(unlist(strsplit(as.character(perf[index]),",")))
    
    print(performance_vector)
    
    result <- unlist(lapply(1:length(performance_vector), function(x, vector, ordering){
      vector[ordering[x]]  #warning vem daqui!! - funciona bem mas devia resolver problema
    }, vector = performance_vector, ordering = new_order))
    print(result)
    
    result
  }, targets= targets_matrix, perf=new_data$ranking)
  performance_matrix <- as.matrix(do.call(rbind, performance))
  rownames(performance_matrix) <- data$dataset
  colnames(performance_matrix) <- colnames(targets_matrix)
  performance_matrix
}

#método baseline de LR
average_rankings <- function(targets_matrix,algorithms){
  result <- apply(targets_matrix, 2, mean)
  names(result) <- algorithms
  result
}

#método para criar matrizes de CF a partir de dados de ranking/ratings
createCFmatrix <- function(target){
  old_target <- target
  max_labels <- max(unlist(lapply(old_target$ranking, function(ranking){
    label <- strsplit(toString(ranking), ",")[[1]]
    length(label)
  })))
  
  tops <- lapply(1:max_labels, function(position){
    top <- target
    top$ranking <- as.factor(unlist(lapply(old_target$ranking, function(ranking){
      strsplit(toString(ranking), ",")[[1]][position]
    })))
    top$value <- position
    top
  })
  target <- as.data.frame(rbindlist(tops, fill=TRUE))
  target <- target[which(!is.na(target$ranking)),]
  
  #sort by two columns (to guarantee similar CF matrices!!)
  levels(target$dataset) <- sort(levels(target$dataset))
  levels(target$ranking) <- sort(levels(target$ranking))
  target <- arrange(target,ranking,dataset)
  
  target
}

#método para criar matrizes de CF a partir de dados de performance
createCFmatrixPerformance <- function(target, performances){
  old_target <- target
  max_labels <- max(unlist(lapply(old_target$ranking, function(ranking){
    label <- strsplit(toString(ranking), ",")[[1]]
    length(label)
  })))
  
  tops <- lapply(1:max_labels, function(position){
    top <- target
    top$ranking <- as.factor(unlist(lapply(old_target$ranking, function(ranking){
      strsplit(toString(ranking), ",")[[1]][position]
    })))
    top
  })
  
  tmp <- unlist(lapply(1:max_labels, function(position){
    unlist(lapply(performances$ranking, function(ranking){
      strsplit(toString(ranking), ",")[[1]][position]
    }))
  }))
  
  target <- as.data.frame(rbindlist(tops, fill=TRUE))
  target$value <- tmp
  target <- target[which(!is.na(target$ranking)),]
  
  #sort by two columns (to guarantee similar CF matrices!!)
  levels(target$dataset) <- sort(levels(target$dataset))
  levels(target$ranking) <- sort(levels(target$ranking))
  target <- arrange(target,ranking,dataset)
  
  target
}

#cria repetições do processo para valores diferentes de zeroes para sampling
createExperiment <- function(data,targets,rmatrix,rlandmarkers,method,type,operations, maxzeroes,perf=NULL,goal=NULL,tuningMatrix=NULL,metric="kendall",N=NULL){
  list1 <- list()
  for(zeroes in 1:maxzeroes){ #2:2
    list1 <- append(list1,LOOCV(data=data,targets=targets,rating_matrix=rmatrix, 
                                rating_matrix_landmarkers=rlandmarkers, 
                                method=method, type=type, operations = operations, zeroes = zeroes,
                                maxzeroes = maxzeroes, perf = perf,goal=goal,tuningMatrix=tuningMatrix,
                                metric=metric,N=N))
  }
  list1
}

#agrega resultados das repetições do processo por número de zeroes usados para sampling
decomposeResults <- function(tmp,tag,maxzeroes){
  res <- list()
  for(zeroes in 1:maxzeroes){
    res[paste0(tag,"_",zeroes)] <- list(unlist(tmp[[zeroes]]))
  }
  res
}

#pretty print results
printResults <- function(data){
  x<-lapply(names(data), function(x){
    cat(paste0(x, "\t\t\t", mean(unlist(data[x])), "\t\t+- ", sd(unlist(data[x])), "\n"))
  })
}


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
  
  #print(preds)
  
  final <- unlist(preds[which(preds$kendall == max(preds$kendall)),][1,]$preds)
  
  #print(final)
  
  final
  
}

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
  
  #print(preds)
  
  #print(preds[which(preds$kendall == max(preds$kendall)),])
  final <- unlist(preds[which(preds$kendall == max(preds$kendall)),][1,]$preds)
  
  #print(final)
  
  final
  
}


bestRFRLR <- function(training_LR,testing_LR,target_train_LR,target_test_LR){
  
  gama <- c(0.8,0.9,0.95)
  sizeF <- c(10)
  
  preds <- expand.grid(gama,sizeF)
  colnames(preds) <- c("gama","size")
  
  preds$preds = NA
  preds$kendall = NA
  
  
  
  for(pos in 1:dim(preds)[1]){
    
    pred <- RFR2(dx=training_LR,target_train_LR,testing_LR,target_test_LR, sizeForest=preds[pos,]$size, gama=preds[pos,]$gama)  
    preds[pos,]$preds <- list(pred)
    preds[pos,]$kendall <- Kendall(pred,target_test_LR)$tau
  }
  
  #print(preds)
  #print(preds[which(preds$kendall == max(preds$kendall)),])
  final <- unlist(preds[which(preds$kendall == max(preds$kendall)),][1,]$preds)
  
  #print(final)
  
  final
  
}

#method for training LR models
trainLR <- function(data,targets,method,target_test,algorithms,i,type="NULL"){
  data$dataset <- NULL  ##remove for all strategies
  
  new_data <- data #normalize(data)
  
  training_LR <- as.matrix(new_data[-i,2:dim(new_data)[2]])
  testing_LR <- as.matrix(new_data[i,2:dim(new_data)[2]])
  target_train_LR <- targets[-i,]
  
  if(method == "kNN" ){  
    
    if(type == "hyb"){
      prediction <- bestNNLR(training_LR,testing_LR,target_train_LR,target_test)
    }
    else {
      prediction <- nn_rank(train.x = training_LR, test.x = testing_LR, n = 1, y = target_train_LR)
    }
  }
  if(method == "RFR"){
    prediction <-  bestRFRLR(training_LR,testing_LR,target_train_LR,target_test)
  }
  if(method == "RT"){
    prediction <- bestRTLR(training_LR,testing_LR,target_train_LR,target_test)
  }
  if(method == "baseline"){
    prediction <- rank(average_rankings(target_train_LR,algorithms))
  }
  prediction
}

#method to convert output of CF4CF-ratings to LR rankings
convertRatingsToRankings <- function(original_rank,maxzeroes){
  tmp <- replace(original_rank,original_rank==0,NA)
  new_rank <- rank(tmp,na='keep')
  new_rank <- unlist(lapply(new_rank, function(x,maxzeroes){
    ((maxzeroes - 1) * (maxzeroes - x))/(maxzeroes - 1) + 1
  }, maxzeroes = maxzeroes))
  new_rank[which(is.na(new_rank))] = 0
  new_rank
}

#method to convert output of CF4CF-performance to LR rankings
convertPerformanceToRankings <- function(original_rank){
  new_rank <- rank(-original_rank,na='keep')  #must be negative!
  new_rank
}

#method to convert output of CF4CF-rankings to LR rankings
convertRankingsToRankings <- function(original_rank){
  new_rank <- rank(original_rank,na='keep')
  new_rank
}

#methods for training CF models
trainCF <- function(model,tmp,operations,algorithms,zeroes,method,maxzeroes,
                    data=NULL, targets=NULL, index=NULL, rating_matrix=NULL,target_test_LR=NULL, type = NULL){
  tmp1 <- tmp
  
  threshold <- 1
  if("pivoting" %in% operations || method == "chaining")
    threshold <- length(tmp@data[1,])
  
  predictions <- matrix(NA, nrow = threshold, ncol = (maxzeroes+1))
  
  for(j in 1:threshold){
    active_dataset <- tmp@data[1,][1:(maxzeroes+1)]
    
    if("sampling" %in% operations){
      active_dataset <- sampleRanking(active_dataset, zeroes, maxzeroes+1)
    }
    
    if("samplingSequential" %in% operations){
      active_dataset <- sampleRankingSequential(active_dataset, j)
    }
    
    if("pivoting" %in% operations){
      active_dataset[j] <- NA
    }
    
    if("adjustmentRating" %in% operations){
      active_dataset <- replaceRating(active_dataset, maxzeroes + 1)
    }
    
    if("adjustmentRanking" %in% operations){
      active_dataset <- replaceRanking(active_dataset)
    }
    
    tmp1@data[1,][1:(maxzeroes+1)] <- active_dataset
    
    test_matrix_CF <- as(tmp1,"realRatingMatrix")
    
    if(method == "landmarkers"){
      prediction <- active_dataset
    }
    else if (method == "chaining"){
      prediction <- chaining(model, test_matrix_CF, tmp1)
    }
    else if ("contextual" %in% operations){
      new_rm <- as(as(rating_matrix,"realRatingMatrix"),"matrix")
      new_rm[index,] <- active_dataset
      new_data <- cbind(data,new_rm)
      prediction <- trainLR(new_data,targets,method,target_test_LR,NULL,index,type)
    }
    else {
      prediction <- as(recommenderlab::predict(object=model,newdata=test_matrix_CF,n=length(algorithms),type="ratings"), "matrix")
    }
    
    x <- as.vector(as(test_matrix_CF,"matrix"))[1:(maxzeroes+1)]
    x[which(is.na(x))] = 0
    y <- as.vector(prediction)[1:(maxzeroes+1)]
    y[which(is.na(y))] = 0
    
    if("contextual" %in% operations){
      pred <- prediction
    }
    else if("pivoting" %in% operations || method == "chaining"){
      pred <- y
    }
    else{
      pred <- x + y
    }
    
    if("convertRatings" %in% operations){
      pred <- convertRatingsToRankings(pred, maxzeroes+1)
    }
    
    if("convertPerformance" %in% operations){
      pred <- convertPerformanceToRankings(pred)
    }
    
    if("convertRankings" %in% operations){
      pred <- convertRankingsToRankings(pred)
    }
    
    predictions[j,] <- pred
  }
  
  tmp1 <- apply(predictions,2,sum,na.rm=TRUE)  
  
  if("convertRatingsPivoting" %in% operations){
    tmp1 <- convertRatingsToRankings(tmp1)
  }
  
  return(tmp1)
  
}

performance_at_k_max <- function(values, k){
  max(values[1:k],na.rm = T)
}

performance_at_k_min <- function(values, k){
  min(values[1:k],na.rm = T)
}


#Leave One Out Cross Validation methodology
LOOCV <- function(data=NULL, targets=NULL, rating_matrix=NULL, method=NULL, algorithms=NULL, zeroes=NULL, rating_matrix_landmarkers=NULL, type=NULL, operations=NULL,maxzeroes=NULL, perf=NULL, goal=NULL, tuningMatrix=NULL, predictions=NULL, metric="kendall",N=1){
  
  if(is.null(perf)){
    results <- list()
  }
  else {
    results <- data.frame(matrix(nrow = nrow(data), ncol = dim(targets)[2]))
    rownames(results) <- data$dataset
  }
  
  if("pivoting" %in% operations || type == "mtl" || method == "chaining" || type == "ASLIB" || type=="alors"){
    iterations <- 1
  }
  else {
    iterations <- 10 
  }
  
  cat(paste0(method, " - " , zeroes, "\n"))
  
  for(i in 1:nrow(data)){  
    
    target_test <- targets[i,]
    
    ####################  NOTE use if goal is to compare to original matrix ###################
    #if(type == "mtl"){  #|| "adjustmentRating" %in% operations){
    #   target_test <- targets[i,]
    # }
    # else {
    #   target_test <- as(rating_matrix,"realRatingMatrix")@data[i,]
    # }
    
    #print(paste0("target: ", toString(target_test)))
    
    if(is.null(perf)){
      intermediate_results <- list()
    }
    else {
      intermediate_results <- data.frame(matrix(nrow = iterations, ncol = dim(targets)[2]))
    }
    
    prediction <- NULL
    
    #speed up for cf4cf - avoids training algorithms in each iterations (unnecessary)
    if(type == "cf4cf" || type=="alors"){
      alors=F
      if("alors" %in% operations || type == "alors")
        alors <- T
      
      if(method != "landmarkers" && method != "chaining"){
        train_matrix_CF <- as(rating_matrix[which(rating_matrix$dataset != data[i,]$dataset),],"realRatingMatrix")
        
        if("context_cf4cf" %in% operations){
          tuning_matrix <- as(tuningMatrix[which(tuningMatrix$dataset != data[i,]$dataset),],"realRatingMatrix")
          model <- bestTunedCFModel(train_matrix_CF, method, tune=TRUE,tuning_matrix, alors = alors)  #TODO change afterwards
        }
        else {
          model <- bestTunedCFModel(train_matrix_CF, method, tune=TRUE,train_matrix_CF, alors = alors)  #TODO change afterwards
        }
        
      }
      
      # if(method == "chaining"){
      #   train_matrix_CF <- as(rating_matrix[which(rating_matrix$dataset != data[i,]$dataset),],"realRatingMatrix")
      #   model <- bestTunedCFModel(train_matrix_CF, "ALS", tune=TRUE)  #TODO change afterwards
      # }
      
      all_data <- as(rating_matrix_landmarkers,"realRatingMatrix")
      tmp <- all_data[which(rownames(all_data@data) == data[i,]$dataset),]
      
      
      if(alors){
        source('RF_multioutput.R')
        
        #construir multioutput regressor para mapear metafeatures para dataset latent representation
        #train CF model to obtain latent factor matrices
        
        train_matrix_CF <- as(rating_matrix,"realRatingMatrix")
        new_model <- Recommender(train_matrix_CF, method = "NEW_ALS")
        res <- recommenderlab::predict(object=new_model,newdata=train_matrix_CF)
        
        user_latent_matrix <- res[[1]]
        user_latent_matrix <- user_latent_matrix[!duplicated(rownames(user_latent_matrix)), ]
        user_latent_matrix <- user_latent_matrix[which(rownames(user_latent_matrix) != data[i,]$dataset),]  #remover dataset de teste dos dados de treino
        item_latent_matrix <- res[[2]]
        
        #train regression random forest to learn mapping metafetaures <- user latent space
        training_data <- data[-i,2:dim(data)[2]]
        row.names(training_data) <- data$dataset[-i]
        testing_data <- as.matrix(data[i,2:dim(data)[2]])
        training_target <- user_latent_matrix[order(match(rownames(user_latent_matrix),training_data$dataset)),]
        training_data <- as.matrix(training_data)
        
        #filter data in order to avoid problems with same label in first place
        
        
        prediction_metafeatures <- MultivariateRandomForest::build_forest_predict(
          trainX = training_data,
          trainY = training_target,
          testX = testing_data,
          n_tree = 100,
          m_feature = as.integer(ncol(training_data)/3),
          min_leaf = as.integer(nrow(training_data)/10)
        )
        
        #final prediction
        tmp@data[1,] <- prediction_metafeatures %*% item_latent_matrix
      }
      
    }
    
    if(type == "hyb"){
      all_data <- as(rating_matrix_landmarkers,"realRatingMatrix")
      tmp <- all_data[which(rownames(all_data@data) == data[i,]$dataset),]
    }
    
    for(j in 1:iterations){
      if(type == "ASLIB"){
        max_length <- length(predictions[1,])
        pred <- unlist(predictions[which(predictions$dataset == rownames(data[i,])),2:max_length])
        
        sorted <- sort(pred)
        prediction <- match(sorted,pred)
        
        prediction
      }
      if(type == "mtl" && method!="oracle"){
        prediction <- trainLR(data,targets,method,target_test,algorithms,i)
      }
      else if(type=="alors"){
        prediction <- convertRatingsToRankings(tmp@data[1,], maxzeroes)
        
      }
      else if(type=="cf4cf"){
        prediction <- trainCF(model,tmp,operations,algorithms,zeroes,method,maxzeroes)
      }
      else if(type=="hyb"){
        prediction <- trainCF(NULL,tmp,operations,algorithms,zeroes,method,maxzeroes,
                              data=data, targets=targets, index=i, rating_matrix = rating_matrix, target_test_LR = target_test, type = "hyb")
      }
      else if(method=="oracle"){
        prediction <- target_test
      }
      
      #print(paste0("prediction is ", toString(prediction)))
      
      if(is.null(perf)){
        
        if(metric == "kendall"){
          intermediate_results <- append(intermediate_results, Kendall(prediction,target_test)$tau)
        }
        if(metric == "ndcg"){
          
          #print("prediction original")
          #print(prediction)
          #print("target original")
          #print(as.numeric(target_test))
          
          
          prediction_sorted <- unlist(lapply(1:length(prediction), function(index){which(prediction==index)}))[1:N]
          target_test_sorted <- unlist(lapply(1:length(target_test), function(index){which(target_test==index)}))[1:N]
          
          #print("prediction")
          #print(prediction_sorted)
          #print("target")
          #print(as.numeric(target_test_sorted))
          
          intermediate_results <- append(intermediate_results, nDCG(prediction_sorted,target_test_sorted))
        }
        
        
      }
      else {
        new_performance_vector <- perf[i,order(prediction)]
        
        if(goal=="max"){
          res <- as.numeric(lapply(1:dim(targets)[2], performance_at_k_max, values=new_performance_vector))
        }
        else {
          res <- as.numeric(lapply(1:dim(targets)[2], performance_at_k_min, values=new_performance_vector))
        }
        
        intermediate_results[j,] <- res
      }
    }
    
    
    
    if(is.null(perf)){
      results <- append(results,mean(unlist(intermediate_results)))
    }
    else {
      tmp <- apply(intermediate_results,2,mean,na.rm=T)
      results[i,] <- tmp
    }
    
    
    
  }
  
  if(!is.null(perf)){
    results <- apply(results,2,mean,na.rm=T)
  }
  
  
  
  list(results)
}


trainModelVarIMp <- function(data,rating_matrix,targets){
  
  new_rm <- as(as(rating_matrix,"realRatingMatrix"),"matrix")
  new_data <- normalize(data)
  new_data <- cbind(new_data,new_rm)
  model <-  RFR2_justModels(new_data[,2:dim(new_data)[2]],targets)
  
  model
}
