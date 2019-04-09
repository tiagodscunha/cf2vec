library(BBmisc)

evaluateLOOCV <- function(train_matrix_CF, method, grid_search){

  e <- recommenderlab::evaluationScheme(train_matrix_CF, method="cross-validation",
                        k=3, given=3)
  results <- list()
  
  for(i in 1:nrow(grid_search)){

    r <- Recommender(getData(e, "train"), method, param=convertColsToList(grid_search[i,])) 

    p <- recommenderlab::predict(r, getData(e, "known"), type="ratings")

    res <- cbind(grid_search[i,], as.list(calcPredictionAccuracy(p, getData(e, "unknown"))))
    
    results <- rbind(results,res)
  }
  
  results
}


bestTunedCFModel <- function(train_matrix_CF, method, tune=T,tuningMatrix=NULL,alors=F){
  
  if(tune){
    if(method == "UBCF"){ 
      
      # BIN_UBCF_param <- list(
      #   method = "jaccard",
      #   nn = 25,
      #   weighted = TRUE,
      #   sample = FALSE
      # )
      
      grid_search <- expand.grid(
        method = c("jaccard", "cosine"),
        nn = c(2,3,4,5,6,7,8,9,10,12,15,20,25)
      )
      
      limit = 2
      
    }
    
    if(method == "ALS"){
      
      # .REAL_ALS_params <- list(
      #   normalize = NULL,
      #   lambda = 0.1,
      #   n_factors = 10,
      #   n_iterations = 10,
      #   min_item_nr = 1,
      #   seed = NULL
      # )
  
      grid_search <- expand.grid(
        lambda = c(0.01, 0.1),
        n_factors = c(10),
        n_iterations = c(100)
      )
      
      limit = 3
    
    }
    
    
    ev <- evaluateLOOCV(tuningMatrix, method, grid_search)
    ev <- ev[which(!is.na(ev$RMSE)),]

    bestParams <- convertColsToList(ev[which(ev$RMSE == min(ev$RMSE))[1],c(1:limit)])

    model1 <- NULL
    model <- Recommender(train_matrix_CF, method = method, param = bestParams)
    #if(alors){
    #  model1 <- Recommender(train_matrix_CF, method = "NEW_ALS", param = bestParams)
    #}
  }
  else {
    model <- Recommender(train_matrix_CF, method = method)
    #if(alors){
    #  model1 <- Recommender(train_matrix_CF, method = "NEW_ALS")
    #}
  }

  return(model)
}

