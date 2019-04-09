library(Kendall)

#standard Random Forest model
RFR <- function(dx,dy,x,y,gama = 0.98, splitCrit = "CS"){

  randfres <- NULL
  res <- 0
  sizeForest <- 100
  for (f in 1:sizeForest) {
    set.seed(f)
    subset <- sample(1:nrow(dx),nrow(dx),replace=T)
    model <- rankTrees(dx[subset,,drop=FALSE], dy[subset,,drop=FALSE], gama = gama, crit = splitCrit)
    prediction <- unlist(PredRTrees(model, x))
    res <- res + Kendall(prediction,y)$tau
  }

  res/sizeForest
}

#same as before but returns model instead of predictions (useful for feature importance)
RFR2_justModels <- function(dx,dy,gama = 0.98, sizeForest = 100, splitCrit = "CS"){
  
  predictions <- list()
  res <- 0
  
  models <- data.frame(
    features <- c(),
    rank <- c()
  )
  for (f in 1:sizeForest) {
    set.seed(f)
    subset <- sample(1:nrow(dx),nrow(dx),replace=T)
    model <- rankTrees(dx[subset,,drop=FALSE], dy[subset,,drop=FALSE], gama = gama, crit = splitCrit)

    all_features <- colnames(dx)
    browser()
    features <- unique(model$Atrib[model$Atrib != " "])
    
    if(!identical(features, character(0))){
      remaining <- all_features[-match(features,all_features)] #all_features[!all_features %in% features]  
      
      tmp <- data.frame(
        metafeatures = c(features,remaining),
        rank = c(c(1:length(features)),rep(length(all_features),length(all_features)-length(features)))
      )
    }
    else {

      tmp <- data.frame(
        metafeatures = remaining,
        rank = rep(length(all_features),length(remaining))
      )
    }

    models <- rbind(models,tmp)
  }
  
  models
}

#modified RFR algorithm with average ranking to create predictions
RFR2 <- function(dx,dy,x,y,gama = 0.95, splitCrit = "CS", sizeForest = 10){
  
  predictions <- list()
  res <- 0
  
  for (f in 1:sizeForest) {
    set.seed(f)
    subset <- sample(1:nrow(dx),nrow(dx),replace=T)
    model <- rankTrees(dx[subset,,drop=FALSE], dy[subset,,drop=FALSE], gama = gama, crit = splitCrit)
    prediction <- unlist(PredRTrees(model, x))
    predictions <- append(predictions, list(prediction))
    res <- res + Kendall(prediction,y)$tau
  }

  predictions_matrix <- as.matrix(do.call(rbind,predictions))
  pred <- unlist(lapply(apply(predictions_matrix, 2, mean), round))   #average ranking of all predictions
  pred

}