source("auxiliary_CF4CF-META.R")
source("tuningCF.R")

run_experiment_cf4cf_meta <- function(type,maxzeroes,goal){
  
  targets_filename <- ""
  if(type == "IR"){
    targets_filename <- "targets/IR.csv"
    ratings_matrix <- read.csv("targets/IR_corrected.csv", sep=";", col.names = c("dataset","algorithm","rating"))
    ratings_landmarkers <- read.csv("targets/IR_landmarkers_corrected.csv", sep=";", col.names = c("dataset","algorithm","rating"))
  }
  else {
    targets_filename <- "targets/RP.csv"
    ratings_matrix <- read.csv("targets/RP_corrected.csv", sep=";", col.names = c("dataset","algorithm","rating"))
    ratings_landmarkers <- read.csv("targets/RP_landmarkers_corrected.csv", sep=";", col.names = c("dataset","algorithm","rating"))
  }
  
  metafeatures <- read.csv("embeddings_grid_search/sample_100_wl6/sample_graphs_dims_30_epochs_2000_lr_0.3_batch_256_negsample_10_embeddings.csv", header = F)
  colnames(metafeatures)[1] <- c("dataset")
  targets_matrix <- createTargetRankings(
    merge(
      metafeatures,
      read.csv(targets_filename, sep=";"), 
      by.x="dataset",by.y="dataset")
  )
  
  performance <- read.csv(targets_filename, sep=";")
  performance <- performance[,c(1,4)]
  colnames(performance) <- c("dataset","ranking")
  perf <- createTargetRankingsPerf(metafeatures,performance,targets_matrix)
  
  #cf4cf ratings
  cf4cf_meta_knn <- LOOCV(data=metafeatures,targets=targets_matrix,rating_matrix=ratings_landmarkers, 
                          rating_matrix_landmarkers=ratings_landmarkers, 
                          method="kNN", type="hyb", operations = c("sampling","adjustmentRating","contextual"), zeroes = 1,
                          maxzeroes = maxzeroes, perf = perf,goal=goal)
  cf4cf_meta_rt <- LOOCV(data=metafeatures,targets=targets_matrix,rating_matrix=ratings_landmarkers, 
                         rating_matrix_landmarkers=ratings_landmarkers, 
                         method="RT", type="hyb", operations = c("sampling","adjustmentRating","contextual"), zeroes = 1,
                         maxzeroes = maxzeroes, perf = perf,goal=goal)
  cf4cf_meta_rfr <- LOOCV(data=metafeatures,targets=targets_matrix,rating_matrix=ratings_landmarkers, 
                          rating_matrix_landmarkers=ratings_landmarkers, 
                          method="RFR", type="hyb", operations = c("sampling","adjustmentRating","contextual"), zeroes = 1,
                          maxzeroes = maxzeroes, perf = perf,goal=goal)
  c(
    knn=cf4cf_meta_knn,
    rt=cf4cf_meta_rt,
    rfr=cf4cf_meta_rfr
  )
}


IR <- run_experiment_cf4cf_meta("IR",4,"max")
RP <- run_experiment_cf4cf_meta("RP",8,"min")

printResults(IR)
printResults(RP)

save(IR,RP,file="results/cf4cf_meta_cf2vec_base.Rda")
