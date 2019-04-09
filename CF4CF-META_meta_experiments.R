source("auxiliary_CF4CF-META.R")
source("tuningCF.R")

run_experiment_cf4cf_meta <- function(type,maxzeroes){
  
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
  
  #cf4cf ratings
  cf4cf_meta_knn <- createExperiment(metafeatures,targets_matrix,ratings_landmarkers,ratings_landmarkers,"kNN","hyb",c("sampling","adjustmentRating","contextual"),maxzeroes)
  #cf4cf_meta_rt <- createExperiment(metafeatures,targets_matrix,ratings_landmarkers,ratings_landmarkers,"RT","hyb",c("sampling","adjustmentRating","contextual"),maxzeroes)
  #cf4cf_meta_rfr <- createExperiment(metafeatures,targets_matrix,ratings_landmarkers,ratings_landmarkers,"RFR","hyb",c("sampling","adjustmentRating","contextual"),maxzeroes)
  
  c(
    decomposeResults(cf4cf_meta_knn,"cf4cf_meta_knn",maxzeroes)
    #decomposeResults(cf4cf_meta_rt,"cf4cf_meta_rt",maxzeroes),
    #decomposeResults(cf4cf_meta_rfr,"cf4cf_meta_rfr",maxzeroes)
  )
}


IR <- run_experiment_cf4cf_meta("IR",4)
RP <- run_experiment_cf4cf_meta("RP",8)

printResults(IR)
printResults(RP)

save(IR,RP,file="results/cf4cf_meta_cf2vec_new.Rda")
