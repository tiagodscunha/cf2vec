library(cvTools)
library(labelrank)
library(Kendall)
library(reshape2)

source("labelrankingforests-master/RankTrees/RankTrees.R")
source("labelrankingforests-master/RankTrees/PredRTrees.R")
source("labelrankingforests-master/RFR.R")
source("LR_tuning.R")
source("LR_evaluation.R")
source("auxiliary.R")

#impact on the baselevel experiments
#due to the huge amount of results available, one has selected a cf2vec representation (i.e. embeddings_grid_search/sample_100_wl6/sample_graphs_dims_30_epochs_2000_lr_0.3_batch_256_negsample_10_embeddings.csv")
run_experiment <- function(type){
  
  if(type == "IR"){
    B <- read.csv("metafeatures_landmarkers/B_IR_final.csv", sep=";")  #landmarkers datasets have NAs
    targets <- read.csv("targets/IR.csv", sep=";")
    goal = "max"
  }
  else{
    B <- read.csv("metafeatures_landmarkers/B_RP_final.csv", sep=";")
    targets <- read.csv("targets/RP.csv", sep=";")
    goal = "min"
  }
  
  A <- read.csv("metafeatures_statistical/mf_final.csv", sep=";")
  C <- read.csv("metafeatures_graph/graph_metafeatures_final.csv", sep=",")
  D <- mergeUnifiedDataset(A,B,C)  
  rm(A,B,C)
  
  cf4vec <- read.csv("embeddings_grid_search/sample_100_wl6/sample_graphs_dims_30_epochs_2000_lr_0.3_batch_256_negsample_10_embeddings.csv", header = F)
  tmp <- colnames(cf4vec)
  tmp[1] <- "dataset"
  colnames(cf4vec) <- tmp
  
  keepcolumn <- c("dataset","performance")
  perf_values <- targets[,which(colnames(targets) %in% keepcolumn)]
  
  metafeatures_result <- rank_evaluation_baselevel_impact(D,targets,perf_values,goal,"MF")
  baseline_result <- rank_evaluation_baselevel_impact(D,targets,perf_values,goal,"MF",onlyBaseline = T)
  cf4vec_result <- rank_evaluation_baselevel_impact(cf4vec,targets,perf_values,goal,"cf4vec")
  
  
  list(metafeatures_result,baseline_result,cf4vec_result)
}

IR <- run_experiment("IR")
RP <- run_experiment("RP")

save(IR, RP, file="results/impact_baselevel_performance.Rda")