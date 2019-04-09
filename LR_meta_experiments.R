library(cvTools)
library(labelrank)
library(Kendall)
library(reshape2)
library(ggplot2)

source("labelrankingforests-master/RankTrees/RankTrees.R")
source("labelrankingforests-master/RankTrees/PredRTrees.R")
source("labelrankingforests-master/RFR.R")
source("LR_tuning.R")
source("LR_evaluation.R")
source("auxiliary.R")

#performs metalevel evaluation for 
#note that one has to manually define which is the configuration selected (sub-folder from embeddings_grid_search)
#also, one must define an appropriate file name on which to save the results
run_experiment <- function(type){
  
  if(type == "IR"){
    B <- read.csv("metafeatures_landmarkers/B_IR_final.csv", sep=";")  #landmarkers datasets have NAs
    targets <- read.csv("targets/IR.csv", sep=";")
  }
  else{
    B <- read.csv("metafeatures_landmarkers/B_RP_final.csv", sep=";")
    targets <- read.csv("targets/RP.csv", sep=";")
  }
  
  A <- read.csv("metafeatures_statistical/mf_final.csv", sep=";")
  C <- read.csv("metafeatures_graph/graph_metafeatures_final.csv", sep=",")
  D <- mergeUnifiedDataset(A,B,C)  
  rm(A,B,C)
  
  metafeatures_result <- rank_evaluation(D,targets,"MF")
  baseline_result <- rank_evaluation(D,targets,"MF",onlyBaseline = T)
  alternative_table <- cbind(metafeatures_result,baseline_result)
  
  embeddings_folder <- "embeddings_grid_search/sample_100_wl6/"  #change appropriately
  
  file.names <- dir(embeddings_folder, pattern =".csv")
  for(i in 1:length(file.names)){
    print(file.names[i])
    E <- read.csv(paste0(embeddings_folder,file.names[i]), header = F)
    colnames(E) <- renameCols(E)
    E_result <- rank_evaluation(E,targets,file.names[i])
    alternative_table <- cbind(alternative_table, E_result)
  }
  
  alternative_table
}

IR <- run_experiment("IR")
RP <- run_experiment("RP")

save(IR, RP, file="embeddings_grid_search_results/grid_search_detailed_graphics_sample_100_wl6_new.Rda") #change appropriately
