library(cvTools)
library(labelrank)
library(Kendall)
library(reshape2)
library(ggplot2)
library(BBmisc)
library(ggrepel)

source("auxiliary.R")


params2Vec <- function(val){
  val <- gsub("sample_graphs_100","",val)
  val <- gsub("sample_graphs","",val)
  tmp <- unlist(strsplit(toString(val),"[_]"))
  res <- c(as.numeric(tmp[3]),as.numeric(tmp[5]),as.numeric(tmp[7]),as.numeric(tmp[9]),as.numeric(tmp[11]))
  res
}

createDF <- function(IR,RP,wl,removeOthers = TRUE){
  final <- rbind(
    apply(IR,2,mean),
    apply(RP,2,mean)
  )
  row.names(final) <- c("IR","RP")
  final <- t(final)
  
  extra_cols <- rbind(
    rep(0,5),rep(0,5)
  )
  
  for(i in 3:dim(final)[1]){
    extra_cols <- rbind(extra_cols,params2Vec(row.names(final)[i]))
  }
  
  
  
  colnames(extra_cols) <- c("dims","epochs","lr","batch","negsample")
  final <- cbind(final,extra_cols)
  final <- as.data.frame(final)
  final$dims <- as.factor(final$dims)
  final$epochs <- as.factor(final$epochs)
  final$lr <- as.factor(final$lr)
  final$batch <- as.factor(final$batch)
  final$negsample <- as.factor(final$negsample)
  final$wl <- wl
  
  if(removeOthers){
    final <- final[-which(rownames(final) == "MF"),]
    final <- final[-which(rownames(final) == "baseline"),]
  }
  final 
}



load("embeddings_grid_search_results/grid_search_detailed_graphics_sample_100_wl3.Rda")
df1 <- createDF(IR,RP,3)

load("embeddings_grid_search_results/grid_search_detailed_graphics_sample_100_wl4.Rda")
df2 <- createDF(IR,RP,4)

load("embeddings_grid_search_results/grid_search_detailed_graphics_sample_100_wl5.Rda")
df3 <- createDF(IR,RP,5)

load("embeddings_grid_search_results/grid_search_detailed_graphics_sample_100_wl6.Rda")
df4 <- createDF(IR,RP,6)

load("embeddings_grid_search_results/grid_search_detailed_graphics_sample_100_wl7.Rda")
df5 <- createDF(IR,RP,7)

load("embeddings_grid_search_results/grid_search_detailed_graphics_sample_100_wl8.Rda")
df6 <- createDF(IR,RP,8)


final <- rbind(df1,df2,df3,df4,df5,df6)
final$wl <- as.factor(final$wl)

rm(df1,df2,df3,df4,IR,RP)

########## select best models

g1 <- ggplot(final,aes(x=RP, y=IR)) +
  geom_point(aes(shape = wl, colour=wl), size=1) + 
  theme_bw() + 
  theme(legend.title=element_blank())
g1

g2 <- ggplot(final,aes(x=RP, y=IR)) +
  geom_point(aes(shape = dims, colour=wl), size=2) + 
  theme_bw() + 
  guides(colour=guide_legend(title=expression(delta))) + 
  guides(shape=guide_legend(title=expression(sigma))) + 
  coord_cartesian(xlim=c(0.77,0.82),ylim=c(0.79, 0.86))
g2









best <- final[  with(final, order(-IR, -RP)), ][2,]  #manually found 

dataset <- read.csv("embeddings_grid_search/sample_100_wl6/sample_graphs_dims_30_epochs_2000_lr_0.3_batch_256_negsample_10_embeddings.csv", header = F)


extractAllAlgorithms <- function(dt,max_length){
  
  dt_m <- lapply(dt$ranking,function(x){
    tmp <- unlist(strsplit(as.character(x),','))
    length(tmp) <- max_length
    tmp
  })
  
  dt_m <- do.call(rbind,dt_m)
  dt_m <- as.data.frame(dt_m)
  print(dt_m)
  
}

targets_IR <- read.csv("targets/IR_shortened.csv", sep=";")
targets_IR$levels <- NULL
targets_IR$performance <- NULL
targets_IR$IR <- match(targets_IR$ranking, levels(targets_IR$ranking))

targets_RP <- read.csv("targets/RP_shortened.csv", sep=";")
targets_RP$levels <- NULL
targets_RP$performance <- NULL
targets_RP$RP <- match(targets_RP$ranking, levels(targets_RP$ranking))

targets <- merge(targets_IR,targets_RP, by.x="dataset", by.y="dataset")
dataset <- merge(dataset,targets,by.x="V1",by.y="dataset")

createGraphs <- function(dt1,dt2,type){
  
  # library(tsne)
  # dt1_tsne = as.data.frame(tsne(dt1[,2:31], perplexity=0.5))
  # dt2_tsne = as.data.frame(tsne(dt2[,2:14], perplexity=0.5))
  
  # library(Rtsne)
  # dt1_tsne <- as.data.frame(Rtsne(as.matrix(dt1[,2:31]), perplexity=0.5, pca_scale=TRUE)$Y)
  # dt2_tsne <- as.data.frame(Rtsne(as.matrix(dt2[,2:14]), perplexity=5, pca_scale=TRUE)$Y)
  
  dt1_tsne <- as.data.frame(princomp(dt1[,2:31])$scores[,1:2])
  dt2_tsne <- as.data.frame(princomp(dt2[,2:14])$scores[,1:2])
  colnames(dt1_tsne) <- c("V1","V2")
  colnames(dt2_tsne) <- c("V1","V2")
  
  dt1_tsne$type <- "cf4vec"
  dt1_tsne$dataset <- dt1$V1
  
  dt2_tsne$type <- "CM"
  dt2_tsne$dataset <- dt2$dataset
  
  if(type == "IR"){
    dt1_tsne$ranking <- dt1$ranking.x
    dt2_tsne$ranking <- dt2$ranking.x
    val = 3
  }
  else {
    dt1_tsne$ranking <- dt1$ranking.y
    dt2_tsne$ranking <- dt2$ranking.y
    val = 2
  }
  
  final <- rbind(dt1_tsne,dt2_tsne)
  
  final$dataset <- unlist(lapply(final$dataset,function(x){
    tmp <- unlist(strsplit(as.character(x),"[.]"))
    tmp <- tmp[1]
    tmp <- gsub("amazon", "AMZ", tmp)
    tmp <- gsub("jester", "JT", tmp)
    tmp <- gsub("movielens", "ML", tmp)
    tmp <- gsub("movietweetings", "MT", tmp)
    tmp <- gsub("yahoo", "YH", tmp)
    tmp <- gsub("tripadvisor", "TA", tmp)
    tmp <- gsub("bookcrossing", "BC", tmp)
    tmp <- gsub("yelp", "YE", tmp)
    tmp <- gsub("flixter", "FL", tmp)
    
    tmp <- gsub("recsys2014", "RS14", tmp)
    tmp <- gsub("digital-music", "music", tmp)
    tmp <- gsub("instant-video", "video", tmp)
    tmp <- gsub("_", "-", tmp)
    tmp
  }))
  
  g <- ggplot(final, aes(x=V1,y=V2, colour=ranking)) +
    geom_point() +
    #geom_text(aes(label=dataset)) +
    geom_text_repel(aes(label=dataset), show.legend = FALSE) +
    theme_bw() + 
    theme (axis.title.x=element_blank(),
           axis.title.y=element_blank()) +
    theme(legend.position="bottom") + 
    guides(colour=guide_legend(ncol=val,title="Metatarget")) +
    facet_grid(type ~ .)
  g
  
}


dt1 <- dataset[,-c(2:31)]


runAll <- function(type){
  if(type == "IR"){
    B <- read.csv("metafeatures_landmarkers/B_IR_final.csv", sep=";")  #landmarkers datasets have NAs
    targets <- read.csv("targets/IR.csv", sep=";")
  }
  else {
    B <- read.csv("metafeatures_landmarkers/B_RP_final.csv", sep=";")
    targets <- read.csv("targets/RP.csv", sep=";")
  }
  
  A <- read.csv("metafeatures_statistical/mf_final.csv", sep=";")
  C <- read.csv("metafeatures_graph/graph_metafeatures_final.csv", sep=",")
  D <- mergeUnifiedDataset(A,B,C)  
  rm(A,B,C)
  
  D <- merge(D,dt1,by.x="dataset",by.y="V1")
  
  createGraphs(dataset,D, type)
  
}

IR_graphic <- runAll("IR")
IR_graphic
RP_graphic <- runAll("RP")
RP_graphic
