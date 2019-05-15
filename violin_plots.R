library(reshape2)

params2Vec <- function(val){
  val <- gsub("sample_graphs_100","",val)
  val <- gsub("sample_graphs_50","",val)
  val <- gsub("sample_graphs_25","",val)
  val <- gsub("sample_graphs","",val)
  tmp <- unlist(strsplit(toString(val),"[_]"))
  res <- c(as.numeric(tmp[3]),as.numeric(tmp[5]),as.numeric(tmp[7]),as.numeric(tmp[9]),as.numeric(tmp[11]))
  res
}

createDF <- function(IR,RP,wl,removeOthers = TRUE){
  
  IR$baseline <- NULL
  RP$baseline <- NULL
  IR$MF <- NULL
  RP$MF <- NULL
  
  ir_melt <- melt(IR)
  ir_melt$target <- "IR"
  
  rp_melt <- melt(RP)
  rp_melt$target <- "RP"
  
  final <- rbind(ir_melt,rp_melt)
  final$wl <- wl
  final
}




getAllMetamodels <- function(framework,metafeature,type,metalearner){
  
  switch(framework,
         "meta_lr"={
           load(paste0("/home/tiago-inesc/cf4cf/results/",framework,"_",metafeature,".Rda"))
           
           if(type == "IR"){
             tmp <- melt(data.frame(
               knn =unlist(IR$knn),
               rt =unlist(IR$rt),
               rfr =unlist(IR$rfr)
             ))
           }
           else {
             tmp <- melt(data.frame(
               knn =unlist(RP$knn),
               rt =unlist(RP$rt),
               rfr =unlist(RP$rfr)
             ))
           }
           return(tmp)
         },
         "meta_cf4cf"={
           load("results/meta_cf4cf.Rda")
           
           if(type=="IR"){
             tmp <- melt(data.frame(
               als1 =unlist(IR$cf4cf_als_1),
               als2 =unlist(IR$cf4cf_als_2),
               als3 =unlist(IR$cf4cf_als_3),
               als4 =unlist(IR$cf4cf_als_4),
               ubcf1 =unlist(IR$cf4cf_ubcf_1),
               ubcf2 =unlist(IR$cf4cf_ubcf_2),
               ubcf3 =unlist(IR$cf4cf_ubcf_3),
               ubcf4 =unlist(IR$cf4cf_ubcf_4)
             ))
           }
           else {
             tmp <- melt(data.frame(
               als1 =unlist(RP$cf4cf_als_1),
               als2 =unlist(RP$cf4cf_als_2),
               als3 =unlist(RP$cf4cf_als_3),
               als4 =unlist(RP$cf4cf_als_4),
               als5 =unlist(RP$cf4cf_als_4),
               als6 =unlist(RP$cf4cf_als_4),
               als7 =unlist(RP$cf4cf_als_4),
               als8 =unlist(RP$cf4cf_als_4),
               ubcf1 =unlist(RP$cf4cf_ubcf_1),
               ubcf2 =unlist(RP$cf4cf_ubcf_2),
               ubcf3 =unlist(RP$cf4cf_ubcf_3),
               ubcf4 =unlist(RP$cf4cf_ubcf_4),
               ubcf5 =unlist(RP$cf4cf_ubcf_5),
               ubcf6 =unlist(RP$cf4cf_ubcf_6),
               ubcf7 =unlist(RP$cf4cf_ubcf_7),
               ubcf8 =unlist(RP$cf4cf_ubcf_8)
             ))
           }
           return(tmp)
           
         },
         "cf4cf_meta"={
           load(paste0("/home/tiago-inesc/cf4cf/results/",framework,"_",metafeature,"_",metalearner,".Rda"))
           
           if(type == "IR"){
             tmp <- melt(data.frame(
               knn =unlist(IR$knn),
               rt =unlist(IR$rt),
               rfr =unlist(IR$rfr)
             ))
           }
           else {
             tmp <- melt(data.frame(
               knn =unlist(RP$knn),
               rt =unlist(RP$rt),
               rfr =unlist(RP$rfr)
             ))
           }
           return(tmp)
         },
         "alors"={
           load(paste0("results/",framework,"_",metafeature,".Rda"))
           
           if(type == "IR"){
             tmp <- melt(data.frame(
               alors =unlist(IR$alors)
             ))
           }
           else {
             tmp <- melt(data.frame(
               alors =unlist(RP$alors)
             ))
           }
           return(tmp)
         },
         "meta_aslib"={
           load(paste0("results/",framework,"_",metafeature,".Rda"))
           
           if(type == "IR"){
             tmp <- melt(data.frame(
               lm =unlist(IR$lm),
               xgboost =unlist(IR$xgboost),
               svm =unlist(IR$svm),
               rrf =unlist(IR$rrf),
               rpart =unlist(IR$rpart),
               rknn =unlist(IR$rknn)
             ))
           }
           else {
             tmp <- melt(data.frame(
               lm =unlist(RP$lm),
               xgboost =unlist(RP$xgboost),
               svm =unlist(RP$svm),
               rrf =unlist(RP$rrf),
               rpart =unlist(RP$rpart),
               rknn =unlist(RP$rknn)
             ))
           }
           return(tmp)
         },
         "lr_cf2vec"={
           
           
           # load("embeddings_grid_search_results/grid_search_detailed_graphics_sample_100_wl3.Rda")
           # df1 <- createDF(IR,RP,3)
           # 
           # load("embeddings_grid_search_results/grid_search_detailed_graphics_sample_100_wl4.Rda")
           # df2 <- createDF(IR,RP,4)
           # 
           # load("embeddings_grid_search_results/grid_search_detailed_graphics_sample_100_wl5.Rda")
           # df3 <- createDF(IR,RP,5)
           
           load("embeddings_grid_search_results/grid_search_detailed_graphics_sample_100_wl6.Rda")
           df4 <- createDF(IR,RP,6)
           # 
           # load("embeddings_grid_search_results/grid_search_detailed_graphics_sample_100_wl7.Rda")
           # df5 <- createDF(IR,RP,7)
           # 
           # load("embeddings_grid_search_results/grid_search_detailed_graphics_sample_100_wl8.Rda")
           # df6 <- createDF(IR,RP,8)
           
           
           final <- df4 #rbind(df1,df2,df3,df4,df5,df6)
           final$wl <- NULL
           final$target <- NULL
          
           return(final)
         },
         "cf4cf_meta_cf2vec" = {
           
           load("results/cf4cf_meta_cf2vec.Rda")
           
           if(type == "IR"){
             tmp <- melt(data.frame(
               knn =unlist(IR$cf4cf_meta_knn_1),
               rt =unlist(IR$cf4cf_meta_rt_1),
               rfr =unlist(IR$cf4cf_meta_rfr_1)
             ))
           }
           else {
             tmp <- melt(data.frame(
               knn =unlist(RP$cf4cf_meta_knn_1),
               rt =unlist(RP$cf4cf_meta_rt_1),
               rfr =unlist(RP$cf4cf_meta_rfr_1)
             ))
           }
           return(tmp)
           
           
         }
  )
}

LR_cm_IR <- getAllMetamodels("meta_lr","comp","IR")
LR_cm_RP <- getAllMetamodels("meta_lr","comp","RP")

LR_cf2vec_IR <- getAllMetamodels("lr_cf2vec",NULL,"IR")
LR_cf2vec_RP <- getAllMetamodels("lr_cf2vec",NULL,"RP")

CF4CF_META_CM_IR <- getAllMetamodels("meta_lr","comp","IR","knn")
CF4CF_META_CM_RP <- getAllMetamodels("meta_lr","comp","RP","knn")

CF4CF_META_cf2vec_IR <- getAllMetamodels("cf4cf_meta_cf2vec",NULL,"IR")
CF4CF_META_cf2vec_RP <- getAllMetamodels("cf4cf_meta_cf2vec",NULL,"RP")


LR_cm_IR$strategy <- "LR"
LR_cm_RP$strategy <- "LR"
CF4CF_META_CM_IR$strategy <- "CF4CF_META"
CF4CF_META_CM_RP$strategy <- "CF4CF_META"
LR_cf2vec_IR$strategy <- "LR"
LR_cf2vec_RP$strategy <- "LR"
CF4CF_META_cf2vec_IR$strategy <- "CF4CF_META"
CF4CF_META_cf2vec_RP$strategy <- "CF4CF_META"

LR_cm_IR$data <- "CM"
LR_cm_RP$data <- "CM"
CF4CF_META_CM_IR$data <- "CM"
CF4CF_META_CM_RP$data <- "CM"
LR_cf2vec_IR$data <- "cf2vec"
LR_cf2vec_RP$data <- "cf2vec"
CF4CF_META_cf2vec_IR$data <- "cf2vec"
CF4CF_META_cf2vec_RP$data <- "cf2vec"

tmp_ir <- rbind(LR_cm_IR,CF4CF_META_CM_IR,LR_cf2vec_IR,CF4CF_META_cf2vec_IR)
tmp_rp <- rbind(LR_cm_RP,CF4CF_META_CM_RP,LR_cf2vec_RP,CF4CF_META_cf2vec_RP)

tmp_ir$target <- "IR"
tmp_rp$target <- "RP"

all <- rbind(tmp_ir,tmp_rp)



all$dataset <- sort(read.csv("metafeatures_statistical/mf_final.csv",sep=";")$dataset,decreasing = F)

#corrigir datasets: onde estão?

all$dataset <- unlist(lapply(all$dataset,function(x){
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




library(ggplot2)

p <- ggplot(all, aes(x=dataset,y=value)) + 
  geom_violin() +
  coord_flip() + 
  facet_grid(data ~ strategy, scales="free") + 
  theme(text = element_text(size=8)) + 
  theme (axis.title.x=element_blank(),axis.title.y=element_blank()) 

p

