library(reshape2)
library(ggplot2)
################ prepare data.frame

params2Vec <- function(val){
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

rm(df1,df2,df3,df4)


### manually selected best
best <- final[  with(final, order(-IR, -RP)), ][2,]  #manually found 
load("embeddings_grid_search_results/grid_search_detailed_graphics_sample_100_wl6.Rda")
best_IR <- IR$sample_graphs_dims_30_epochs_2000_lr_0.3_batch_256_negsample_10_embeddings.csv
best_RP <- RP$sample_graphs_dims_30_epochs_2000_lr_0.3_batch_256_negsample_10_embeddings.csv

mf_IR <- IR$MF
mf_RP <- RP$MF

avg_IR<- IR$baseline
avg_RP<- RP$baseline

load("results/cf4cf_meta_cf2vec.Rda")
cf4cf_meta_cf2vec_IR <- IR$cf4cf_meta_knn_1
cf4cf_meta_cf2vec_RP <- RP$cf4cf_meta_knn_1

load("results/cf4cf_meta_comp_knn.Rda")
cf4cf_meta_cf2vec_cm_IR <- IR$cf4cf_meta_knn_1
cf4cf_meta_cf2vec_cm_RP <- RP$cf4cf_meta_knn_1

tmp <- data.frame(
  cf4vec <- unlist(lapply(1:length(best_IR),function(x){mean(best_IR[x],best_RP[x])})),
  Metalearning <- unlist(lapply(1:length(mf_IR),function(x){mean(mf_IR[x],mf_RP[x])})),
  AverageRankings <- unlist(lapply(1:length(avg_IR),function(x){mean(avg_IR[x],avg_RP[x])})),
  cf4cf_meta_cf2vec <- unlist(lapply(1:length(best_IR),function(x){mean(cf4cf_meta_cf2vec_IR[x],cf4cf_meta_cf2vec_RP[x])})),
  cf4cf_meta_cm <- unlist(lapply(1:length(mf_IR),function(x){mean(cf4cf_meta_cf2vec_cm_IR[x],cf4cf_meta_cf2vec_cm_RP[x])}))
)


colnames(tmp) <- c("LR+cf2vec","LR+CM","AVG","CF4CF-META+cf2vec","CF4CF-META+CM")

library(scmamp)
plotCD (tmp, alpha=0.05, cex=1.25)


##################################333


createSimpleGraphic <- function(tmp){
  
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#9999CC", "#66CC99")
  
  g <- ggplot(tmp, aes(x=algorithm,y=performance,group=algorithm,fill=algorithm)) +
    scale_fill_manual(values=cbPalette) +
    geom_bar(stat = "identity", width = 0.7, position = "dodge") +
    guides(fill = guide_legend(title = "Meta-algorithms"))+
    facet_grid(. ~ strategy) +
    ylab("Kendall's tau") +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    theme(text= element_text(size = 16)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  g
  
}

tmp_ir <- data.frame(
  AVG = avg_IR,
  CF4CF_META_CM = cf4cf_meta_cf2vec_cm_IR,
  CF4CF_META_CF2VEC = cf4cf_meta_cf2vec_IR,
  LR_CM = mf_IR,
  LR_CF2VEC = best_IR
)

colnames(tmp_ir) <- c("AVG","CF4CF-META+CM","CF4CF-META+cf2vec","LR+CM","LR+cf2vec")

tmp_rp <- data.frame(
  AVG = avg_RP,
  CF4CF_META_CM = cf4cf_meta_cf2vec_cm_RP,
  CF4CF_META_CF2VEC = cf4cf_meta_cf2vec_RP,
  LR_CM = mf_RP,
  LR_CF2VEC = best_RP
)

colnames(tmp_rp) <- c("AVG","CF4CF-META+CM","CF4CF-META+cf2vec","LR+CM","LR+cf2vec")

#kendall's tau
new_tmp_ir <- apply(tmp_ir,2,mean)
new_tmp_ir <- melt(new_tmp_ir)
new_tmp_ir$metatarget <- "IR"
new_tmp_ir$algorithm <- rownames(new_tmp_ir)

new_tmp_rp <- apply(tmp_rp,2,mean)
new_tmp_rp <- melt(new_tmp_rp)
new_tmp_rp$metatarget <- "RP"
new_tmp_rp$algorithm <- rownames(new_tmp_rp)

new_tmp <- rbind(new_tmp_ir,new_tmp_rp)
colnames(new_tmp) <- c("performance","strategy","algorithm")
createSimpleGraphic(new_tmp)
