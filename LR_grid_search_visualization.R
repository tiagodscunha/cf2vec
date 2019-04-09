library(reshape2)
library(ggplot2)

################ 

#script used to create visualizations for all hyperparameter tuning results
#one must change the aesthetics parameter "x=wl" to the desired value to analyze

##################3

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


##create baselines
tmp <- createDF(IR,RP,5,FALSE)
mtl <- tmp[which(rownames(tmp) == "MF"),]
avg <- tmp[which(rownames(tmp) == "baseline"),]

tmp <- final
rownames(tmp) <- NULL
tmp <- unique(tmp[,3:8])

mtl_df <-tmp
mtl_df$IR <- mtl$IR
mtl_df$RP <- mtl$RP
mtl_df <- melt(mtl_df)

avg_df <-tmp
avg_df$IR <- avg$IR
avg_df$RP <- avg$RP
avg_df <- melt(avg_df)

final <- melt(final)

p <- ggplot(final, aes(x=wl,y=value)) + 
  geom_boxplot() +
  scale_y_continuous(name = "Kendall's Tau") +
  stat_boxplot(geom ='errorbar') +
  theme_bw() +
  facet_grid(. ~ variable) + 
  geom_point(data=mtl_df, aes(color="LR+CM")) + 
  geom_point(data=avg_df, aes(color="AVG")) +
  guides(colour=guide_legend(title="Competitors")) + 
  theme ( axis.title.x=element_blank())
p

