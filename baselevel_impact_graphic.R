library(plyr)
library(reshape2)
library(ggplot2)


load("results/impact_baselevel_performance.Rda")


make_graph <- function(data, metric){
  
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#9999CC", "#66CC99")

  data<- data[which(data$variable != "AVG"),] 
  data<- data[which(data$variable != "baseline"),] 
  
  ggplot(data=data, aes(x=algorithms, y=value, group=variable, color=variable)) +
    geom_line() +
    geom_point() +
    xlab("Algorithms") + ylab("Lift (%)") +
    scale_x_continuous(breaks=seq(0, 10, 1)) +
    facet_grid(. ~ strategy, scales = "free") +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    theme(text= element_text(size = 16)) +
    theme(legend.title=element_blank()) +
    scale_color_manual(values = cbPalette) +
    theme(axis.title.x=element_blank())
  #aes(group=rev(variable))
  
}

normalizeDF <- function(dt,inverted){
  
  data_graph <- as.data.frame(apply(dt,1,function(t){
    unlist(lapply(t,function(y){
      
      if(inverted){
        res <- (- (y - t[which(names(t) == "AVG")]))*100
      }
      else{
        res <- (y - t[which(names(t) == "AVG")])*100
      }
      res
    }))
  }))
  
  row.names(data_graph) <- colnames(dt)
  
  data_graph
}


createData <- function(x,inverted, strategy){
  
  number_labels <- length(unlist(x[[1]]))
  
  data_graph <- data.frame(
    'baseline' = unlist(x[[2]]),
    cf4vec = unlist(x[[3]]),
    Metalearning = unlist(x[[1]]),
    #cf4cf_meta_cf2vec = unlist(x[[4]]),
    #cf4cf_meta_cm = unlist(x[[5]]),
    AVG = unlist(x[[2]])
  )
  rownames(data_graph) <- NULL
  
  data_graph <- as.data.frame(t(normalizeDF(data_graph,inverted)))
  data_graph$algorithms <- c(1:number_labels)
  
  
  data_graph <- melt(data_graph, id=c("algorithms"))
  data_graph$strategy <- strategy
  
  data_graph
}

IR_prev <- IR
RP_prev <- RP

# load("results/cf4cf_meta_cf2vec_base.Rda")
# IR_prev <- append(IR_prev,list(t(as.data.frame(t(IR$knn)))))
# RP_prev <- append(RP_prev,list(t(as.data.frame(t(RP$knn)))))
# 
# load("results/cf4cf_meta_mf_base.Rda")
# IR_prev <- append(IR_prev,list(t(as.data.frame(t(IR$knn)))))
# RP_prev <- append(RP_prev,list(t(as.data.frame(t(RP$knn)))))

load("results/meta_lr_comp_base.Rda")
IR_prev <- append(IR_prev,list(t(as.data.frame(t(IR$avg)))))
RP_prev <- append(RP_prev,list(t(as.data.frame(t(RP$avg)))))

df1 <- createData(IR_prev,F,"IR")
df2 <- createData(RP_prev,T,"RP")

final <- rbind(df1,df2)

levels(final$variable) <- c(levels(final$variable),"LR+cf2vec","AVG", "LR+CM") #,"CF4CF-META+cf2vec","CF4CF-META+CM")
final$variable[final$variable == 'cf4vec'] <- 'LR+cf2vec'
final$variable[final$variable == 'AR'] <- 'AVG'
final$variable[final$variable == 'Metalearning'] <- 'LR+CM'
#final$variable[final$variable == 'cf4cf_meta_cf2vec'] <- 'CF4CF-META+cf2vec'
#final$variable[final$variable == 'cf4cf_meta_cm'] <- 'CF4CF-META+CM'

make_graph(final)

