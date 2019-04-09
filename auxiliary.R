library(BBmisc)

valuesToString <- function(x){
  paste0(paste0(x[1], " +- "), x[2])
}

removeCFS <- function(df,threshold){
  library(caret)
  
  corr_m <- cor(df)
  corr_m[is.na(corr_m)] <- 0
  
  
  toRemove <- findCorrelation(corr_m, cutoff = threshold)
  print(length(toRemove))
  df1 <- as.data.frame(df[,-(which(colnames(df) %in% colnames(df)[toRemove]))])
  
  print("metafeatures removed due to CFS")
  print(setdiff(colnames(df),colnames(df1)))
  
  df1
  
}


replaceNA <- function(df){
  df <- apply(df,2,function(x){
    x[which(is.na(x))] <- mean(x, na.rm = T)
    x
  })
}

normalizeMatrix <- function(df){
  df1 <- as.data.frame(BBmisc::normalize(df))
  colnames(df1) <- colnames(df)
  
  tmp <- data.frame(dataset=rownames(df))
  df1 <- cbind(tmp,df1)
  df1
}

mergeUnifiedDataset <- function(A,B,C){
  
  library(data.table) ## 1.9.3
  library(splitstackshape)
  library(plyr)
  library(BBmisc)
  
  D1 <- merge(A,B,by.x="dataset",by.y="dataset")
  df <- merge(D1,C,by.x="dataset",by.y="dataset")
  
  rownames(df) <- df$dataset
  df$dataset <- NULL
  df <- removeCFS(df,0.9)
  df <- normalizeMatrix(df)
  
  df
}

renameCols <- function(dt){
  count <- length(colnames(dt))
  new_names <- 1:(count -1)
  new_names <- as.character(new_names)
  new_names <- unlist(lapply(new_names, function(x){paste0("E",x)}))
  tmp <- c("dataset",new_names)
  tmp
}
