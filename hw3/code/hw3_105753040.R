#sensitivity evaluation
sensitivity_func<-function(query_m,d)
{
  if(query_m == "male"){
    TP <- 0
    FN <- 0
    len <- length(d$persons)
    for(x in c(1:len)){
      if(d[x,"reference"]=="male" && d[x,"prediction"]=="male"){
        TP <- TP + d[x,"pred.score"]
      }
      else if(d[x,"reference"]=="male" && d[x,"prediction"]=="female"){
        FN <- FN + d[x,"pred.score"]
      }
    }
    return(round(TP/(TP+FN),digits = 2))
  }
  else if (query_m == "female") {
    TP <- 0
    FN <- 0
    len <- length(d$persons)
    for(x in c(1:len)){
      if(d[x,"reference"]=="Female" && d[x,"prediction"]=="Female"){
        TP <- TP + (1-d[x,"pred.score"])
      }
      else if(d[x,"reference"]=="Memale" && d[x,"prediction"]=="Male"){
        FN <- FN + (1-d[x,"pred.score"])
      }
    }
    return(round(TP/(TP+FN),digits = 2))
  } 
  else {
    stop(paste("ERROR: unknown query function", query_m))
  }
}

#specificity evaluation
specificity_func<-function(query_m,d)
{
  if(query_m == "male"){
    FP <- 0
    TN <- 0
    len <- length(d$persons)
    for(x in c(1:len)){
      if(d[x,"reference"]=="Female" && d[x,"prediction"]=="Male"){
        FP <- FP + d[x,"pred.score"]
      }
      else if(d[x,"reference"]=="Female" && d[x,"prediction"]=="Female"){
        TN <- TN + d[x,"pred.score"]
      }
    }
    
    return(round(TN/(TN+FP),digits = 2))
  }
  else if (query_m == "female") {
    TN <- 0
    FP <- 0
    len <- length(d$persons)
    for(x in c(1:len)){
      if(d[x,"reference"]=="Male" && d[x,"prediction"]=="Female"){
        FP <- FP + (1-d[x,"pred.score"])
      }
      else if(d[x,"reference"]=="Male" && d[x,"prediction"]=="Male"){
        TN <- TN + (1-d[x,"pred.score"])
      }
    }
    return(round(TN/(TN+FP),digits = 2))
  } 
  else {
    stop(paste("ERROR: unknown query function", query_m))
  }
}

library('ROCR')
# AUC evaluation
AUC_func <- function(query_m, d){
  eval <- prediction(d$pred.score,d$reference)
  #plot(performance(eval,"tpr","fpr"))
  return(round(attributes(performance(eval,'auc'))$y.values[[1]],digits = 2))
}

#F1_func evaluation
F1_func <- function(query_m, d, sensitivity){
  if(query_m == "male"){
    TP <- 0
    FP <- 0
    len <- length(d$persons)
    for(x in c(1:len)){
      if(d[x,"reference"]=="Male" && d[x,"prediction"]=="Male"){
        TP <- TP + d[x,"pred.score"]
      }
      else if(d[x,"reference"]=="Female" && d[x,"prediction"]=="Male"){
        FP <- FP + d[x,"pred.score"]
      }
    }
    precision <- TP/(TP+FP)
    return(round(2*precision*sensitivity/(precision+sensitivity),digits = 2))
  }
  else if (query_m == "female") {
    TP <- 0
    FP <- 0
    len <- length(d$persons)
    for(x in c(1:len)){
      if(d[x,"reference"]=="Female" && d[x,"prediction"]=="Female"){
        TP <- TP + (1-d[x,"pred.score"])
      }
      else if(d[x,"reference"]=="Male" && d[x,"prediction"]=="Female"){
        FP <- FP + (1-d[x,"pred.score"])
      }
    }
    precision <- TP/(TP+FP)
    return(round(2*precision*sensitivity/(precision+sensitivity),digits = 2))    
  } 
  else {
    stop(paste("ERROR: unknown query function", query_m))
  }
}
# read parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw1.R -query min|max -files file1 file2 ... filen –out out.csv", call.=FALSE)
}

# parse parameters
i<-1 
while(i < length(args))
{
  if(args[i] == "-query"){
    query_m<-args[i+1]
    i<-i+1
  }else if(args[i] == "-files"){
    j<-grep("-", c(args[(i+1):length(args)], "-"))[1]
    files<-args[(i+1):(i+j-1)]
    i<-i+j-1
  }else if(args[i] == "-out"){
    out_f<-args[i+1]
    i<-i+1
  }else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}

print("PROCESS")
print(paste("query mode :", query_m))
print(paste("output file:", out_f))
print(paste("files      :", files))

methods<-c()
sensitivitys<-c()
specificitys<-c()
F1s<-c()
AUCs<-c()

#read files
for(file in files)
{
  method<-gsub(".csv", "", basename(file))
  d<-read.table(file, header=T,sep=",")
  sensitivity <- sensitivity_func(query_m,d)
  specificity <- specificity_func(query_m,d)
  F1 <- F1_func(query_m, d, sensitivity)
  AUC <- AUC_func(query_m,d)
  
  methods <- c(methods, method)
  sensitivitys<-c(sensitivitys, sensitivity)
  specificitys<-c(specificitys, specificity)
  F1s<-c(F1s, F1)
  AUCs<-c(AUCs, AUC)
}
  second_sen <- sort(sensitivitys,decreasing = TRUE)[2]
  second_spec <- sort(specificitys,decreasing = TRUE)[2]
  second_F1 <- sort(F1s,decreasing = TRUE)[2]
  second_AUC <- sort(AUCs,decreasing = TRUE)[2]
  
  secondSenMethod <- methods[which(sensitivitys==second_sen)]
  secondSpecMethod <- methods[which(specificitys==second_spec)]
  secondF1Method <- methods[which(F1s==second_F1)]
  secondAUCMethod <- methods[which(AUCs==second_AUC)]
  
  maxSenMethod <- methods[which.max(sensitivitys)]
  maxSpecMethod <- methods[which.max(specificitys)]
  maxF1Method <- methods[which.max(F1s)]
  maxAUCMethod <- methods[which.max(AUCs)]
  
  filesecond <- c(secondSenMethod[2],
                  secondSpecMethod,
                  secondF1Method,
                  secondAUCMethod)
  
  filemax <- c(maxSenMethod[1],
               maxSpecMethod,
               maxF1Method,
               maxAUCMethod)
  
  whichMethod <- c()
  for(x in c(1:4)){
    filem <- paste("hw3/data/set1/",filemax[x],".csv",sep="")
    files <- paste("hw3/data/set1/",filesecond[x],".csv",sep="")
    dm <- read.table(filem, header=T,sep=",")
    ds <- read.table(files, header=T,sep=",")
    
    t <- rbind(data.frame(group=filemax[x],probability=round(dm$pred.score,digits=0)),  
               data.frame(group=filesecond[x],probability=round(ds$pred.score,digits=0)))
    
    tab <- table(t)
    fisher.test(tab)
    (aConversionRate <- tab[filemax[x],'0']/sum(tab[filemax[x],]))
    (bConversionRate <- tab[filesecond[x],'0']/sum(tab[filesecond[x],]))
    (commonRate <- sum(tab[,'1'])/sum(tab))

    # Frequentist significance test
    significance <- pbinom( 	
      lower.tail=F, 	
      q=tab[filesecond[x],'0']-1, 	
      size=sum(tab[filesecond[x],]), 	
      prob=commonRate 	
    )
    if(significance<0.05){
      whichMethod<-c(whichMethod,paste(filemax[x],"*",sep=""))
    }
    else{
      whichMethod<-c(whichMethod,paste(filemax[x],sep=""))
      
    }
  }
  methods <- c(methods,"highest")
  sensitivitys<-c(sensitivitys, whichMethod[1])
  specificitys<-c(specificitys, whichMethod[2])
  F1s<-c(F1s, whichMethod[3])
  AUCs<-c(AUCs, whichMethod[4])

#output file
out_data<-data.frame(methods, sensitivitys, specificitys, F1s, AUCs, stringsAsFactors = F)
write.table(out_data, file=out_f, sep=",", row.names = FALSE)
  
