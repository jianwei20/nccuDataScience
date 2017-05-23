library('class')
library('ROCR')

args = commandArgs(trailingOnly=TRUE)
if (length(args)!=4) {
  stop("USAGE: Rscript hw5_105753040.R -fold n –out performance.csv", call.=FALSE)
}
n <- args[2]
out_f <- args[4]

d <- read.csv("./Archaeal_tfpssm.csv",header=F)

calcAUC <- function(predcol,outcol) {
  perf <- performance(prediction(predcol,outcol=='CP'),'auc')
  as.numeric(perf@y.values)
}
totalTrain <- 0
totalCal <- 0
totalTest <- 0

d<-d[sample(nrow(d)),]
flds <- cut(seq(1,nrow(d)),breaks=n,labels=FALSE)

for(i in 1:n){
  for(j in 1:n){
    if(i!=j){
      CalIndexes <- which(flds==j,arr.ind=TRUE)
      TestIndexes <- which(flds==i,arr.ind=TRUE)
      dCal <- d[CalIndexes,]
      dTest <- d[TestIndexes,]
      dTrain <- d[-CalIndexes,]
      dTrain <- dTrain[-TestIndexes,]
      
      nK <- 200
      knnTrain <- dTrain[,5600:5602]  	 
      knnCl <- dTrain[,2]=='CP' 	
      knnPred <- function(df) { 	
        knnDecision <- knn(knnTrain,df,knnCl,k=nK,prob=T)
        ifelse(knnDecision==TRUE, 	
               attributes(knnDecision)$prob,
               1-(attributes(knnDecision)$prob))
      }
      totalTrain <- totalTrain + calcAUC(knnPred(dTrain[,5600:5602]),dTrain[,2])
      totalCal <- totalCal + calcAUC(knnPred(dCal[,5600:5602]),dCal[,2])
      totalTest <- totalTest + calcAUC(knnPred(dTest[,5600:5602]),dTest[,2])
    }
  }
}

set1 <- c('trainning',round(totalTrain/(n*n),digits=2))
set2 <- c('calibration',round(totalCal/(n*n),digits=2))
set3 <- c('test',round(totalTest/(n*n),digits=2))
out_data <- data.frame(rbind(c('set','accuracy'),set1,set2,set3),stringsAsFactors = F)
write.table(out_data, file=out_f, sep=",", row.names = FALSE, col.names = FALSE)

