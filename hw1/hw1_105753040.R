########################
# homework1 example
########################

args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw1_105753040.R input", call.=FALSE)
} else if (length(args)==1) {
  i_f <- args[1]
}
data <- read.table(paste("./",args[2],sep=''),header=TRUE,sep=",")
weight <- round(max(data[2]),2)
height <- round(max(data[3]),2)
set <- gsub(".csv","",args[2])
df <- data.frame(set,weight,height,row.names = NULL)
write.table(df,file="result.csv",sep=",",row.names = FALSE)