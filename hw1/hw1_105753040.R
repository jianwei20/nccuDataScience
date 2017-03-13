args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw1_105753040.R input", call.=FALSE)
}else if (length(args)==1) {
  i_f <- args[1]
  #read csv data
  data <- read.table(paste("./",i_f,sep=''),header=TRUE,sep=",")
  weight <- round(max(data[2]),2)
  height <- round(max(data[3]),2)
  #output csv data
  set <- gsub(".csv","",i_f)
  df <- data.frame(set,weight,height,row.names = NULL)
  write.table(df,file="result.csv",sep=",",row.names = FALSE)
}else if (length(args)==4){
  inFile <- args[2]
  outFile <- args[4]
  #the second argument is -out
  if(args[1]=="-out"){
    inFile <- args[4]
    outFile <- args[2]
  }
  #read csv data
  data <- read.table(paste("./",inFile,sep=''),header=TRUE,sep=",")
  weight <- round(max(data[2]),2)
  height <- round(max(data[3]),2)
  #output csv data
  set <- gsub(".csv","",inFile)
  df <- data.frame(set,weight,height,row.names = NULL)
  write.table(df,file=outFile,sep=",",row.names = FALSE)
}

