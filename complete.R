complete <- function(directory, id=1:332)
{
  #validate inputs
  #Check directory exists
  if (!dir.exists(directory)){stop(paste(c("Error: directory ",directory," does not exist")))}
    
  #Prep file paths by appending .csv and prepending the directory
  #massages by adding the trailing zero
  prepFileNames<-function(x)
  {
    if (x<10){x<-paste(c(0,x),collapse='')}
    if (x<100){x<-paste(c(0,x),collapse='')}
    x<-paste(c(directory,"/",x,".csv"),collapse='')
  }
  
  getNobs<-function(x)
  {
    x<-nrow(x[complete.cases(x),])
  }
  
  filePaths <-lapply(id, prepFileNames)
  
  #Read files at one go
  data<-lapply(filePaths,read.csv)

  #get the nobs
  nobs<-unlist(lapply(data,getNobs))
  
  output<-data.frame(id,nobs)
}