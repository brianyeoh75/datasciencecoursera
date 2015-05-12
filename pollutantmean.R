pollutantmean <- function(directory, pollutant, id=1:332)
{
  #input validation
  #Check directory exists, check pollutant exists
  if (!dir.exists(directory)){stop(paste(c("Error: directory ",directory," does not exist")))}
  if (!pollutant %in% c("sulfate","nitrate")){stop(paste(c("Error: pollutant ",pollutant," is invalid")))}
  
  #prep file paths by appending .csv and prepending the directory
  #massages by adding the trailing zero
  prepFileNames <- function(x)
  {
    if (x<10){x<-paste(c(0,x),collapse='')}
    if (x<100){x<-paste(c(0,x),collapse='')}
    x<-paste(c(directory,"/",x,".csv"),collapse='')
  }
  filePaths <-lapply(id, prepFileNames)
  
  #Read files at one go
  data<-lapply(filePaths,read.csv)
  
  #collapses the list of dataframes stored in data into one dataframe
  data<-Reduce(function(...) merge(..., all=TRUE), data)
  
  #calculate mean
  mean(unlist(data[pollutant]),na.rm=TRUE)
}
  