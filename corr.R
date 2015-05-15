corr<-function(directory, threshold=0)
{
  #validate inputs
  #Check directory exists
  if (!dir.exists(directory)){stop(paste(c("Error: directory ",directory," does not exist")))}
  
  #function to take only the frames that cross the threshold
  clean<-function(x)
  {
    frame<-as.data.frame(x)
    if (nrow(frame[complete.cases(frame),])>threshold)
    {
      x<-frame[complete.cases(frame),2:3]
    }
    else
    {
      x<-data.frame()
    }
  }
  
  #function to apply the correlation
  applyCor<-function(x)
  {
    frame<-as.data.frame(x)
    if(nrow(frame)>0){x<-cor(frame[1],frame[2])}
  }
  
  #Read files at one go
  fileList<-list.files(directory, full.names=TRUE)
  data<-lapply(fileList,read.csv)
  data<-lapply(data,clean)
  
  output<-unlist(lapply(data,applyCor))
  if (length(output)<1)
  {
    numeric()
  }
  else
  {
    output
  }
  
}