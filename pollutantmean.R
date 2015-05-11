pollutantmean <- function(directory, pollutant, id=1:332)
{
  #input validation
  
  
  #prep filenames
  prepFileNames <- function(x)
  {
    if (x<99)
    {
      x=paste(c(0,x,".csv"),collapse='')
    }
    else
    {
      x=paste(c(x,".csv"),collapse='')
    }
  }
  
  filePaths = lapply(id, prepFileNames)
  
  print(filePaths)
  #calculate mean
  
  
}