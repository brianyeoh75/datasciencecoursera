rankall<-function(outcome, num="best")
{
  #read in the source file
  sourceData<-read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  #create validation vectors:
  #states are read in from the file, 
  #while outcomes are defined as a list with pre-defined target columns
  states<-unique(sourceData[,"State"])
  states<-sort(states)
  outcomes<-list("heart attack"=11,"heart failure"=17,"pneumonia"=23)
  
  #validate outcomes
  if (length(outcomes[[outcome]])==0) {stop ("invalid outcome")}
  
    
  #extract the state hospitals
  #for (state in states)
  #{
  #  hosps[1,hosps$state]<-stateRank(state, sourceData, outcomes[[outcome]], num)
  #}
  
  ranks<-lapply(states, function(x) {stateRank(x, sourceData, outcomes[[outcome]],num)})
  
  hosps<-data.frame(hospital=unlist(ranks),state=states)
  
  
}

stateRank<-function(state, sourceData, outcome, num)
{
  #extract the state hospitals
  stateHospitals<-sourceData[sourceData$State==state,c(2,outcome)]
  
  #coerce to numeric and strip out NAs
  stateHospitals[,2]<-as.numeric(stateHospitals[,2])
  stateHospitals<-na.omit(stateHospitals)
  
  #order the hospitals
  stateHospitals<-stateHospitals[order(stateHospitals[,2], stateHospitals[,1]),]
  
  #store the length of the hospitals
  numHospitals<-length(stateHospitals[[1]])
  
  #now validate the rank
  if (num=="best") {return(stateHospitals[1,1])}
  else if (num=="worst") {return(stateHospitals[numHospitals,1])}
  else if (num>numHospitals) {return(NA)}
  
  stateHospitals[as.numeric(num),1]
}