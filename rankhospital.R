rankhospital<-function(state, outcome, num="best")
{
  #read in the source file
  sourceData<-read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  #create validation vectors:
  #states are read in from the file, 
  #while outcomes are defined as a list with pre-defined target columns
  states<-unique(sourceData[,"State"])
  outcomes<-list("heart attack"=11,"heart failure"=17,"pneumonia"=23)
  
  #validate states and outcomes
  if (!is.element(state, states)) {stop ("invalid state")}
  if (length(outcomes[[outcome]])==0) {stop ("invalid outcome")}
  
  #extract the state hospitals
  stateHospitals<-sourceData[sourceData$State==state,c(2,outcomes[[outcome]])]
  
  #coerce to numeric and strip out NAs
  stateHospitals[,2]<-as.numeric(stateHospitals[,2])
  stateHospitals<-na.omit(stateHospitals)
  
  #order the hospitals
  stateHospitals<-stateHospitals[order(stateHospitals[,2], stateHospitals[,1]),]
  
  #store the length of the hospitals
  numHospitals<-length(stateHospitals[[1]])
  
  stateHospitals
  
  #now validate the rank
  if (num=="best") {return(stateHospitals[1,1])}
  else if (num=="worst") {return(stateHospitals[numHospitals,1])}
  else if (num>numHospitals) {return(NA)}
  
  stateHospitals[as.numeric(num),1]
  
}