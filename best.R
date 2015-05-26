best <- function(state, outcome)
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

  #coerce to numeric 
  stateHospitals[,2]<-as.numeric(stateHospitals[,2])
  stateHospitals<-na.omit(stateHospitals)
  stateHospitals<-stateHospitals[order(stateHospitals[,2], stateHospitals[,1]),]
  
  stateHospitals[1,1]
}