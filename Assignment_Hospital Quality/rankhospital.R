rankhospital <- function(state, outcome, num = "best"){
  data <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv")
  subsetstate <- data[data$State==state,]
  if (!nrow(subsetstate)){
    stop("invalid state")
  }
  cause<-NA
  if(outcome=="heart attack") {
    cause <- "Heart.Attack"
  }
  if(outcome=="heart failure") {
    cause <- "Heart.Failure"
  }
  if(outcome=="pneumonia") {
    cause<- "Pneumonia"
  }
  if(is.na(cause)){stop("invalid outcome")}
  ## Check that state and outcome are valid
  colum <- paste("Hospital.30.Day.Death..Mortality..Rates.from", cause, sep=".")
  mycolums<- c("Hospital.Name",colum)
  ##print(mycolums)
  data_subset <- subsetstate[,mycolums]
  ##print(data_subset)
  data_need <- data_subset[!data_subset[colum]=="Not Available",]
  ##print(data_need)
  id <- order(as.double(as.matrix(data_need[colum])),data_need["Hospital.Name"])
  if (num=="best") {num<-1}
  if (num=="worst") {num<-length(id)}
  ans <- data_need[id[num],"Hospital.Name"]
  as.vector(ans)
}