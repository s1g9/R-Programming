rankall <- function(outcome, num = "best") {
  data <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv")
  Sta<- unique(data$State)
  cause<-NA
  o
  strsplit(outcome, " ")
  outcome <- paste()
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
  ## print(mycolums)
  ans<- matrix(,nrow=0,ncol=2)
  
  for (i in 1:length(Sta)) {
  state <- Sta[i]
  subsetstate <- data[data$State==state,]
  data_subset <- subsetstate[,mycolums]
  ##print(data_subset)
  data_need <- data_subset[!data_subset[colum]=="Not Available",]
  ##print(data_need)
  id <- order(as.double(as.matrix(data_need[colum])),data_need["Hospital.Name"])
  if (num=="best") {num<-1}
  if (num=="worst") {num<-length(id)}
  ans <- rbind(ans,list(as.character(data_need[id[num],"Hospital.Name"]),as.character(state)))
  }
  b <- data.frame(ans)
  d <- order(as.character(as.matrix(b[,2])))
  names(b)<- c("hospital","state")
  b[d,]
}