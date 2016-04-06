best <- function(state, outcome) {
  hosp<-read.csv("hospital-data.csv")
  states<-unique(hosp[,"State"])
  outcomenames<-c("heart attack","heart failure", "pneumonia")

  ##Check the validity of state
    if (is.na(match(state,states)) == TRUE)
    stop("invalid state")    
  ##Check the validity of outcome
  if (is.na(match(outcome,outcomenames)) == TRUE)
    stop("invalid outcome")    
  
  ##Read the contents of the data file
  outcomedf <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ##Create a data frame withe only the necessary columns
  df<-subset(data.frame(outcomedf[,"State"],outcomedf[,"Hospital.Name"],outcomedf[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"],
                 outcomedf[,"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"],
                 outcomedf[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]),outcomedf[,"State"]==state)
  
  ##Change the name of the columns  
  names(df)<-c("state","hospital","heart attack","pneumonia","heart failure")

  ##Create data frame with only the desired outcome converted to numeric
  df2<-suppressWarnings(data.frame(df[,"hospital"],as.numeric(as.character(df[,outcome]))))
  
  ##Rename the columns of df2
  names(df2)<-c("hospital",outcome)
  
  ##Create the ordered data frame
  ordereddf<-df2[order(df2[,outcome], df2$"hospital", decreasing=F,na.last=NA),]

  ##Return the first line only
  as.character(head(ordereddf,1)[,"hospital"])

}
