rankall <- function(outcome,num="best")
  {

  ##Create vector with possible outcomes for them to be consulted
  outcomenames<-c("heart attack","heart failure", "pneumonia")
  
  ##Check the validity of outcome
  if (is.na(match(outcome,outcomenames)) == TRUE)
    stop("invalid outcome")    

  ##Read the contents of the data file
  outcomedf <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ##Create a data frame withe only the necessary columns
  df<-subset(data.frame(outcomedf[,"State"],outcomedf[,"Hospital.Name"],outcomedf[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"],
                        outcomedf[,"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"],
                        outcomedf[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]))
  
  ##Change the name of the columns  
  names(df)<-c("state","hospital","heart attack","pneumonia","heart failure")
  
  ##Create data frame with only the desired outcome converted to numeric
  df2<-suppressWarnings(data.frame(df[,"state"],df[,"hospital"],as.numeric(as.character(df[,outcome]))))
  
  ##Rename the columns of df2
  names(df2)<-c("state","hospital",outcome)
  
  ##Create the ordered data frame eliminating NAs
  ordereddf<-df2[order(df2[,outcome], df2$"hospital", decreasing=F,na.last=NA),]

  ##Create the ranked data frame
  final<-transform(ordereddf,rankasc=ave(as.double(ordereddf[,outcome]),as.character(ordereddf[,"state"]),FUN=function(x) rank(x,ties.method="first")),rankdesc=ave(as.double(ordereddf[,outcome]),as.character(ordereddf[,"state"]),FUN=function(x) rank(-x,ties.method="first")))
  
  ##Assign numbers to num in case they are character type
  if (num=="worst")
    {
      num<-1
      dfcom<-subset(final,rankdesc==num,c("hospital","state"))
    }
  else if (num=="best") 
    {
      num<-1
      dfcom<-subset(final,rankasc==num,c("hospital","state"))
    }
  else
  {
    dfcom<-subset(final,rankasc==num,c("hospital","state"))
  }
  ##Create the data frame of states
  states<-data.frame(state=unique(df[,"state"]))
  
  final2<-subset(merge(states,dfcom,"state","state",all.x=TRUE),1==1,c("hospital","state"))
  row.names(final2) <- final2$state

  final2
    
  }