rankhospital <- function(state = "WA", outcome = "heart attack", rank = 1) {
  #set working directory
  setwd("C:/R/r-coursera/programming_assignment3/")

  #fix these numbers in colClasses!!! only want the 3 outcomes imported as numeric
  allData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  
  #step2 check if valid:
  if (state %in% allData$State == FALSE) {
    stop("invalid state") }
  else if (outcome %in% c("heart attack", "heart failure", "pneumonia") == FALSE) {
    stop("invalid outcome")}
  else { #(all below)
    colName <- ""
    if (outcome == "heart attack") {
      colName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" } #is column no. 11
    else if (outcome == "heart failure") {
    colName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure" } #is column no. 17
    else if (outcome == "pneumonia") {
      colName <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"  } #is colunn no. 23
 
                      ##no: sort the column from lowest to highest (lowest is best)
                      # ## no? remove NAs so they aren't counted??
  suppressWarnings(allData[, colName] <- as.numeric(allData[, colName]))
  #next problem will require remove NAs at this point? remove them now to rid error? or remove before as.numeric

  if (rank == "worst") {
  # put that specific outcome into a vector ordered ascending outcome
  stateOutcomeData <- allData[allData$State == state , c("State", "Hospital.Name", colName)]
  # order by state colName ascending then by Hospital Name
  stateOutcomeData <- stateOutcomeData[order(stateOutcomeData[,colName], rev(stateOutcomeData[,2]), decreasing = TRUE), ]

  return(stateOutcomeData[1,2])


  }

  else if (rank == "best") {
    rank <- 1
    # put that specific outcome into a vector ordered ascending outcome
    stateOutcomeData <- allData[allData$State == state , c("State", "Hospital.Name", colName)]
    # order by state colName ascending then by Hospital Name
    stateOutcomeData <- stateOutcomeData[order(stateOutcomeData[,colName], stateOutcomeData[,2]), ]

    return(stateOutcomeData[rank,2])
  }

  else {

    # put that specific outcome into a vector ordered ascending outcome
    stateOutcomeData <- allData[allData$State == state , c("State", "Hospital.Name", colName)]
    # order by state colName ascending then by Hospital Name
    stateOutcomeData <- stateOutcomeData[order(stateOutcomeData[,colName], stateOutcomeData[,2]), ]

    return(stateOutcomeData[rank,2])
  }
}      
}