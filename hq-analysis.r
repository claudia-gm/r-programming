library(plyr)

# Download and unzip the data in the working directory
filename <- "ProgAssignment3-data.zip"
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-data.zip"
  download.file(fileURL, filename, method="curl")
}  
if (!file.exists("Hospital Quality")) { 
  unzip(filename) 
}

# PART 1 - Plot the 30-day mortality rates for heart attack
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
# You may get a warning about NAs being introduced; that is okay
# The following line makes a simple histogram of the 30-day death rates 
# from heart attack (column 11 in the outcome dataset)
hist(outcome[, 11])


# PART 2 - Finding the best hospital in a state
best <- function(state, outcome) {
  # Read outcome data
  data <- read.csv("outcome-of-care-measures.csv")
  # Make sure that the state and the outcome are valid
  states <- levels(data[, 7])[data[, 7]]
  state_flag <- FALSE
  for (i in 1:length(states)) {
    if (state == states[i]) {
      state_flag <- TRUE
    }
  }
  if (!state_flag) {
    stop ("invalid state")
  } 
  if (!((outcome == "heart attack") | (outcome == "heart failure")
        | (outcome == "pneumonia"))) {
    stop ("invalid outcome")
  }
  
  ## Return hospital name with the lowest 30-day death rate
  col <- if (outcome == "heart attack") {
    11
  } else if (outcome == "heart failure") {
    17
  } else {
    23
  }
  
  data[, col] <- suppressWarnings(as.numeric(levels(data[, col])[data[, col]]))
  data[, 2] <- as.character(data[, 2])
  statedata <- data[grep(state, data$State), ]
  orderdata <- statedata[order(statedata[, col], statedata[, 2], na.last = NA), ]
  orderdata[1, 2]
}

# Test best(), the expected answer is [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
best("TX", "heart attack")


# PART 3 - Ranking hospitals by outcome in a state
rankhospital <- function(state, outcome, num = "best") {
  # Read outcome data
  data <- read.csv("outcome-of-care-measures.csv")
  # Make sure that the state and the outcome are valid
  states <- levels(data[, 7])[data[, 7]]
  state_flag <- FALSE
  for (i in 1:length(states)) {
    if (state == states[i]) {
      state_flag <- TRUE
    }
  }
  if (!state_flag) {
    stop ("invalid state")
  } 
  if (!((outcome == "heart attack") | (outcome == "heart failure")
        | (outcome == "pneumonia"))) {
    stop ("invalid outcome")
  }
  
  # Return hospital name in that state with the given rank 30-day death rate
  col <- if (outcome == "heart attack") {
    11
  } else if (outcome == "heart failure") {
    17
  } else {
    23
  }
  
  data[, col] <- suppressWarnings(as.numeric(levels(data[, col])[data[, col]]))
  data[, 2] <- as.character(data[, 2])
  statedata <- data[grep(state, data$State), ]
  orderdata <- statedata[order(statedata[, col], statedata[, 2], na.last = NA), ]
  if(num == "best") {
    orderdata[1, 2]
  } else if(num == "worst") {
    orderdata[nrow(orderdata), 2]
  } else{
    orderdata[num, 2]
  }
}

#Test rankhospital(). The expected answer is [1] "DETAR HOSPITAL NAVARRO"
rankhospital("TX", "heart failure", 4)

