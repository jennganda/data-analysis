setwd("C://Users/JennGanda/Documents/R/R-training/assignment")
getwd()
#outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
#head(outcome)
#ncol(outcome)
#nrow(outcome)
#names(outcome)
#str(outcome)
#summary(outcome)
> r <- rankall("heart attack", 4)
> as.character(subset(r, state == "HI")$hospital)
[1] "CASTLE MEDICAL CENTER"
#outcome[, 11] <- as.numeric(outcome[, 11])
#hist(outcome[, 11])
rnk<- function(x, num = "best") {
      if  (num=="best") {
            idx <- 1
      }
      else if(num=="worst"){idx<-nrow(x)}  
      else if (is.numeric(num)) {
            if (num>nrow(x)) { 
                  return(NA)
            }
            else 
            { idx<-num}
      }   
  
      return(c(x[idx,1],x[idx,2]))
      
}

rankall <- function(outcome,num="best") {
      ## Read outcome data
      doutcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      ##get only the data needed
      keeps <-c("Hospital.Name","State","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
      d1 <- doutcome[keeps]
      #change the long name 
      names(d1) <- c("hospital","state","heart attack","heart failure","pneumonia")
  
      ## Check that state and outcome are valid
   

    if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
                  stop('invalid outcome')
                  } 

       else {
             keep<- c("hospital","state",outcome)
             d2 <- d1[keep]
            ## Return hospital name in that state with lowest 30-day death
            ## rate
            #CONVERT AS NUMERIC
            d2[,outcome]<-suppressWarnings(as.numeric(d2[,outcome]))
            bad<-is.na(d2[,outcome])
            d3 <- d2[!bad,]
            # split the data by state
           
            
            index  <- with(d3,order(d3["state"],d3[outcome],d3["hospital"]))
            ordered_desired_data <- d3[index, ]
            s <-  split(ordered_desired_data , ordered_desired_data$state)
            # get the list using lapply
           list1 <-  lapply(s, rnk,num)
            #rbind all the list as matrix
           x <- do.call(rbind, list1)
           #create the dataframe
           hospitals<-data.frame(x)
           cnames <- c("hospital","state")
           colnames(hospitals) <-cnames
           
           
            
       }    # valid parameters
           
     return(hospitals)
}