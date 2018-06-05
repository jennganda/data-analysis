setwd("C://Users/JennGanda/Documents/R/R-training/assignment")
getwd()
#outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
#head(outcome)
#ncol(outcome)
#nrow(outcome)
#names(outcome)
#str(outcome)
#summary(outcome)

#outcome[, 11] <- as.numeric(outcome[, 11])
#hist(outcome[, 11])


rankhospital <- function(state2, outcome,num="best") {
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
   

      
      if(!state2 %in% d1[, "state"]){
                        stop('invalid state')
                  }
      else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
                  stop('invalid outcome')
                  } 

       else {
             keep<- c("hospital","state",outcome)
             d1 <- d1[keep]
             ## Return hospital name in that state with lowest 30-day death
            ## rate
            d2 <-  subset(d1,d1$state %in% state2,select=c("hospital",outcome))
            #CONVERT AS NUMERIC
         
            d2[,2]<-suppressWarnings(as.numeric(d2[,2]))
            bad<-is.na(d2[,outcome])
            d3 <- d2[!bad,]
            index  <- with(d3,order(d3[outcome],d3["hospital"]))
            ordered_desired_data <- d3[index, ]
            
            
            if  (num %in% c("best") ) {
                  num <- 1
            }
            else if  (num %in% c("worst")) {
                  num <- length(ordered_desired_data[, outcome])
                  
            }
            else if (is.numeric(num)) {
                            if (num>nrow(d2)) { 
                                          return(NA)
                            }
            }      
      
            hospital <- ordered_desired_data[num, 1]
       }    # valid parameters
           
     return(hospital)
}