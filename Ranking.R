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


best <- function(state2, outcome,num="best") {
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
      if  (!(num %in% c("best","worst")) & is.Numeric(num))
                    {
                      numYes<- TRUE
                    }
      if(!state2 %in% d1[, "state"]){
                        stop('invalid state')
                  }
      else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
                  stop('invalid outcome')
                  } 

       else {
             ## Return hospital name in that state with lowest 30-day death
            ## rate
            d2 <-  subset(d1,d1$state %in% state2,select=c("hospital",outcome))
            #CONVERT AS NUMERIC
            d2 <- d2[complete.cases(d2), ]
            
            d2[,2]<-suppressWarnings(as.numeric(d2[,2]))
            if (numYes) {
                            if (num>nrow(d2)) { return(NA)}
                             else {
                                  hospitals  <- d2[order(d2)]
                                  hospital <- hospitals[num,]
                              }
                    }
            else if (num=="best")
                    {
                    min_val <- min(d2[,2])
                    hospitals<- d2[,"hospital"][which(d2[,2]==min_val)]
                    hospital  <- hospitals[order(hospitals)]
                    
                    }
            else if (num=="worst")
                    {
                    max_val <- max(d2[,2])
                    hospitals<- d2[,"hospital"][which(d2[,2]==max_val)]
                    hospital  <- hospitals[order(hospitals)]
                    
                     }
       }
           
     return(hospital)
}