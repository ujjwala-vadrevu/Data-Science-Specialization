rankall <- function(outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        
        con<-"outcome-of-care-measures.csv"
        # print(con)
       
        x <- read.csv(con)
        x<-x[,c(7,2,11,17,23)]
        hos<-character()
        st<-character(2)
        
        if(outcome=="heart attack")
        {
                x<-x[!(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack=="Not Available"),c(1,2,3)]
                x[,1]<-as.factor(x[,1])
                x[,2]<-as.character(x[,2])
                x[,3]<-as.numeric(paste(x[,3])) 
                y<-x[order(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,x$Hospital.Name),]
        }
        else if(outcome =="heart failure")
        {
                x<-x[!(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure=="Not Available"),c(1,2,4)]
                x[,1]<-as.factor(x[,1])
                x[,2]<-as.character(x[,2])
                x[,3]<-as.numeric(paste(x[,3])) 
                y<-x[order(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,x$Hospital.Name),]
        } 
        else if(outcome =="pneumonia")
        {
                x<-x[!(x$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia=="Not Available"),c(1,2,5)]
                x[,1]<-as.factor(x[,1])
                x[,2]<-as.character(x[,2])
                x[,3]<-as.numeric(paste(x[,3])) 
                y<-x[order(x$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,x$Hospital.Name),]
        } 
        else
        {
                stop("invalid outcome")
        }
        z<-split(y,y$State)
        if (num=="best") num = 1
        
        for (i in 1:length(z))
        {
               st[i]<-names(z[i])
               if (num == "worst")
               {
                       #print(length(z[[i]]$Hospital.Name))
                       hos[i]<-z[[i]]$Hospital.Name[length(z[[i]]$Hospital.Name)]
               }
               else
               {
                       hos[i]<-z[[i]]$Hospital.Name[num]
               }
        }
        data.frame(hospital=hos,state=st)
        
}