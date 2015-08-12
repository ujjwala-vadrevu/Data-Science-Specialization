best <- function(state, outcome) {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        con<-"outcome-of-care-measures.csv"
        # print(con)
        x <- read.csv(con)
        x<-x[,c(7,2,11,17,23)]
        
        if(state %in% unique(x$State))
        {
                y<-x[x$State==state,]
                names(y)<-names(x)
                
                if(outcome=="heart attack")
                {
                        y<-y[!(y$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack=="Not Available"),c(2,3)]
                        y[,1]<-as.character(y[,1])
                        y[,2]<-as.numeric(paste(y[,2]))
                        z<-y[order(y$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,y$Hospital.Name),]
                        #print(z)
                }
                else if(outcome =="heart failure")
                {
                       y<-y[!(y$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure=="Not Available"),c(2,4)]
                       y[,1]<-as.character(y[,1])
                       y[,2]<-as.numeric(paste(y[,2]))
                       z<-y[order(y$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,y$Hospital.Name),]
                       #print(z)
                } 
                else if(outcome =="pneumonia")
                {
                        y<-y[!(y$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia=="Not Available"),c(2,5)]
                        y[,1]<-as.character(y[,1])
                        y[,2]<-as.numeric(paste(y[,2]))
                        z<-y[order(y$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,y$Hospital.Name),]
                        #print(z)
                } 
                else
                {
                        stop("invalid outcome")
                }
        } 
        else
        {
                stop("invalid state")
        }
        z[1,1]
}