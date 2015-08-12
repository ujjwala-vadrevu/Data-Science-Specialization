corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
        
        counter <-0
        cor <- numeric(332)
        numObs <- numeric(332)
        for (counter in 1:332) {
                
                con<-paste0(directory,"/",formatC(counter,width = 3,flag='0'),".csv")
                x <- read.csv(con)
                y<-x[complete.cases(x),]
                cor[counter]<-cor(y$sulfate,y$nitrate)
                numObs[counter]<-nrow(y)
        }
        z<-data.frame(correl=cor, nobs=numObs)
        z<-z[z$nobs>=threshold,]
        a<-numeric()
        if (nrow(z)>0)
        {
           a<-z$correl   
        }
        a
}