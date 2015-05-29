rankall <- function(outcome, num = "best") {
        ## read outcome data in downloaded csv file
        outcome.df <- read.csv("Assignment3-data/outcome-of-care-measures.csv", 
                               colClasses="character",
                               na.strings = "Not Available")
        ## subset columns of interest: hospital, state and three disease outcomes
        mortality <- outcome.df[, c(2, 7, 11,17,23)]      
        ## fix numeric values (now columns: 3:5)
        m.df <- sapply(3:5, function(x) as.numeric(mortality[,x]))
        mortality <- cbind(mortality[,1:2],m.df)
        
        colnames(mortality) <- c("hospital", "state", "heart_attack",
                                 "heart_failure","pneumonia")
        ## check the 'outcome' arguments is valid; stop() and
        ### return "invalid outcome" for bad input,
        ### "missing argment" if missing argument)
        if(missing(outcome)) stop("missing argument")
        outcome = tolower(outcome)
        diseases <- c("heart attack","heart failure","pneumonia")
        if(is.na(diseases[match(outcome,diseases)])) stop("invalid outcome")
        ## better way?
        if (outcome == diseases[1]) dis.index <<- 1
        if (outcome == diseases[2]) dis.index <<- 2
        if (outcome == diseases[3]) dis.index <<- 3
        ## create a vector of valid disease column names
        disease.colnames <- as.vector(sapply(diseases,function(x) sub("\\s+","_",x)))
        ## sub "_" for space in disease (outcome) name
        disease <- sub("\\s+","_",outcome)
        ## create a vector of valid state names
        states <- as.character(unique(mortality$state))
        ## create a list of data.frames for each outcome (disease)
        ### e.g.: d.df <- mortality[, c(1,2,match(disease,colnames(mortality)))]
        ## create a list of disease.data.frames, one per disease
        heartattack.df <- mortality[,c(1,2,3)]
        heartfail.df <- mortality[,c(1,2,4)]
        pneum.df <- mortality[,c(1,2,5)]
        dis.list <- list(pneum.df,heartattack.df,heartfail.df)
        ## use argument 'outcome' as index to dis.list
        myoutcome.df <- dis.list[[dis.index]]
        mystate.df <- myoutcome.df[myoutcome.df$state == state,]
        ## order by outcome value, then hospital name
        stateorder <- order(as.numeric(mystate.df[,3]),mystate.df[,1])
        ## recode for "best"  and "worst" 
        if(!is.numeric(num)) {
                if(num == "best") num <- 1
                if(num == "worst") num <- which(mystate.df[,3] == max(mystate.df[,3], na.rm=T))
        }
        besthospital <- as.character(mystate.df[stateorder[num],1])
        
        
        ### "best" is rank 1, "worst" is the largest number for rank for the state
        
        ## for each state, find the hospital of the given rank
        ### NA if no hospital for a state has that rank
        
        ## return a data.frame with 2 columns: the hospital names and state (Abreciation)
        ### exclude states with NA for the rank
        
        
}