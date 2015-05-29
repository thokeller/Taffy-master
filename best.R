best <- function(state, outcome) {
        ## read outcome data in downloaded csv file
        outcome.df <- read.csv("Assignment3-data/outcome-of-care-measures.csv", 
                               colClasses="character",
                               na.strings = "Not Available")
        ## subset columns of interest: hospital, state and three disease outcomes
        mortality <- outcome.df[, c(2, 7, 11,17,23)]
        colnames(mortality) <- c("hospital", "state", "heart_attack",
                                 "heart_failure","pneumonia")
        ## check that 'state' and 'outcome' arguments are valid; stop() and
        ### return "invalid state", or "invalid outcome" for bad input,
        ### "missing argment" if missing
        if(missing(state)) stop("missing argument")
        if(missing(outcome)) stop("missing argument")
        ## allow variations in case
        state = toupper(state)
        outcome = tolower(outcome)
        ## create vector of valid state names
        states <- as.character(unique(mortality$state))
        if(is.na(states[match(state,states)])) stop("invalid state")
        ## and vector of valid outcome (disease) values
        diseases <- c("heart attack","heart failure","pneumonia")
        if(is.na(diseases[match(outcome,diseases)])) stop("invalid outcome")
        ## sub "_" for space in disease (outcome) name
        disease <- sub("\\s+","_",outcome) 
        ## return hospital in 'state' with lowest 30-day death rate (mortality)
        ### example best("TX", "heart attack") returns [1] "CYPRESS FAIRBANKS..."
        ### hospital names should be sorted alphabetically, return the first for ties
        mystate.df <- mortality[mortality$state == state,]
        stateorder <- order(as.numeric(mystate.df[,which(names(mystate.df) == disease)]),
                            mystate.df[,1])
        besthospital <- as.character(mystate.df[stateorder[1],1])
        return(besthospital)
}