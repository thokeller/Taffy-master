rankhospital <- function(state, outcome, num = "best") {
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
        if(is.na(num)) stop("invalid num")
        ## return hospital in 'state' with that rank for 30-day death rate (mortality)
        ### a num (rank) that is greater than the largest number for rank should return NA
        mystate.df <- mortality[mortality$state == state,]
        stateorder <- order(mystate.df[,which(names(mystate.df) == disease)],
                            mystate.df[,1])
        ## recode for "best"  and "worst" 
        if(is.numeric(num)) rownumber <- stateorder[num]
        if(!is.numeric(num)) {
                num <- ifelse(num == "best", 1, num)
                num <- ifelse(num == "worst" && disease == "heart_attack", 
                              which(mystate.df$heart_attack == max(mystate.df$heart_attack, na.rm=T)), num)
                num <- ifelse(num == "worst" && disease == "heart_failure",
                              which(mystate.df$heart_failure == max(mystate.df$heart_failure, na.rm=T)), num)
                num <- ifelse(num == "worst" && disease == "pneumonia", 
                              which(mystate.df$pneumonia == max(mystate.df$pneumonia, na.rm=T)), num)
                rownumber <- num
        }
        ## use num to retrieve ranked hospital
        rankedhospital <- as.character(mystate.df[rownumber,1])
        return(rankedhospital)
}