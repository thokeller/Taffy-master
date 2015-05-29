rankall <- function(outcome, num = "best") {
        require(dplyr)
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
        ## sub "_" for space in disease (outcome) name
        disease <- sub("\\s+","_",outcome)
        ## create a vector of valid state names
        states <- as.character(sort(unique(mortality$state)))
        mort_df <- tbl_df(mortality)
        states.list <- lapply(states, function(x) mort_df[mort_df$state == x,c(1,2,3)])
        #hospital <- ()
        #state <- ()
        output.df <- data.frame(hospital=character(),state=character())
        for(i in 1:length(states)){
                mystate_df <- states.list[[i]]
                colnames(mystate_df) <- c("hospital","state","disease")
                if(disease == "heart_attack") {
                        filter_s_df <- filter(mystate_df,!is.na(disease))
                        sorted_s_df = arrange(filter_s_df,disease,hospital)
                        ifelse(num == "worst", num <<- length(filter_s_df$disease), num)
                        out_df <-cbind(sorted_s_df[num,1:2])
                        #output.df <- do.call(cbind,sorted_s_df[num,1:2])
                }
                output.df <- do.call(rbind,list(out_df))
        }
        #return(output.df)
}