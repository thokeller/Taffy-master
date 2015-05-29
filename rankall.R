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
        # capture column for outcome argument
        disease.cols <- as.vector(sapply(colnames(mort_df), function(x) sub("\\s+","_",x)))
        col_disease <- match(disease,disease.cols)
        states.list <- lapply(states, function(x) mort_df[mort_df$state == x,c(1,2,col_disease)])
        ## fix num for "best" 
        ifelse(num == "best", num <- 1, num)
        ## create list of states with outcome rank = num
        output.list <- lapply( 1:length(states), function(x,y,z) FUN={
                mystate_df <- states.list[[x]]
                colnames(mystate_df) <- c("hospital","state","disease")
                filter_s_df <- filter(mystate_df,!is.na(disease))
                sorted_s_df = arrange(filter_s_df,disease,hospital)
                ifelse(num == "worst", num <- length(sorted_s_df$disease), num)
                sorted_s_df[num,1:2]   
        }, y=disease, z=num)
        
        output.df <- rbind_all(output.list)

}




