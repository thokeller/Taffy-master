orderindex <- function(x) {
        mort_df <- tbl_df(mortality)
        # for apply: x is index to states, y is outcome (disease)
        outcome <- "heart_attack"
        states_comp.df <- sapply(1:length(states), function(x,y) {
                mystate.df <- filter(mort_df,state == states[x])
                ok <- complete.cases(mystate.df[,c(1,2,mystate.df$y)])
                mystate.df[unlist(ok),]
        }, y = outcome
        )
        
}