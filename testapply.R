output.df <- data.frame(hospital=character(),state=character())
states <- c("AK","AL","CA","TX","WI")
disease <- "heart_attack"
num <- 20
output.df <- sapply(1:length(states),  function(x,y,z) FUN = {
        print(x)
        out <- paste(x, y, z, sep=", ")
}, y = disease, z = num)
output.df
