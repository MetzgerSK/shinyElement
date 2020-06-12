## GENERIC FUNCTION TO AVERAGE ACROSS ALL COVARS
vcovAvg <- function(mats){
    # lapply: To sum all matrices in [[1]] (and then return the summed matrix),
    #         and then same thing for [[2]]...
    #
    # Map: divide each element of input list (here, ., from the pipes) by last
    #      argument (here, # sims via length)
    #
    #      NOTE: doesn't matter which list element you pick for length--just 
    #      need to get # sims from it
    
    res <- lapply(X=mats, function(x) { Reduce('+',x) }) %$%
              Map('/', ., length(mats[[1]]))  
    
    return(res)
}