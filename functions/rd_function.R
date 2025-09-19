# Define a Risk Difference function 
rd_function <- function(data, strata, treatment, outcome = "outcome"){
  # takes a data frame with name of the strata column and the treatment column, if no strata = overall risk difference is calculated
  if (strata == "overall") {
    rd <- (mean(data[[outcome]][data[[treatment]] == 0]) - mean(data[[outcome]][data[[treatment]] == 1]))
  } else {
    # if strata is not overall, calculate the risk difference each of the strata 
    # count the number of unique values in the strata column
    n_strata <- length(unique(data[[strata]]))
    # initialize a vector to store the risk differences
    rd <- vector("numeric", n_strata)
    # loop through each strata and calculate the RD
    for (i in 1:n_strata) {
      rd[i] <- (mean(data[[outcome]][data[[strata]] == i & data[[treatment]] == 0]) -
                  mean(data[[outcome]][data[[strata]] == i & data[[treatment]] == 1]))
    }
  }
  return(rd)
}