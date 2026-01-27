# Define a Standard Error function to calculate SE alongside risk difference
se_rd_function <- function(data, strata, treatment, outcome = "outcome"){
    # takes a data frame with name of the strata column and the treatment column
    # calculates SE for risk difference using SE = sqrt(p1(1-p1)/n1 + p0(1-p0)/n0)
    
    if (strata == "overall") {
      # Calculate proportions
      p0 <- mean(data[[outcome]][data[[treatment]] == 0])
      p1 <- mean(data[[outcome]][data[[treatment]] == 1])
      
      # Calculate sample sizes
      n0 <- sum(data[[treatment]] == 0)
      n1 <- sum(data[[treatment]] == 1)
      
      # Calculate SE
      se <- sqrt((p1 * (1 - p1) / n1) + (p0 * (1 - p0) / n0))
      
    } else {
      # if strata is not overall, calculate the SE for each of the strata 
      # count the number of unique values in the strata column
      n_strata <- length(unique(data[[strata]]))
      
      # initialize a vector to store the standard errors
      se <- vector("numeric", n_strata)
      
      # loop through each strata and calculate the SE
      for (i in 1:n_strata) {
        # Calculate proportions for this stratum
        p0 <- mean(data[[outcome]][data[[strata]] == i & data[[treatment]] == 0])
        p1 <- mean(data[[outcome]][data[[strata]] == i & data[[treatment]] == 1])
        
        # Calculate sample sizes for this stratum
        n0 <- sum(data[[strata]] == i & data[[treatment]] == 0)
        n1 <- sum(data[[strata]] == i & data[[treatment]] == 1)
        
        # Calculate SE for this stratum
        se[i] <- sqrt((p1 * (1 - p1) / n1) + (p0 * (1 - p0) / n0))
      }
    }
    
    return(se)
  }
