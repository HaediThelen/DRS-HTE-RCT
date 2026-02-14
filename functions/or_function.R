# Define an Odds Ratio function 
or_function <- function(data, strata, treatment, outcome = "outcome"){
  # takes a data frame with name of the strata column and the treatment column
  # if strata = "overall", overall odds ratio is calculated
  if (strata == "overall") {
    # Calculate risks for each treatment group
    risk_0 <- mean(data[[outcome]][data[[treatment]] == 0])
    risk_1 <- mean(data[[outcome]][data[[treatment]] == 1])
    # Calculate odds for each group
    odds_0 <- risk_0 / (1 - risk_0)
    odds_1 <- risk_1 / (1 - risk_1)
    # Calculate OR (treatment=1 vs treatment=0)
    or <- odds_1 / odds_0
  } else {
    # if strata is not overall, calculate the odds ratio for each of the strata 
    # count the number of unique values in the strata column
    n_strata <- length(unique(data[[strata]]))
    # initialize a vector to store the odds ratios
    or <- vector("numeric", n_strata)
    # loop through each strata and calculate the OR
    for (i in 1:n_strata) {
      # Calculate risks for each treatment group within stratum i
      risk_0 <- mean(data[[outcome]][data[[strata]] == i & data[[treatment]] == 0])
      risk_1 <- mean(data[[outcome]][data[[strata]] == i & data[[treatment]] == 1])
      # Calculate odds for each group
      odds_0 <- risk_0 / (1 - risk_0)
      odds_1 <- risk_1 / (1 - risk_1)
      # Calculate OR
      or[i] <- odds_1 / odds_0
    }
  }
  return(or)
}