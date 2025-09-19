# Function that stratifies data by either quantile or a provided list of cuts
stratify_function <- function(data, column, n_quantiles, cuts){
  # Takes a data frame and stratifies column using quantile into n-quantiles, or into by a provided list of cuts if cuts is specified
  if (missing(cuts)) {
    if (n_quantiles == 4){ #quartiles
      cuts <- quantile(data[[column]])
      strata <- cut(data[[column]], breaks = cuts, include.lowest = TRUE, right = T, labels = c(1:n_quantiles)) 
    } 
    else if (n_quantiles == 5){ #quintiles
      cuts <- quantile(data[[column]], probs = c(0, 0.2, 0.4, 0.6, 0.8, 1))
      print(1)
      strata <- cut(data[[column]], breaks = cuts, include.lowest = TRUE, right = T, labels = c(1:n_quantiles))
      print(2)
    } 
    else {
      print("n_quantiles must be 4 or 5")
    }
  } 
  else { # pre-specified cuts
    strata <- cut(data[[column]], breaks = cuts, include.lowest = TRUE, right = T, labels = c(1:n_quantiles))
  }
  return(strata)
}