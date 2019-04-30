#' name: aux_mean
#' description: computes the mean
#' param: trials the number of trials
#' param: prob a probability
#' return: double: the mean
aux_mean<- function(trials, prob){
  return(trials * prob)
}

#' name: aux_variance
#' description: computes the variance
#' param: trials the number of trials
#' param: prob a probability
#' return: double: the variance
aux_variance<- function(trials, prob){
  return(trials * prob * (1-prob))
}

#' name: aux_mode
#' description: computes the mode
#' param: trials the number of trials
#' param: prob a probability
#' return: int: the mean
aux_mode<- function(trials, prob){
  return(round(trials * prob + prob))
}

#' name: aux_skewness
#' description: computes the skewness
#' param: trials the number of trials
#' param: prob a probability
#' return: double: the skewness
aux_skewness<- function(trials, prob){
  return((1-2*prob)/((trials * prob* (1-prob))^0.5))
}

#' name: aux_kurtosis
#' description: computes the kurtosis
#' param: trials the number of trials
#' param: prob a probability
#' return: double: the kurtosis
aux_kurtosis<- function(trials, prob){
  return((1-6*prob*(1-prob))/((trials * prob* (1-prob))))
}

