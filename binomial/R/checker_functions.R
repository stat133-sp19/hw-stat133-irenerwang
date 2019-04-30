#' name: check_prob
#' description: checks if the given prob is valid
#' param: prob a probability
#' return: boolean: prob is between 0 and 1
check_prob <- function(prob){
  is_valid <- FALSE
  if(prob < 1 & prob > 0){
    is_valid <- TRUE
  }
  if(is_valid == FALSE){
    stop('p has to be a number between 0 and 1')
  }
  return(is_valid)
}

#' name: check_trials
#' description: checks if the given number of trials is valid
#' param: trials: number of trials
#' return: boolean: number of trials is positive
check_trials <- function(trials){
  is_valid <- FALSE
  if(trials > 0){
    is_valid <- TRUE
  }
  if(is_valid == FALSE){
    stop('trials has to be a number greater than 1')
  }
  return(is_valid)
}


#' name: check_success
#' description: checks if the given number of successes is valid
#' param: success: the number of successes
#' param: trials: the number of trials to run
#' return: boolean: number of trials is non-negative
check_success <- function(success, trials){
  is_valid <- FALSE
  if((success >= 0) & (success <= trials)){
    is_valid <- TRUE
  }
  if(is_valid == FALSE){
    if(success < 0) {
      stop('invalid success value')
    } else {
      stop('success cannot be greater than trials')
    }
  }
  return(is_valid)
}

