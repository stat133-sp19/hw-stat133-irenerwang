#' @title: bin_choose
#' @description: computes the number of combination in which k successes can occur in n trials
#' @param: n the number of trials
#' @param: k the number of successes
#' @return: double: the total number of combinations
#' @export
#' @examples: bin_choose(n = 5,k = 2)
#' @examples: bin_choose(5,0)
#' @examples: bin_choose(5,1:3)
bin_choose <- function(n, k){
  for(i in k){
    if(n < i){
      stop('k cannot be greater than n')
    }
  }
  numerator <- factorial(n)
  denominator <- factorial(k)*factorial(n-k)
  return(numerator/denominator)
}

#' @title: bin_probability
#' @description: computes the probability in which k successes can occur in n trials
#' @param: success the number of successes
#' @param: trials the number of trials
#' @param: prob the probability of success
#' @return: double: the probability in which k successes can occur in n trials
#' @export
#' @examples: bin_probability(success = 2,trials = 5, prob = 0.5)
#' @examples: bin_probability(success = 0:2,trials = 5, prob = 0.5)
#' @examples: bin_probability(success = 55,trials = 100, prob = 0.45)
bin_probability <- function(success, trials, prob){
  valid_trials <- check_trials(trials)
  valid_prob <- check_prob(prob)
  valid_success <- TRUE
  for(i in success){
    this_success <- check_success(i, trials)
    if(this_success == FALSE) {
      valid_success <- FALSE
    }
  }
  if((valid_trials == TRUE) & (valid_prob == TRUE) & (valid_success == TRUE)){
    return(bin_choose(trials, success)*((prob)^success)*((1-prob)^(trials - success)))
  } else if (valid_trials == FALSE) {
    stop('invalid trials value')
  } else if (valid_prob == FALSE) {
    stop('invalid probability value')
  } else {
    stop('invalid success value')
  }
}

#' @title: bin_distribution
#' @description: computes the probability distribution of binomial(trials, prob)
#' @param: trials the number of trials
#' @param: prob the probability of success
#' @return: dataframe with two classes, bindis and dataframe
#' @export
#' @examples: bin_distribution(trials = 5, prob = 0.5)
bin_distribution <- function(trials, prob){
  successes <- 0:trials
  probabilities <- c()
  for(i in successes) {
    this_prob <- bin_probability(i, trials, prob)
    probabilities <- c(probabilities, this_prob)
  }
  distribution <- data.frame('success' = successes, 'probability' = probabilities)
  class(distribution) <- c('bindis', 'data.frame')
  return(distribution)
}

#' @export
plot.bindis <- function(dist){
  return(barplot(dist$probability, xlab = 'successes', ylab = 'probability'))
}

#' @title: bin_cumulative
#' @description: computes the cumulative distribution of binomial(trials, prob)
#' @param: trials the number of trials
#' @param: prob the probability of success
#' @return: dataframe with two classes, bincum and dataframe
#' @export
#' @examples: bin_cumulative(trials = 5, prob = 0.5)
bin_cumulative <- function(trials, prob){
  successes <- 0:trials
  probabilities <- c()
  cumulation <-c()
  for(i in successes) {
    this_prob <- bin_probability(i, trials, prob)
    cumulation <- c(cumulation, sum(probabilities, this_prob))
    probabilities <- c(probabilities, this_prob)
  }

  cumulative <- data.frame('success' = successes, 'probability' = probabilities, 'cumulative' = cumulation)
  class(cumulative) <- c('bincum', 'data.frame')
  return(cumulative)
}


#' @export
plot.bincum <- function(dist){
  return(plot(dist$cumulative, type = 'b', xlab = 'successes', ylab = 'probability'))
}

#' @title: bin_variable
#' @description: makes a binomial random variable
#' @param: trials the number of trials
#' @param: prob the probability of success
#' @return: binvar object
#' @export
#' @examples: bin_variable(trials = 10, prob = 0.3)
bin_variable <- function(trials, prob) {
  valid_trials <- check_trials(trials)
  valid_prob <- check_prob(prob)
  variable <- c(trials, prob)
  class(variable) <- 'binvar'
  return(variable)
}

#' @export
print.binvar <- function(variable){
  cat('Binomial Variable"\n\n')
  cat('Parameters"\n\n')
  cat(sprintf('number of sides: %s', variable[1]), "\n")
  cat(sprintf('prob of success: %s', variable[2]), "\n")
}

#' @export
summary.binvar <- function(variable){
  trials <- variable[1]
  prob <- variable[2]
  mean <- aux_mean(trials, prob)
  variance <- aux_variance(trials, prob)
  mode <- aux_mean(trials, prob)
  skewness <- aux_skewness(trials, prob)
  kurtosis <- aux_kurtosis(trials, prob)
  summary <- c(trials, prob, mean, variance, mode, skewness, kurtosis)
  names(summary) <- c('trials', 'prob', 'mean', 'variance', 'mode', 'skewness', 'kurtosis')
  class(summary) <- 'summary.binvar'
  return(summary)
}

#########################fix this one!
#' @export
print.summary.binvar <- function(variable){
  summary_var <- summary(variable)
  cat('Summary Binomial"\n\n')
  cat('Parameters"\n\n')
  cat(sprintf('number of sides: %s', summary_var[1]), "\n")
  cat(sprintf('prob of success: %s', summary_var[2]), "\n\n")
  cat('Measures"\n\n')
  cat(sprintf('Mean: %s', summary_var[3]), "\n")
  cat(sprintf('Variance: %s', summary_var[4]), "\n")
  cat(sprintf('Mode: %s', summary_var[5]), "\n")
  cat(sprintf('Skewness: %s', summary_var[6]), "\n")
  cat(sprintf('Kurtosis: %s', summary_var[7]), "\n")

}

#' @title: bin_mean
#' @description: calculates the mean of the binomial with trials number of trials and probability of succeess prob
#' @param: trials the number of trials
#' @param: prob the probability of success
#' @return: the mean of the binomial(trials, prob)
#' @export
#' @examples: bin_mean(trials = 10, prob = 0.3)
bin_mean <- function(trials, prob) {
  valid_trials <- check_trials(trials)
  valid_prob <- check_prob(prob)
  return(aux_mean(trials, prob))
}

#' @title: bin_variance
#' @description: calculates the variance of the binomial with trials number of trials and probability of succeess prob
#' @param: trials the number of trials
#' @param: prob the probability of success
#' @return: the variance of the binomial(trials, prob)
#' @export
#' @examples: bin_variance(trials = 10, prob = 0.3)
bin_variance <- function(trials, prob) {
  valid_trials <- check_trials(trials)
  valid_prob <- check_prob(prob)
  return(aux_variance(trials, prob))
}

#' @title: bin_mode
#' @description: calculates the mode of the binomial with trials number of trials and probability of succeess prob
#' @param: trials the number of trials
#' @param: prob the probability of success
#' @return: the mode of the binomial(trials, prob)
#' @export
#' @examples: bin_mode(trials = 10, prob = 0.3)
bin_mode <- function(trials, prob) {
  valid_trials <- check_trials(trials)
  valid_prob <- check_prob(prob)
  return(aux_mode(trials, prob))
}

#' @title: bin_skewness
#' @description: calculates the skewness of the binomial with trials number of trials and probability of succeess prob
#' @param: trials the number of trials
#' @param: prob the probability of success
#' @return: the skewness of the binomial(trials, prob)
#' @export
#' @examples: bin_skewness(trials = 10, prob = 0.3)
bin_skewness <- function(trials, prob) {
  valid_trials <- check_trials(trials)
  valid_prob <- check_prob(prob)
  return(aux_skewness(trials, prob))
}

#' @title: bin_kurtosis
#' @description: calculates the kurtosis of the binomial with trials number of trials and probability of succeess prob
#' @param: trials the number of trials
#' @param: prob the probability of success
#' @return: the kurtosis of the binomial(trials, prob)
#' @export
#' @examples: bin_kurtosis(trials = 10, prob = 0.3)
bin_kurtosis <- function(trials, prob) {
  valid_trials <- check_trials(trials)
  valid_prob <- check_prob(prob)
  return(aux_kurtosis(trials, prob))
}

