# Here's a (simulated) experiment, with a single subject and 500 categorization trials.
all.data <- read.csv('experiment-data.csv')
source('memory-limited-exemplar-model.R')
rm(sample.data.set)
rm(sample.training.data)

#wrapper function to minimize with optim
exemplar.memory.min <- function(params) {
  return (exemplar.memory.log.likelihood(all.data,params[1],params[2]))
}

# Use optim() to fit the model to this data.
# Note: In optim() you can tell it to display updates as it goes with:
# optim( ... , control=list(trace=4))
pars <- optim(c(2,.5), exemplar.memory.min, method="Nelder-Mead", control=list(trace=4))

exemplar.memory.min.fixed.decay <- function(params) {
  return (exemplar.memory.log.likelihood(all.data,params[1],1))
}
# Now try fitting a restricted version of the model, where we assume there is no decay.
# Fix the decay.rate parameter to 1, and use optim to fit the sensitivity parameter.
# Note that you will need to use method="Brent" in optim() instead of Nelder-Mead. 
# The brent method also requires an upper and lower boundary:
# optim( ..., upper=100, lower=0, method="Brent")
fixed.pars <- optim(c(2), exemplar.memory.min.fixed.decay, upper=100, lower=0, method="Brent")

# What's the log likelihood of both models? (see the $value in the result of optiom(),
# remember this is the negative log likeihood, so multiply by -1.

#The log likelihood of the variable decay model is 188, while the likelihood of the 
#fixed decay model is 249 

# What's the AIC and BIC for both models? Which model should we prefer?
variable.free.params <- 2
fixed.free.params <- 1
variable.likelihood <- pars$value
fixed.likelihood <- fixed.pars$value

AIC.variable <- 2*variable.free.params - 2*log(variable.likelihood) 
AIC.fixed <- 2*fixed.free.params - 2*log(fixed.likelihood)

BIC.variable <- variable.free.params*log(nrow(all.data)) - 2*log(variable.likelihood)
BIC.fixed <- fixed.free.params*log(nrow(all.data)) - 2*log(fixed.likelihood)

exp((AIC.variable - AIC.fixed)/2)

#We should favor the variable decay rate model; both the Akaike and Bayes IC's 
#suggest that the additional free parameter (decay rate) improves the likelihood
#of the data more than the additional free parameter detracts from the model

#### BONUS...
# If you complete this part I'll refund you a late day. You do not need to do this.

# Use parametric bootstrapping to estimate the uncertainty on the decay.rate parameter.
# Unfortunately the model takes too long to fit to generate a large bootstrapped sample in
# a reasonable time, so use a small sample size of 10-100 depending on how long you are
# willing to let your computer crunch the numbers.

# Steps for parametric bootstrapping:
# Use the best fitting parameters above to generate a new data set (in this case, that means
# a new set of values in the correct column for all.data).
# Fit the model to this new data, record the MLE for decay.rate.
# Repeat many times to get a distribution of decay.rate values.
# Usually you would then summarize with a 95% CI, but for our purposes you can just plot a
# histogram of the distribution.

