# doesitwork.R

#setwd("C:/Users/u341138/Dropbox/Documents/Teaching/2017-2018/Bayesian statistics/R/DBDA2Eprograms")

# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!

# Required packages for this exercise.
require(rjags)
require(coda)
require(BEST)
require(stats)
source("DBDA2E-utilities.R")


plotGaussian <- function(data, mean, sd) {
  plot.new()
  par(mfrow=c(1,1))
  h = hist(data, breaks = 50)
  xrange = seq(min(data),max(data),length=50) 
  gaussian_distribution = dnorm(xrange, mean=mean, sd = sd)
  scaled_gaussian = gaussian_distribution * diff(h $ mids[1:2])*length(data) 
  lines(xrange, scaled_gaussian)
}

# This code provides an outline; additional code needs to be added by yourself!



#----------   Model 1: learning a mean   --------------

# Set the initial parameters

sigma = 2.0
N = 1000
mu = 10

# Generate observations here
x = rep(0, N)

# fill vector x here

for(i in 1:N){
  mean <- rnorm(1,mu,sigma)
  new_x <- rnorm(1,mean,sigma)
  x[i] <- new_x
}

hist(x,breaks = 50)


#define starting mean
mu_hat <- 1
sigma_hat <- 1
# Write down the same model in JAGS notation.
model1.string = "
  model {
    # Prior
  
      mu ~ dnorm(mu_hat, 1/sigma_hat)
  

    # Likelihood
      for(i in 1:N){
        x[i] ~ dnorm(mu, 1/sigma_hat)
      }
  }
"

# JAGS usually reads models from a text file; here we use a string as a fake file.   
model1.spec = textConnection(model1.string)

# Construct the object containing both the model specification as well as the data and some sampling parameters.
jagsmodel1 <- jags.model(model1.spec,
                         data = list('N' = N,                    # the number of data points                       
                                     'mu_hat' = mu_hat,          # prior value
                                     'sigma_hat' = sigma_hat,            # the standard deviation of the likelihood
                                     'x' = x                     # the observations
                                     ),  
                         n.chains=4)

mcmciterations = 10000 
samples = coda.samples(jagsmodel1,
                       c('mu'), # which variables do you want to monitor?
                       n.iter=mcmciterations)

# plot/interpret the results
mcmcsummary = summary(samples)
mcmcsummary $ statistics

plot1 <- plotPost(samples, xlab = "mu", ylab = "P(mu)")


#----------   Model 2: learning mean and variance   --------------

# Set the initial parameters
sigma = 2.0
N = 1000
mu = 10
a = 1
b = 1

sigma_hat = 1
mu_hat = 1

# Generate observations here
x = rep(0, N)

for(i in 1:N){
  sd <- rgamma(a,b)
  mean <- rnorm(1,mu,sigma)
  new_x <- rnorm(1,mean,sd)
  x[i] <- new_x
}

hist(x, breaks = 50)


# Write down the same model in JAGS notation.
model2.string = "
  model {
    # Prior
    mu ~ dnorm(mu_hat, 1/sigma_hat)
    sigma ~ dgamma(a,b)

    # Likelihood
    for(i in 1:N){
        x[i] ~ dnorm(mu, 1/sigma)
}
    
  }
"

# JAGS usually reads models from a text file; here we use a string as a fake file.   
model2.spec = textConnection(model2.string)

# Construct the object containing both the model specification as well as the data and some sampling parameters.
jagsmodel2 <- jags.model(model2.spec,
                         data = list('N' = N,                    # the number of data points
                                     'x' = x,                   # the observations
                                      'sigma_hat' = sigma_hat,
                                     'mu_hat' = mu_hat,
                                     'a' = a,
                                     'b' = b
                                     ),  
                         n.chains=4)

mcmciterations = 10000 
samples2 = coda.samples(jagsmodel2,
                       c('mu','sigma'), # which parameters do you want to monitor?
                       n.iter=mcmciterations)



# plot/interpret the results
mcmcsummary2 = summary(samples2)
statistics <-mcmcsummary2 $ statistics
stat_mu <- statistics[1][1]
stat_sigma <- statistics[2][1]
plot2_mu <- plotPost(samples2[,1], xlab = "mu", ylab = "P(mu)")
plot2_sigma <- plotPost(samples2[,2], xlab = "sigma", ylab = "P(sigma)")

reconstructed_gaussian <- plotGaussian(x,stat_mu,stat_sigma)
original_gaussian      <- plotGaussian(x,mu,sigma)

#----------   Model 3: the wrong model   --------------


# Set the initial parameters
N = 1000
df = 2

# Generate observations here
x = rep(0, N)
for(i in 1:N){
  x[i] <- rt(1,2)
}

hist(x, breaks = 50)


# Write down the same model in JAGS notation.
model3.string = "
  model {
# Prior
mu ~ dnorm(mu_hat, 1/sigma_hat)
sigma ~ dgamma(a,b)

# Likelihood
for(i in 1:N){
x[i] ~ dnorm(mu, 1/sigma)
}

}
"

# JAGS usually reads models from a text file; here we use a string as a fake file.   
model3.spec = textConnection(model3.string)

# Construct the object containing both the model specification as well as the data and some sampling parameters.
jagsmodel3 <- jags.model(model3.spec,
                         data = list('N' = N,                    # the number of data points
                                     'x' = x,                     # the observations
                                     'sigma_hat' = sigma_hat,
                                     'mu_hat' = mu_hat,
                                     'a' = a,
                                     'b' = b
                                     ),  
                         n.chains=4)

mcmciterations = 10000 
samples3 = coda.samples(jagsmodel3,
                       c('mu','sigma'), # which variables do you want to monitor?
                       n.iter=mcmciterations)


# plot/interpret the results
mcmcsummary3 = summary(samples3)
statistics <-mcmcsummary3 $ statistics
stat_mu <- statistics[1][1]
stat_sigma <- statistics[2][1]
plot3_mu <- plotPost(samples3[,1], xlab = "mu", ylab = "P(mu)")
plot3_sigma <- plotPost(samples3[,2], xlab = "sigma", ylab = "P(sigma)")

reconstructed_gaussian <- plotGaussian(x,stat_mu,stat_sigma)
# The Gaussian distribution fit to the data generated by the t-student distribution becomes more platykurtic compared to the one fitted to data distributed by a Gaussian distribution. This is because JAGS is trying to make the Gaussian fit the heavy tails of the data generated by the t-student distribution that appear as "outliers" from the perspective of a normal distribution.




