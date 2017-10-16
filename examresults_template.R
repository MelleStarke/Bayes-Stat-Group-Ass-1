
# STUDENT NAMES
#
#
# Edwin Wenink s4156072
#


# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!

# Required packages for this exercise (you may add more if you want to).
require(rjags)
require(coda)
source("DBDA2E-utilities.R")

#------------------------   Model 1: exam scores   ----------------------------

n = 40
m = 15
a = 40
b = 10
pStudy = rbeta(1,a,b)
cat(pStudy, " this is our actual probability for pStudy")
pRandom = 0.5
#result vector
k <- rep(0,m)
#repeat for 15 students
for(i in 1:m){
  #choose group: 0 = guessing, 1 = studied
  group <- sample(x=c(0,1),size=1,prob=c(0.5,0.5))
  
  #switch on group
  if(group==0){            #guessing
    pCorrect <- pRandom
  }
  else{                    #studying
    pCorrect <- pStudy
  }
  #generate amount of correct answers
  correctAnswers <- rbinom(n=1,size=n,prob=pCorrect)
  k[i] <- correctAnswers
}

hist(k,10)

# THE DATA
n = 40
#k = c(19, 20, 16, 23, 22, 30, 38, 29, 34, 35, 35, 32, 37, 36, 33)
p = length(k)
pi <- c(0.5,0.5)

# THE MODEL
exammodel1.string = "
  model {

  #prior
  pStudy ~ dbeta(a_hat,b_hat)

  for(i in 1:m){
  group[i] ~ dbern(0.5)
  theta[i] <- ifelse(group[i]==0,pGuess,pStudy)
  k[i] ~ dbin(theta[i],n)  

  }
  }
"

# JAGS usually reads models from a text file; here we use a string as a fake file.   
exammodel1.spec = textConnection(exammodel1.string)

# SAMPLING PARAMETERS
niter = 10000
nchains = 4
a_hat = 1
b_hat = 1

# Construct the object containing both the model specification as well as the data and some sampling parameters.
jagsmodel1 <- jags.model(exammodel1.spec,
                   data = list('k' = k,
                               'a_hat' = a_hat,
                               'b_hat' = b_hat,
                               'm' = p,
                               'n' = n,
                               'pGuess' = pRandom
                               ),
                   n.chains = nchains)

# Collect samples to approximate the posterior distribution.
model1samples = coda.samples(jagsmodel1,
                           c('group'), # which variables do you want to model
                           n.iter = niter)


# Add your analyses based on the collected samples here:
mcmcsummary_model1 = summary(model1samples)
mcmcsummary_model1 $ statistics
plotPost(model1samples)
diagMCMC(codaObject = model1samples, parName = 'pStudy')

#----------   Model 2: exam scores with individual differences   --------------

# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!

# Required packages for this exercise (you may add more if you want to).
require(rjags)
require(coda)
source("DBDA2E-utilities.R")

# THE DATA
n = 40
k = c(19, 20, 16, 23, 22, 30, 38, 29, 34, 35, 35, 32, 37, 36, 33)
p = length(k)
pGuess = 0.5



# With respect to the last exercise, we now sample a mean from a beta distribution
# Consequently, given this mean and precision kappa, we sample individual succes rates for the group that studied

# THE MODEL
exammodel2.string = "
  model {
  for(i in 1:p){
    group[i] ~ dbern(0.5)
}
  meanPStudy ~ dunif(0.5,1)
  kappa ~ dpois(15)
  alpha = meanPStudy*kappa
  beta = (1-meanPStudy)*kappa
  
  
  for(i in 1:p){
    pStudy[i] ~ dbeta(alpha,beta)
    theta[i] = equals(group[i],0)*pGuess + equals(group[i],1)*pStudy[i]
  
    k[i] ~ dbin(theta[i],n)  

   }
  }
"

# JAGS usually reads models from a text file; here we use a string as a fake file.   
exammodel2.spec = textConnection(exammodel2.string)

# SAMPLING PARAMETERS
mcmciterations = 100

# Construct the object containing both the model specification as well as the data and some sampling parameters.
jagsmodel2 <- jags.model(exammodel2.spec,
                         data = list('k' = k,
                                     'n' = n,
                                     'p' = p,
                                     'pGuess' = pGuess
                                     ),
                         n.chains = 4)

# Collect samples to approximate the posterior distribution.
model2samples = coda.samples(jagsmodel2,
                           c('pStudy'), # which variables do you want to monitor?
                           n.iter = mcmciterations)

# Add your analyses on the collected samples here:

mcmcsummary_model2 = summary(model2samples)
mcmcsummary_model2 $ statistics
plotPost(model2samples)

#----------   Model 3: easy and difficult questions   --------------
# Required packages for this exercise (you may add more if you want to).
require(rjags)
require(coda)
source("DBDA2E-utilities.R")

# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!
n = 10
m = 20
pGuess = 0.5
a_hat = 1
b_hat = 1
q = rep(0.2,m)

k1 = matrix(0L, nrow = n, ncol = m)

k1[1,]  = c( 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0)
k1[2,]  = c( 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
k1[3,]  = c( 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0)
k1[4,]  = c( 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
k1[5,]  = c( 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0)
k1[6,]  = c( 1, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0)
k1[7,]  = c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0)
k1[8,]  = c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
k1[9,]  = c( 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1)
k1[10,] = c( 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0)

#for 2.3.3

k2 = matrix(0L, nrow = n, ncol = m)

k2[1,]  = c( 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0)
k2[2,]  = c( 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
k2[3,]  = c( 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0)
k2[4,]  = c( 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
k2[5,]  = c( 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0)
k2[6,]  = c( 1, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0)
k2[7,]  = c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0)
k2[8,]  = c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
k2[9,]  = c( 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1)
k2[10,] = c( 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0)
k2[1,13] = NA
k2[8,5]  = NA
k2[10,18]= NA

# THE MODEL
exammodel3.string = "
  model {

##Choose groups for students##
    for(i in 1:n){
    group[i] ~ dbern(0.5)
    }

##probabilities for questions being answered correctly##
    for(i in 1:m){
      q[i] ~ dbeta(a_hat,b_hat)
    }

  meanPStudy ~ dunif(0.5,1)
  kappa ~ dpois(15)
  alpha = meanPStudy*kappa
  beta = (1-meanPStudy)*kappa
  
  for(i in 1:n){
    ##probability theta of student i getting a question right##
    pStudy[i] ~ dbeta(alpha,beta)
    theta[i] = equals(group[i],0)*pGuess + equals(group[i],1)*pStudy[i]
    }
  
  for(i in 1:n){                     ##n is student, m is question##
    for(j in 1:m){
      prob[i,j] = q[j]*theta[i]
      k[i,j] ~ dbern(prob[i,j])
    }
  }
  }
"


# JAGS usually reads models from a text file; here we use a string as a fake file.   
exammodel3.spec = textConnection(exammodel3.string)

# SAMPLING PARAMETERS
mcmciterations = 1000

# NOTE: select either k = k1 or k = k2 in the following model

# Construct the object containing both the model specification as well as the data and some sampling parameters.
jagsmodel3 <- jags.model(exammodel3.spec,
                         data = list('k' = k1, 
                                     'n' = n,
                                     'm' = m,
                                     'pGuess' = pGuess,
                                     'a_hat' = a_hat,
                                     'b_hat' = b_hat
                                     ),
                         n.chains = 4)

# Collect samples to approximate the posterior distribution.
model3samples = coda.samples(jagsmodel3,
                             #c('k[1,13]','k[8,5]','k[10,18]'),
                             #c('q'), # question succes rates
                             c('pStudy'), # individual student success rates
                             n.iter = mcmciterations)

# Add your analyses on the collected samples here:
mcmcsummary_model3 = summary(model3samples)
mcmcsummary_model3 $ statistics
plotPost(model3samples)

#----------   Model 4: differences between groups   --------------

# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!

require(rjags)
require(coda)
source("DBDA2E-utilities.R")

n1 = 50
n2 = 49
k1 = 37
k2 = 48



# THE MODEL
exammodel4.string = "
model {
  ## Prior 
theta1 ~ dunif(0,1)
theta2 ~ dunif(0,1)
  ## Likelihood
  k1 ~ dbin(theta1, n1)
  k2 ~ dbin(theta2, n2)
  delta = abs(theta1-theta2)
}
"


# JAGS usually reads models from a text file; here we use a string as a fake file.   
exammodel4.spec = textConnection(exammodel4.string)

# SAMPLING PARAMETERS
mcmciterations = 1000

# Construct the object containing both the model specification as well as the data and some sampling parameters.
jagsmodel4 <- jags.model(exammodel4.spec,
                         data = list('k1' = k1,
                                     'k2' = k2,
                                     'n1' = n1,
                                     'n2' = n2),
                         n.chains = 4)

# Collect samples to approximate the posterior distribution.
model4samples = coda.samples(jagsmodel4,
                             c('delta','theta1','theta2'), # which variables do you want to monitor
                             n.iter = mcmciterations)

# Add your analyses on the collected samples here:
mcmcsummary_model4 = summary(model4samples)
mcmcsummary_model4 $ statistics
plotPost(model4samples[,1])

