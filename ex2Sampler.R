library(rjags)
library(coda)
library(BEST)
library(stats)
source("DBDA2E-utilities.R")

#number of students
M <- 15

#number of questions
N <-40

#Probability for guessing students
pGuess <- 0.5

#Probability for studying students
aStudy <- 16
bStudy <- 4


observations <- rep(0,M)

#generate data

for(i in 1:M){

  #group: 0 is guessing, 1 is studied
  group <- sample(1,x = c(0,1), prob = c(0.5,0.5))
  
  if(group == 1){                  #studied
    # pick from distribution of probabilities for correct answer
    answerProb <- rbeta(1,aStudy,bStudy)
  }
  else{                          #guessing
    answerProb <- pGuess
  }  
  
  #now generate 40 answers for student code with 0 for wrong, 1 for correct

  correctAnswers <- rbinom(n=1,prob=answerProb, size=N)
  observations[i] <- correctAnswers
}
observations
hist(observations, breaks = 10)
