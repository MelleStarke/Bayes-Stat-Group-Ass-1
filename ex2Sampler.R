require(rjags)
require(coda)
require(BEST)
require(stats)
source("DBDA2E-utilities.R")

#number of students
N <- 15

#number of questions
M <-40

#Probability for guessing students
aGuess <- 1
bGuess <- 1

#Probability for studying students
aStudy <- 8
bStudy <- 2


observations <- rep(0,N)

#generate data

for(i in N){

  #group: 0 is guessing, 1 is studied
  group <- sample(1,x = c(0,1), prob = c(0.5,0.5))
  
  if(group == 1){                  #studied
    # pick from distribution of probabilities for correct answer
    answerProb <- rbeta(1,aStudy,bStudy)
  }
  else{                          #guessing
    answerProb <- rbeta(1,aGuess,bGuess)
  }  
  
  #now generate 40 answers for student code with 0 for wrong, 1 for correct
  answers <- rep(0,M)
  for(m in M){
    answers[m] <- sample(1,x = c(0,1), prob = c((1-answerProb), answerProb))
  }
  correctAnswers <- Reduce("+",answers)
  observations[i] <- correctAnswers
  
}
