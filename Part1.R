library(expm)

# Opgave 1 ----------------------------------------------------------------
rm(list=ls())
#Define our P matrix
P = matrix(
  c(0.9915,0.005,0.0025,0     ,0.001,
    0     ,0.986,0.005 ,0.004 ,0.005,
    0     ,0    ,0.992 ,0.003 ,0.005,
    0     ,0    ,0     ,0.991 ,0.009,
    0     ,0    ,0     ,0     ,1    ),
  nrow = 5,
  ncol = 5)
P <- t(P)



N.woman = 1000


simulate.cancer <- function(N.woman=1000, t.max=Inf){
  '
  Simulates the evolution of woman who previously had breast cancer

  Args:
    N.woman : the number of woman to simulate
    t.max : the maximum number of time steps to take. If not specified we will continue until death
    P : Probability matrix, for moving between the states

  Returns:
    states : the time spent in each state for each woman
  '
  #Initialize all woman in state 1
  woman = rep(1,N.woman)
  #Define matrix for saving how long each woman spent in each state
  states = matrix(0, ncol = 5, nrow = N.woman)


  for (i in 1:N.woman){
    #Simulate each woman while either they are still alive, or until the maximum time step t.max, if specified
    t = 0
    while (!(woman[i]>4 || t>t.max)){
      #Update the time spent in the current state
      states[i,woman[i]] = states[i,woman[i]] + 1
      #Get the new state of the i'th woman
      woman[i]=sample(x = c(1:5), size =1, replace =TRUE, prob = P[woman[i],])
      #Update the time step
      t = t + 1
    }
  }
  return(list("states" = states, "woman" = woman))
}


simulation1 = simulate.cancer(N.woman)

#Number of cases with distant cancer, is all the woman that havn't been in state 2
N.distant = sum(simulation1$states[,2]==0)
#Number of cases with local cancer, is all the woman that haven been in state 2
N.locally = sum(simulation1$states[,2]!=0)

#Calculate the ratio of woman that get the cancer again locally
ratio.locally = N.locally/N.woman
sprintf("Proportion of woman for whom the cancer reappers locally is %s",(ratio.locally))

#The life time of each woman can be calculated as the sum of time spent in each state
life.time = rowSums(simulation1$states)

#plot the life time distribution
hist(life.time, main = 'Distribution of life time', xlab = 't [Months]')

# Opgave 2 ----------------------------------------------------------------


#run the simulator upto t=120
simulation.t120 = simulate.cancer(N.woman, t.max = 120)


#Plot the distribution of the states for the different women at t=120, and save the observed densities
observed = hist(simulation.t120$women, breaks = c(0:5), main='', xlab = 'State')$count


#Calculate the expected distribution
p0 = c(1,0,0,0,0)
expected = p0 %*% (P%^%120)*N.women
#Add the expected distribution to the plot
lines(c(0:4), expected, type="s", col="red")

chi2.test.samples = function(observed, expected){
  'Performs a chi2 test, given a list of observed and expected values'
  n = length(observed)
  t=0
  for (i in 1:n){
    t=t+((observed[i]-expected[i])^2)/expected[i]
  }
  pvalue = pchisq(t,df=n-1)
  return(pvalue)
}

#Perform a chi2 test to compare the emperical distribution to the theoretical
p = chi2.test.samples(observed, expected)
title(sprintf("States at t=120, p=%0.3e",p))
sprintf("P-value for chi2 test between the emperical and expected distribution of states at t=120 is  p=%s",p)


####### Opgave 3

pt = function(t){
  'Calculate the probability distribution P(T=t) of living t months'
  #Define pi, the initial distribution of states at t=0
  pi = c(1,0,0,0)
  #p0 is the probability of dying at state 1,2,3,4
  p0 = P[1:4,5]
  #Ps is the first 4 rows and columns of P
  Ps = P[1:4,1:4]
  #Create a vector for storing the values
  p = vector()

  #The matrix power operator doesn't support non-scalar input, so we have to calculate the probabilities one by one
  for (i in 1:length(t)){
    p[i] = pi %*% (Ps%^%t[i]) %*% p0
  }
  return(p)
}

#Plot the emperical distribution of life time again, here binned by every year
bins = seq(1,max(life.time)+12,12)
observed = hist(life.time, main = '', xlab = 't [Months]', breaks = bins)$count

#Calculate the expected life time distribution
expected = pt(bins[1:length(bins)-1])* 12 * N.women

#Add the expected distribution as a line on our plot
lines(bins[1:length(bins)-1],expected, col="red")

#Perform a chi2 test to compare the emperical distribution to the theoretical
p = chi2.test.samples(observed, expected)
#Update the title of the histogram with the p-value
title(sprintf('Life time  chi2 p=%0.3e',p))
sprintf("P-value for chi2 test between the emperical and expected distribution of life time is  p=%s",p)


####### Opgave 4 - Lifetime estimation
N.woman = 1000
t.max = Inf

#Initialize all woman in state 1
woman = rep(1,N.woman)
#Define matrix for saving how long each woman spent in each state
states = matrix(0, ncol = 5, nrow = N.woman)

i = 1
while (i <=1000){
  #Simulate each woman while either they are still alive, or until the maximum time step t.max, if specified
  t = 0
  accept = FALSE
  while (!(woman[i]>4 || t>t.max)){
    #Update the time spent in the current state
    states[i,woman[i]] = states[i,woman[i]] + 1
    #Get the new state of the i'th woman
    woman[i]=sample(x = c(1:5), size =1, replace =TRUE, prob = P[woman[i],])
    #Update the time step
    t = t + 1
    if (states[i,1]>12){
      woman[i]=1
      states[i,]=c(0,0,0,0,0)
    }
  }
  if (states[i,1]<12 && sum(states[i,])>12){
    i = i +1
  } else {
    woman[i]=1
    states[i,]=c(0,0,0,0,0)
  }
}
## Lifetime calculate mean and var
mean(rowSums(states))
var(rowSums(states))
max(rowSums(states))
min(rowSums(states))
sd(rowSums(states))
hist(rowSums(states))



########## Opgave 5

fraction = vector()
control = vector()
for (i in 1:100){
  Sim = simulate.cancer(200,Inf)
  fraction[i] = sum(rowSums(Sim$states)<=350)/200
  control[i] = sum(rowSums(Sim$states))/200
}

X = fraction
Y = control

X_fractionbar = mean(X)
Conf_fraction1 = mean(X) - qt(0.975, df =n-1) * sd(X) / sqrt(n)
Conf_fraction2 = mean(X) + qt(0.975, df =n-1) * sd(X) / sqrt(n)
c(X_controlbar,Conf_fraction1,Conf_fraction2)



#Define pi, the initial distribution of states at t=0
pi = c(1,0,0,0)
#Ps is the first 4 rows and columns of P
Ps = P[1:4,1:4]
# vector of ones with appropiate dimensions
ones = c(1,1,1,1)
# II enhedsmatrice
II = diag(4)
E = pi%*%solve(II-Ps)%*%ones
cs = -cov(fraction,control)/var(control)
Z = fraction + cs*(control-E)
X_controlbar=mean(Z)

n = 100
Conf_control1 = mean(Z) - qt(0.975, df =n-1) * sd(Z) / sqrt(n)
Conf_control2 = mean(Z) + qt(0.975, df =n-1) * sd(Z) / sqrt(n)
c(X_controlbar,Conf_control1,Conf_control2)


#Compare the two variance calculations
var(X)
var(Z)

reduction = var(X)/var(Z)
reduction


