############# Extra
library(expm)
rm(list=ls())

#Initialize Q-matrix
Q = matrix(
  c(-0.0085,  0.005,  0.0025,       0,   0.001,
    0, -0.014,   0.005,   0.004,   0.005,
    0,      0,  -0.008,   0.003,   0.005,
    0,      0,       0,  -0.009,   0.009,
    0,      0,       0,       0,       0),
  nrow = 5,
  ncol = 5)
Q <- t(Q)


############# Task 11
#Initializations
woman = rep(1,1000) # Create 1000 women
N.iter = 1000
count = matrix(0, ncol = 5, nrow = N.iter) #
variable = 1
k = 2
# Simulate the lifetime of the women
for (i in 1:N.iter){
  while (woman[i]<5){ #Woman has to have state under 5. otherwise they are dead
    event = rexp(1,rate=-k * Q[woman[i],woman[i]])
    count[i,woman[i]] = count[i,woman[i]] + event
    if (woman[i] <4 & variable == k){
      woman[i]=sample(x = c((woman[i]+1):5), size =1, replace =TRUE,
                      prob =-(Q[woman[i],(woman[i]+1):5])/Q[woman[i],woman[i]])
      variable = 1
    }
    else if (variable != k){
      variable = variable +1
    } else {
      woman[i]=5
      variable = 1
    }
  }
}
hist(rowSums(count))
