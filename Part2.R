rm(list=ls())
Q = matrix(
  c(-0.0085,  0.005,  0.0025,       0,   0.001,
          0, -0.014,   0.005,   0.004,   0.005,
          0,      0,  -0.008,   0.003,   0.005,
          0,      0,       0,  -0.009,   0.009,
          0,      0,       0,       0,       0),
  nrow = 5,
  ncol = 5)
Q <- t(Q)


## Task 7

woman = rep(1,1000) # Create 1000 women
N.iter = 1000

count = matrix(0, ncol = 5, nrow = N.iter) #

for (i in 1:N.iter){
  while (woman[i]<5){
    count[i,woman[i]] = count[i,woman[i]]+1
    woman[i]=sample(x = c(1:5), size =1, replace =TRUE, prob = P[woman[i],])
  }
}