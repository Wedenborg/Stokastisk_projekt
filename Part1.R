##### Opgave 1
rm(list=ls())
P = matrix(
  c(0.9915,0.005,0.0025,0,0.0001,0,0.986,0.005,0.004,
    0.005,0,0,0.992,0.003,0.005,0,0,0,0.991,0.009,0,0,0,0,1),
  nrow = 5,
  ncol = 5)
P <- t(P)

woman = rep(1,1000)
N.iter = length(woman)
i = 1
count = matrix(0, ncol = 5, nrow = N.iter)
for (i in 1:N.iter){
  while (woman[i]<5){
    count[i,woman[i]] = count[i,woman[i]]+1
    woman[i]=sample(x = c(1:5), size =1, replace =TRUE, prob = P[woman[i],])
    }
  }

distant = sum(count[,2]==0)
locally = sum(count[,2]!=0)
hist(rowSums(x = count))
mean(rowSums(count))
sameplace = locally/N.iter*100
