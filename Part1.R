##### Opgave 1
rm(list=ls())
P = matrix(
  c(0.9915,0.005,0.0025,0,0.001,0,0.986,0.005,0.004,
    0.005,0,0,0.992,0.003,0.005,0,0,0,0.991,0.009,0,0,0,0,1),
  nrow = 5,
  ncol = 5)
P <- t(P)

woman = rep(1,1000)
N.iter = length(woman)
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

######## Opgave 2
woman = rep(1,1000)
N.iter = length(woman)

for (i in 1:N.iter){
  counter = 0
  while (counter < 120){
    count[i,woman[i]] = count[i,woman[i]]+1
    woman[i]=sample(x = c(1:5), size =1, replace =TRUE, prob = P[woman[i],])
    counter = counter + 1
  }
}
library(expm)
hist(rowSums(x = count))
woman[woman!=1]
hist(woman)
p0 = c(1,0,0,0,0)
p_t=p0%*%(P%^%120)
observed = as.vector(p_t*1000)
expected =as.vector(table(woman))

t=0
for (i in 1:length(observed)){
  t=t+((observed[i]-expected[i])^2)/expected[i]
} # Slaa op i qhisq tabel med df=4
pvalue = 1- pchisq(t,df=4) # Vi afviser ikke!

####### Opgave 3



