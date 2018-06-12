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
    event = rexp(1,rate=-Q[woman[i],woman[i]])
    count[i,woman[i]] = event
    if (woman[i] <4){
    woman[i]=sample(x = c((woman[i]+1):5), size =1, replace =TRUE,
                    prob =-(Q[woman[i],(woman[i]+1):5])/Q[woman[i],woman[i]])
    }
    else{
      woman[i]=5
    }

  }
}

lifeTime = rowSums(count)
hist(lifeTime)
C = qchisq(p = c(0.025,0.975), df = 999)
# Conf for stadard deviation: see notes part 2 file
conf=c(sqrt(999*var(lifeTime)/C[2]),sqrt(999*var(lifeTime)/C[1]))
mean(lifeTime)

n = length(lifeTime)
Conf_control1 = mean(lifeTime) - qt(0.975, df =n-1) * sd(lifeTime) / sqrt(n)
Conf_control2 = mean(lifeTime) + qt(0.975, df =n-1) * sd(lifeTime) / sqrt(n)
c(mean(lifeTime),Conf_control1,Conf_control2) #mean and CI for the mean

#survival rate after 30.5 months
(sum(rowSums(count[,1:2])>30.5 & (count[,3]!=0 | count[,4]!=0)))/1000

############# Opgave 8
Q = Q[1:4,1:4]
funk = function(t){
  ones = rep(1,4)
  p0 = c(1,0,0,0)
  1 - p0%*%expm(x = Q*t)%*%ones

}
opg8=vector()
Elife = vector()
for (i in 1:1000){
opg8[i] = funk(i)
Elife = ecdflife(i)
}
plot(opg8)
lines(ecdf(lifeTime),col='cyan',lwd = 3)

ecdflife = ecdf(lifeTime)
ks.test(lifeTime, 'funk',1,1000)
