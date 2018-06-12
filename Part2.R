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

  p = vector()
  for (i in 1:length(t)){
    p[i] = 1 - p0%*%expm(x = Q*t[i])%*%ones
  }
  return(p)

}
plot(opg8,type ='l',lwd = 3)
lines(ecdf(lifeTime),col='red',lwd = 3)

ks.test(lifeTime, funk)

########## Opgave 9
# Q matrix for treatment
Q2 = matrix(c(-0.0085,0,0,0,0
              ,0.0025,-0.014,0,0,0
              ,0.00125,0,-0.008,0,0
              ,0,0.002,0.003,-0.009,0
              ,0.001,0.005,0.005,0.009,0),5,5)

woman = rep(1,1000) # Create 1000 women
N.iter = 1000

count_treat = matrix(0, ncol = 5, nrow = N.iter) #

for (i in 1:N.iter){
  while (woman[i]<5){
    event = rexp(1,rate=-Q2[woman[i],woman[i]])
    count_treat[i,woman[i]] = event
    if (woman[i] <4){
      woman[i]=sample(x = c((woman[i]+1):5), size =1, replace =TRUE,
                      prob =-(Q2[woman[i],(woman[i]+1):5])/Q2[woman[i],woman[i]])
    }
    else{
      woman[i]=5
    }

  }
}

# S function for non-treatment
life_cdf=ecdf(lifeTime)

S_func = function(dt , t){
  S = (1000-dt(t)*1000)/1000
       return(S)
}
plot(S_func(life_cdf,1:1000))

# find s for treatment
lifeTimeTreat =rowSums(count_treat)
lifeTreat_cdf=ecdf(lifeTimeTreat)
plot(S_func(lifeTreat_cdf,1:1000))


# plot with death rate for treat and non-treat
plot(S_func(life_cdf,1:1000),col = 'red',type='l')
lines(S_func(lifeTreat_cdf,1:1000),col='blue',type='l')


#### Opgave 10
# Log rank test
N = (S_func(life_cdf,1:1000)+S_func(lifeTreat_cdf,1:1000))*1000
diff(N)
for (j in 1:1000){
  oo=oo+O[j]
  N[j] = 2000-O
}


