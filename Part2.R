library(expm)
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
t = c(1:1000)
plot(funk(t),type ='l',lwd = 3)
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

cdf = function(lifeTime){
  bins = sort(lifeTime)
  cdf.value = cumsum(hist(lifeTime, breaks = bins, plot=FALSE)$count)/1000
  return(list("t"=tail(bins,-1), "cdf"=cdf.value))
}

# S function for non-treatment

life_cdf=cdf(lifeTime)

S_func = function(cdf){
  S = (1000-cdf$cdf*1000)/1000
       return(S)
}
plot(life_cdf$t,S_func(life_cdf),'l')

# find s for treatment
lifeTimeTreat =rowSums(count_treat)
lifeTreat_cdf=cdf(lifeTimeTreat)
plot(lifeTreat_cdf$t,S_func(lifeTreat_cdf),'l')

  
# plot with death rate for treat and non-treat
plot(life_cdf$t,S_func(life_cdf),col = 'red',type='s')
lines(lifeTreat_cdf$t,S_func(lifeTreat_cdf),col='blue',type='s')


#### Opgave 10
# Log rank test
N1 = head(S_func(life_cdf)*1000,-1)
N2 = head(S_func(lifeTreat_cdf)*1000,-1)
N = N1+N2

O1 = abs(diff(N1))
O2 = abs(diff(N2))
O = abs(diff(N))

N = tail(N,-1)
N2 = tail(N2,-1)

E2 = O/N*N2

V = (O*(N2/N)*(1-N2/N)*(N-O))/(N-1)

Z = sum(O2-E2)/sum(V)

pValue = 1-pnorm(Z)

mean(V)
