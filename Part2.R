############# Part 2
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


############# Task 7
#Initializations
woman = rep(1,1000) # Create 1000 women
N.iter = 1000
count = matrix(0, ncol = 5, nrow = N.iter) #

# Simulate the lifetime of the women
for (i in 1:N.iter){
  while (woman[i]<5){ #Woman has to have state under 5. otherwise they are dead 
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

# Visualize the lifetime for the women
lifeTime = rowSums(count)
hist(lifeTime)
C = qchisq(p = c(0.025,0.975), df = 999)

# Confidence interval for standard deviation
conf=c(sqrt(999*var(lifeTime)/C[2]),sqrt(999*var(lifeTime)/C[1]))
mean(lifeTime)

n = length(lifeTime)
Conf_control1 = mean(lifeTime) - qt(0.975, df =n-1) * sd(lifeTime) / sqrt(n)
Conf_control2 = mean(lifeTime) + qt(0.975, df =n-1) * sd(lifeTime) / sqrt(n)
c(mean(lifeTime),Conf_control1,Conf_control2) #mean and CI for the mean

#survival rate after 30.5 months
(sum(rowSums(count[,1:2])>30.5 & (count[,3]!=0 | count[,4]!=0)))/1000

############# Task 8
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

plot(o,type ='l',lwd = 3)
lines(ecdf(lifeTime),col='red',lwd = 3)
ks.test(lifeTime, funk)

########## Task 9
# Q matrix for treatment
Q2 = matrix(c(-0.00475,0,0,0,0
              ,0.0025,-0.007,0,0,0
              ,0.00125,0,-0.008,0,0
              ,0,0.002,0.003,-0.009,0
              ,0.001,0.005,0.005,0.009,0),5,5)


woman = rep(1,1000) # Create 1000 women
count_treat = matrix(0, ncol = 5, nrow = N.iter) #

# Simulate the lifetime of the women with the treatment matrix
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

# Survival function for non-treatment
life_cdf=cdf(lifeTime)

S_func = function(cdf){
  S = (1000-cdf$cdf*1000)/1000
       return(S)
}
plot(life_cdf$t,S_func(life_cdf),'l')

# Survival function for treatment
lifeTimeTreat =rowSums(count_treat)
lifeTreat_cdf=cdf(lifeTimeTreat)
plot(lifeTreat_cdf$t,S_func(lifeTreat_cdf),'l')

# Plot with death rate for treat and non-treat
plot(life_cdf$t,S_func(life_cdf),col = 'red',type='s',xlab = 't [months]', ylab = 'Women_Alive')
lines(lifeTreat_cdf$t,S_func(lifeTreat_cdf),col='blue',type='s')
title('Survival functions')


############# Task 10
# Log rank test
N1 = head(S_func(life_cdf)*1000,-1)
N2 = head(S_func(lifeTreat_cdf)*1000,-1)
N = N1+N2

O1 = abs(diff(N1))
O2 = abs(diff(N2))
O = abs(diff(N))

N = tail(N,-1)
N2 = tail(N2,-1)

#Calculate the expected value
E2 = O/N*N2

#Calculate the variance
V = (O*(N2/N)*(1-N2/N)*(N-O))/(N-1)

#Calculate the test statistic
Z = sum(O2-E2)/sum(V)

#Find the p-value
pValue = 1-pnorm(Z)
