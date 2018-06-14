#################### Part 3
Q = matrix(
  c(-0.0085,  0.005,  0.0025,       0,   0.001,
    0, -0.014,   0.005,   0.004,   0.005,
    0,      0,  -0.008,   0.003,   0.005,
    0,      0,       0,  -0.009,   0.009,
    0,      0,       0,       0,       0),
  nrow = 5,
  ncol = 5)
Q <- t(Q)


## Task 12

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
Y = matrix(1, nrow = length(woman), ncol = 26)
for (t in seq(48,1200, 48)){
  for (i in 1:dim(count)[1]){
    if (t < count[i,1]){
      Y[i,t/48+1] = 1
    } else if (t< (count[i,1]+count[i,2])){
      Y[i,t/48+1] = 2
    } else if (t< (count[i,1]+count[i,2]+count[i,3])){
      Y[i,t/48+1] = 3
    } else if (t< (count[i,1]+count[i,2]+count[i,3]+count[i,4])){
      Y[i,t/48+1] = 4
    } else {
      Y[i,t/48+1] = 5
    }
  }
}
sum(Y[,26])

############ Opgave 13

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
    if (woman[i]!=Y[i,t]){
      woman[i]=1
    }
  }

}
