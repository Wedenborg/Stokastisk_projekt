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

woman = rep(1,10) # Create 1000 women
N.iter = 10
TT = seq(48,1200, 48)

count = matrix(0, ncol = 5, nrow = N.iter) #
i = 1
while (i<=10){
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
  Y_test = vector()
  Y_test[1] = 1
  for (t in TT){
    if (t < count[i,1]){
      Y_test[t/48+1] = 1
    } else if (t< (count[i,1]+count[i,2])){
      Y_test[t/48+1] = 2
    } else if (t< (count[i,1]+count[i,2]+count[i,3])){
      Y_test[t/48+1] = 3
    } else if (t< (count[i,1]+count[i,2]+count[i,3]+count[i,4])){
      Y_test[t/48+1] = 4
    } else {
      Y_test[t/48+1] = 5
    }
  }
  if (all(Y_test == Y[i,])){
    i = i+1
    print(i)
  } else {
    woman[i] = 1
  }

}


Q0 = matrix(c(-0.005,0,0,0,0,
            0.00125,-0.005,0,0,0,
            0.00125,0.002,-0.005,0,0,
            0.00125,0.0015,0.0025,-0.005,0,
            0.00125,0.0015,0.0025,0.005,0),5,5)
rowSums(Q0)

S = vector()
for (i in 1:5){
  count_test = count
  for (j in 1:5){
    if (i < j ){
    N[i,j] = sum(count_test[i] > 0 && count_test[j] > 0 )
    # If a person has jumped from i to j they are not count until new i ´.
    count_test[count_test[i] > 0 && count_test[j] > 0] = 0
    }
  }
  S[i] = sum(coun[,i])
  }
Q1[i,j] = N[i,j]/S[i]


#####################
## Task 13 2.0
