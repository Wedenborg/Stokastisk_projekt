#Define our Q matrix
Q = matrix(
  c(-0.0085,  0.005,  0.0025,       0,   0.001,
    0, -0.014,   0.005,   0.004,   0.005,
    0,      0,  -0.008,   0.003,   0.005,
    0,      0,       0,  -0.009,   0.009,
    0,      0,       0,       0,       0),
  nrow = 5,
  ncol = 5)
Q <- t(Q)


# Task 12 -----------------------------------------------------------------

N.women = 1000
woman = rep(1,N.women) # Create 1000 women

#Define a matrix for our states
states = matrix(0, ncol = 5, nrow = N.women) 

#Loop over the number of women
for (i in 1:N.women){
  #Run until the women is dead
  while (woman[i]<5){
    #Generate the new state, and time of change
    event = rexp(1,rate=-Q[woman[i],woman[i]])
    states[i,woman[i]] = event
    #If the woman is in state 4, the only option is 5
    if (woman[i] <4){
      woman[i]=sample(x = c((woman[i]+1):5), size =1, replace =TRUE,
                      prob =-(Q[woman[i],(woman[i]+1):5])/Q[woman[i],woman[i]])
    }
    else{
      woman[i]=5
    }
  }
}

# Create the time series based on the simulated lifetimes
# We simulate over 1680 months, a little over 137 years
Y = matrix(1, nrow = length(woman), ncol = 36)
#Run in steps of 48 hours
for (t in seq(48,1680, 48)){
  #Iterate over each of the women
  for (i in 1:dim(states)[1]){
    #Check what state the woman is in at time t
    if (t < states[i,1]){
      Y[i,t/48+1] = 1
    } else if (t < (states[i,1] + states[i,2])){
      Y[i,t/48+1] = 2
    } else if (t < (states[i,1] + states[i,2] + states[i,3])){
      Y[i,t/48+1] = 3
    } else if (t < (states[i,1] + states[i,2] + states[i,3] + states[i,4])){
      Y[i,t/48+1] = 4
    } else {
      Y[i,t/48+1] = 5
    }
  }
}

# Calculate the sum of the last column,
# it has to add up to 5000, as we simulate till all women are dead
sum(Y[,36])


# Task 13 -----------------------------------------------------------------


sample.womans = function(Q, Y, N.women=1000, N.samples=100){
  '
  Samples some women in continous time, given a Q matrix and Y vector
  
  Args:
    Q : The probability matrix
    Y : The state trajectory distribution to try and sample from
    N.women : the number of women to sample
    N.samples : the number of samples to try at each state change, in order to find a state satisfying the criterias

  Returns:
    S : the Sojurn time for each state (state 5 is by design always 0)
    N : The number of women jumping from state i to j
    states : the time spent in each state by each woman
  '
  woman = rep(1,N.women) # Create 1000 women
  states = matrix(0, ncol = 5, nrow = N.women) 
  
  N = matrix(0, ncol=5, nrow = 5)
  S = rep(0, 5)
  
  #Generate one woman at a time
  for (i in 1:N.women){
    
    #Calculate the steps where the state is changed
    state.changes = match(unique(Y[i,]), Y[i,])
    
    #Only loop over where the events where the woman changes states
    t = 0
    for(j in 2:length(state.changes)){
        #Loop until we find a candidate
        while(TRUE){
        #Generate alot of event times
        events = rexp(N.samples,rate=-Q[woman[i],woman[i]])
        
        #Generate alot of new states
        if(woman[i]<4){
          new.states = sample(x = c((woman[i]+1):5), size =N.samples, replace =TRUE,
                           prob =-(Q[woman[i],(woman[i]+1):5])/Q[woman[i],woman[i]])
        }
        #if the woman is in state 4, the only new possible state is 5
        else{
          new.states = rep(5,100)
        }
        
        #the simulated state should satisfy 
        # 1. the new state being the same
        # 2. the time of state change should be larger than the time of the step just before the timestep of Y
        # 3. the time of state change should be less than or equal to the time of the first step of that state in Y
        criteria = new.states ==  Y[i,state.changes[j]] & t+events>((state.changes[j]-1)*48-48) & t+events<=(state.changes[j]*48-48)
        
        #Check if we generated any candidates
        if(any(criteria)){
          #Just pick the first of the samples that satisfy the criteria
          new.state = new.states[criteria][1]
          event = events[criteria][1]
          
          #Update the value of N and S
          N[woman[i], new.state] = N[woman[i], new.state] + 1
          S[woman[i]] = S[woman[i]] + event
          
          #Update woman 
          states[i, woman[i]] = event
          woman[i] = new.state
          t = t + event
          
          #Break the loop, and move on to next state change
          break
        }
      }
    }
  }
  return(list("S" = S, "N" = N, "states" = states))
}

#Our initial guess of the  Q matrix, where there is equal probability of each jump
Qk = matrix(c(-0.005,0,0,0,0,
            0.00125,-0.005,0,0,0,
            0.00125,0.002,-0.005,0,0,
            0.00125,0.0015,0.0025,-0.005,0,
            0.00125,0.0015,0.0025,0.005,0),5,5)


#Initialize convergence flag
converged = FALSE
#Create vector for storing the error at each iteration
fejl = vector() 

#Run until convergence
while(!converged){
  
  #Generate a sample
  sample = sample.womans(Qk, Y, N.women = N.women)
  
  #create a copy of the current Q matrix, for comparison
  Q_old = Qk
  
  #Update the new state
  for(i in 1:5){
    for(j in 1:5){
      if(i<j){
        Qk[i,j] = sample$N[i,j]/sample$S[i] 
      }
    }
    #Reset the diagonal
    Qk[i,i] = 0
    #Update the diagonal
    Qk[i,i]=-rowSums(Qk)[i] 
  }
  
  #Add the error to the list of errors
  fejl = c(fejl,max(abs(Qk-Q_old))) 
  #Print the error, for our own sake
  print(tail(fejl,1)) 
  #Update congervence flag
  converged = max(abs(Qk-Q_old))<10^(-3)
}

