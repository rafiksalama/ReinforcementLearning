q.learn <- function(R, N, alpha, gamma, tgt.state) {
  ## initialize Q to be zero matrix, same size as R
  Q <- matrix(rep(0,length(R)), nrow=nrow(R))
  ## loop over episodes
  for (i in 1:N) {
    ## for each episode, choose an initial state at random
    cs <- sample(1:nrow(R), 1)
    ## iterate until we get to the tgt.state
    while (1) {
      ## choose next state from possible actions at current state
      ## Note: if only one possible action, then choose it;
      ## otherwise, choose one at random
      next.states <- which(R[cs,] > -1)
      if (length(next.states)==1)
        ns <- next.states
      else
        ns <- sample(next.states,1)
      ## this is the update
      Q[cs,ns] <- Q[cs,ns] + alpha*(R[cs,ns] + gamma*max(Q[ns, which(R[ns,] > -1)]) - Q[cs,ns])
      ## break out of while loop if target state is reached
      ## otherwise, set next.state as current.state and repeat      
      if (ns == tgt.state) break
      cs <- ns
    }
  }
  ## return resulting Q normalized by max value
  return(100*Q/max(Q))
}

N <- 1000
alpha <- 1
gamma <- 0.8
tgt.state <- 6
R <- matrix(c(-1,-1,-1,-1,0,-1,-1,-1,-1,0,-1,0,-1,-1,-1,0,-1,-1,-1,0,0,-1,0,-1,0,-1,-1,0,-1,0,-1,100,-1,-1,100,100),nrow=6)
print(R)

Q <- q.learn(R,N,alpha,gamma,tgt.state)
print(Q)

#The state is the price, the new state is the new price. The actions are the increase, decrease, or do nothing.
#The actions are either decrease or increase, or stay as you are. The reward are determined based on being declined, 
#ignored, or accepted.

#Now the main point is identifying the action to take at a specific budget, we do not really know the end goal, but we know is
#the reward that we get at every state.

#So if I have a Q of state and action. I will need to design the initial values based on the initial preference, let us start by the following.
#State space is 100, from £0 to £99 per week. There are some states that seems impossible to get to.
#We will need to setup the reward that attracts towards lower budgets, for cost savvy people, etc.
#We need now to setup the initial data structure, and the update proces.

#We need to store parameters
alpha <- 1
gamma <- 0.8
#We need to store this Q
Q <- matrix(rep(0,30), nrow=10)
reward = c(10,-10,0) #accepted, denied, ignored
action = c(1,2,3) #increase, decrease, stay as you are
#We do not know what will the state be, but we just take an action, wait for the reward from it.
updateQ <- function(ps,cs,actiontaken,reward){
  Q[ps,actiontaken] <- Q[ps,actiontaken] + alpha*(reward + gamma*max(Q[cs, which(Q[cs,]==max(Q[cs,]))[1]]) - Q[ps,actiontaken])  
  #What is the next action
  Q
}

Q

#Let us see if we get there now. my target is £5, my initial state is £8, taken an action to decrease so 2, my current state is £7
#updateQ(8,7,2,-10)
#Let us start by taking the next recommended action, and incrementing by 1 or decrementing by 1
ps = 1
cs = 2
action = 1
for(i in 1:1000)
{
  #get reward of current state
  if(cs == 5) reward = 10
  else reward = sample(c(0,-10),1,replace = F)
  Q = updateQ(ps,cs,action,reward)
  #get next action for the current state
  ps = cs
  action = sample(which(Q[cs,]==max(Q[cs,])),1,F)
  #take action
  if(action == 1 && ps<10) cs = ps+1
  else if(action ==2 && ps>1) cs = ps-1
  else cs = ps
  print(paste(ps,cs,action, reward))
}

Q




