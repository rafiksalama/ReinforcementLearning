#2 rooms, and time => state is described by temperature at every state. We have 5,17-23 temperatures, which are 8 temperatures.
#The state is the current schedule at one hour. This means that we have 8^n possible states at every hour.
#We can encode this as hour, room1, room2. with 2 rooms, we 49*24 possible states.

transition_matrix = matrix(ncol=96,nrow=96,1/96)
states = matrix(ncol=3,nrow=64*24,0)
y = 0
for(i in c(5,17:23))
  for(j in c(5,17:23))
    for(x in 0:23)
    {
      y = y+1
      states[y,] = c(i,j,x)
      #print(c(i,j,x))
    }

#The reward function checks state against current user feedback, and cost, let us we have the following PDF for R1 and R2
LoungeT = c(rep(5,6),rep(20,3),rep(5,8),rep(21,4),rep(5,3))
BedroomT = c(rep(5,5),rep(20,3),rep(5,13),rep(18,3))
ComfortTemperature = cbind(BedroomT,LoungeT)
#Importance is a matrix of room, hour, along with some probability of importance for heating the rooms.

Importance = matrix(ncol=2,nrow=24,0)
Importance[,1] = c(rep(0.2,5),rep(0.8,3),rep(0.2,13),rep(0.6,3))
Importance[,2] = c(rep(0.2,6),rep(0.8,3),rep(0.2,8),rep(0.9,4),rep(0.2,3))
#LoungeO = c(rep(0.1,17),rep(0.5,5),rep(0.2,2))
#BedroomO = c(rep(0.8,8),rep(0.1,12),rep(0.3,2),rep(0.6,2))
reward <- function(temperatureState,hour,cost,ComfortTemperature)
{
  #According to these 3 weights, we will calculate the reward of being in one state
  #bedroom, temperature reward, is if the temperature prefernce is above 10, and the state is 2, then we 
  #have a reward of the temperature weight
  #r = -(abs(temperatureState[,1] - ComfortTemperature[hour+1,1])) * Importance[hour+1,1]
  #r = r - (abs(temperatureState[,2] - ComfortTemperature[hour+1,2])) * Importance[hour+1,2]
  r = dnorm(temperatureState[,1],20,1) * Importance[hour+1,1]
  r = r + dnorm(temperatureState[,2],20,1) * Importance[hour+1,2]
  #r = r - as.integer(rowSums(temperatureState > 17) > 0) * (1-as.integer(mean(Importance[hour+1,])>=0.5))
  #To guarantee cost, we have to make sure that the sum of costs of the 24 hours is a penalty
  #The cost of heating 1 room is the same as heating 2 rooms, and the cost of heating any hour is £1
  #plot(r[1:24], col="blue", type="l",ylim=c(min(r),max(r)))
  #lines(r[25:48], col="red")
  #lines(r[73:96], col="black")
  ##lines(r[49:72], col="brown")
  r
}

#rv = reward(states,1,Temperature)
#Knowing the state value, can you get the schedule
getSchedule <- function(states,ComfortTemperature)
{
  schedule = matrix(ncol=2,nrow=24,0)
  h = c(0:23)
  for(i in 1:24)
  {
    h=i-1
    #State at one hour
    rv = reward(states[which(states[,3]==h),1:2],h,1,ComfortTemperature)
    rown = which(states[,3]==h)[which(rv==max(rv))][1]
    schedule[i,] = states[rown,1:2]
  } 
  schedule
}

costOfSchedule <- function(schedule){ sum(rowSums(schedule>17)>0)*10 }

#Let us assume the transition matrix is all 1/96 for now.
#The value function of the reward process is simply 

#The state is hour

#Optimise Schedule

#Schedule optimisation is a process of identifying the best schedule based on the importance of 
#comfort identified for every hour in every room. So every hour, we identiy the comfort and the importance of 
#comfort as the reward of adding comfort in this hour in this room, then we move to the next one. 
#We choose a schedule with less overall cost budget versus the defined budget.

#Importance of comfort is elicited at every hour versus the overall budget as the state that I have no control on
#which is in the mind of the user.

#The reward of this status is that the importance is defined by the amount of feedbacks that I put for every hour
#in every room, plus being scheduled, and the cost. 

#To have more reward, the policy would be to take an action of modifying the importance to match the importance
#of the hour.

#Occupancy, and feedback is a model of the importance 
costTarget = 150

#Markov Chain Monte Carlo, randomly move the room/hour to a discounted state by their probability, or let them stay as they are
#sample a random number for a uniform distribution, and if your probability is smaller than this number, then discount your
#probability of importance by some value

changeSchedule <- function(schedule, Importance, delta)
{
  for(i in 1:24)
  {
    for(j in 1:2)
    {
      if(runif(1) > Importance[i,j] && schedule[i,j]>17) 
      {
          schedule[i,j] = round(sh[i,j] * (1-delta*0.2))
      }
    }
  }
  schedule
}

sh = getSchedule(states,ComfortTemperature)
currentCost = costOfSchedule(sh)
deltaCost = (currentCost-150)/currentCost

while(deltaCost > 0){
  
  sh = reduceCostofASchedule(sh,Importance,deltaCost)
  currentCost = costOfSchedule(sh)
  deltaCost = deltaCost = (currentCost-150)/currentCost
  print(deltaCost)
}

