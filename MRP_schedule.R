#2 rooms, and time => state is described by 3 place holders.
#[T1,T2,ti], 4*24 = 96 possible states
transition_matrix = matrix(ncol=96,nrow=96,1/96)
states = matrix(ncol=3,nrow=96,0)
y = 0
for(i in 0:1)
{
  for(j in 0:1)
  {
    for(x in 0:23)
    {
      y = y+1
      states[y,] = c(i,j,x)
      print(c(i,j,x))
    }
  }
}

#The reward function checks state against current user feedback, and cost, let us we have the following PDF for R1 and R2
LoungeT = c(rep(10,6),rep(20,3),rep(10,8),rep(21,4),rep(10,3))
BedroomT = c(rep(10,5),rep(20,3),rep(10,13),rep(18,3))
LoungeO = c(rep(0.1,17),rep(0.5,5),rep(0.2,2))
BedroomO = c(rep(0.8,8),rep(0.1,12),rep(0.3,2),rep(0.6,2))

reward <- function(states,cost,occupancy,comfortweight,occupancyweight)
{
  #According to these 3 weights, we will calculate the reward of being in one state
  #bedroom, temperature reward, is if the temperature prefernce is above 10, and the state is 2, then we 
  #have a reward of the temperature weight
  heatingLoungeComfort = as.integer(LoungeT!=10)
  heatingBedroomComfort = as.integer(BedroomT!=10)
  r = as.integer(states[,1] == heatingLoungeComfort) * comfortweight
  r = r + as.integer(states[,2] == heatingBedroomComfort) * comfortweight
  #Penalise for low occupancy
  LoungeOccupancy = as.integer(LoungeO > occupancy)
  BedroomOccupancy = as.integer(BedroomO > occupancy)
  r = r - as.integer(states[,1] != LoungeOccupancy) * occupancyweight
  r = r - as.integer(states[,2] != BedroomOccupancy) * occupancyweight
  #To guarantee cost, we have to make sure that the sum of costs of the 24 hours is a penalty
  #The cost of heating 1 room is the same as heating 2 rooms, and the cost of heating any hour is £1
  plot(r[1:24], col="blue", type="l",ylim=c(min(r),max(r)))
  lines(r[25:48], col="red")
  lines(r[49:72], col="brown")
  lines(rv[73:96], col="black")
  r
}

rv = reward(states,1,0.1,1,10)
#Knowing the state value, can you get the schedule
getSchedule <- function(reward,states)
{
  schedule = matrix(ncol=2,nrow=24,0)
  h = c(0:23)
  for(i in 1:24)
  {
    h=i-1
    rown = which(states[,3]==h)[which(reward[which(states[,3]==h)]==max(reward[which(states[,3]==h)]))][1]
    schedule[i,] = states[rown,1:2]
  } 
  schedule
}

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




