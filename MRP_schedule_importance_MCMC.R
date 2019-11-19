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
LoungeT = c(rep(5,6),rep(20,3),rep(5,8),rep(5,7))
BedroomT = c(rep(5,5),rep(20,3),rep(5,10),rep(20,3),rep(5,3))
priorSchedule = cbind(LoungeT,BedroomT)
#Importance is a matrix of room, hour, along with some probability of importance for heating the rooms.

Importance = matrix(ncol=2,nrow=24,0)
Importance[,1] = c(rep(0.2,6),rep(0.2,3),rep(0.2,8),rep(0.9,7))
Importance[,2] = c(rep(0.2,5),rep(0.7,3),rep(0.2,13),rep(0.2,3))
colnames(Importance) <- c("Lounge","Bedroom")
T20 = c(0.01,0.09,0.1,0.15,0.3,0.15,0.1,0.05,0.05)
T5 = c(0.3,0.2,0.2,0.1,0.1,0.03,0.03,0.02,0.02)
c(0,cumsum(T5))

LoungeComfort = matrix(ncol=9,nrow=24,0)
LoungeComfort =  rbind(matrix(rep(T5,6),byrow = T, ncol=9),matrix(rep(T20,3),byrow = T, ncol=9),matrix(rep(T5,8),byrow = T, ncol=9),matrix(rep(T20,7),byrow = T, ncol=9))
BedroomComfort = matrix(ncol=9,nrow=24,0)
BedroomComfort =  rbind(matrix(rep(T5,5),byrow = T, ncol=9),matrix(rep(T20,3),byrow = T, ncol=9),matrix(rep(T5,10),byrow = T, ncol=9),matrix(rep(T20,3),byrow = T, ncol=9),matrix(rep(T5,3),byrow = T, ncol=9))
comfortMean = list(LoungeComfort,BedroomComfort)
colnames(comfortMean[[1]]) <- c(5,17:24)
colnames(comfortMean[[2]]) <- c(5,17:24)
mostComfortable <- function(multinomialDistibutions){
  mc = vector()
  for(i in 1:24) mc[i] = c(5,17:24)[which.max(multinomialDistibutions[i,])]
  mc
}
expectedTemp = cbind(ceiling(apply(matrix(c(1:24)),1,function(x){sum(c(5,17:24)*comfortMean[[1]][x,])})),ceiling(apply(matrix(c(1:24)),1,function(x){sum(c(5,17:24)*comfortMean[[2]][x,])})))
expectedTemp = cbind(mostComfortable(comfortMean[[1]]),mostComfortable(comfortMean[[2]]))
expectedTemp[expectedTemp<17] = 17

#comfortMean = cbind(c(rep(5,6),rep(20,3),rep(5,8),rep(20,7)),c(rep(5,5),rep(20,3),rep(5,10),rep(20,3),rep(5,3)))
#colnames(comfortMean) <- c("Lounge","Bedroom")

#Probability of costing something is a function of temperature, so we can assume that P(cost) is poisson distribution
#centered around 21

#LoungeO = c(rep(0.1,17),rep(0.5,5),rep(0.2,2))
#BedroomO = c(rep(0.8,8),rep(0.1,12),rep(0.3,2),rep(0.6,2))
scheduleDiscomfortProbability <- function(schedule,expectedTemp,Importance)
{
  schedule[schedule<17] = 17
  s = 0
  t = 0
  denom = 0
  for(i in 1:ncol(schedule))
  {
    #prop = apply(schedule,2,function(x){as.numeric(comfortMean[[i]][,which(c(5,17:24)==5)])})[,i]
    s = s + sum((Importance[,i])*abs(schedule[,i]-expectedTemp[,i]+c(0,diff(schedule[,i]))))
    denom = denom + sum((Importance[,i])*expectedTemp[,i])
  }
  s/denom
}

ScheduleStability <- function(schedule){
  p1 = table(diff(schedule[,1]))/24
  p2 = table(diff(schedule[,2]))/24
  abs((1-sum(-p1*log(p1)))+(1-sum(-p2*log(p2))))/2
  
}

costOfSchedule <- function(schedule){
  schedule[schedule<17] = 17
  maxT = apply(schedule,1,max)
  nroom = apply(schedule,1,function(x){sum(x>10)})
  pTemp = diff(c(maxT[24],maxT))
  pTemp = replace(pTemp,pTemp<0,0)
  sum(maxT*0.3+nroom*0.2+pTemp*0.7)
  #sum(rowSums(schedule>17)>0)*10
}

deltaCost <- function(schedule,targetCost){
  currentCost = costOfSchedule(schedule)
  ifelse(currentCost>targetCost,(currentCost-targetCost)/targetCost,0)
  #maxCost/(maxCost - currentCost) - 1
  
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
costTarget = 150

#Markov Chain Monte Carlo, randomly move the room/hour to a discounted state by their probability, or let them stay as they are
#sample a random number for a uniform distribution, and if your probability is smaller than this number, then discount your
#probability of importance by some value

changeSchedule <- function(schedule, Importance,maxChanges,penalizeImportance)
{
  #x = 0
  #rImportance = runif(48)
  sa = sample(c(1:48),maxChanges,F)
  m = 0
  for(x in sa)
  {
      m = m+1
      j = ceiling(x / 24)
      i = x - ((j-1)*24)
      cs = c(0,cumsum(comfortMean[[j]][i,]))
      u = runif(1)
      schedule[i,j] = min(as.numeric(names(which(cs>u))))
      #schedule[i,j] = sample(c(5,17:24),1,F)
  }
  schedule
}

accept <- function(valueOfSchedule,currentValue,k){
  Temp <- (1 - 0.01)^k
  random <- runif(1,0,1)
  dif <- valueOfSchedule-currentValue
  (valueOfSchedule< currentValue || runif(1, 0, 1) < exp(-(valueOfSchedule - currentValue) / Temp))
}

#Suggest a schedule
maxCost = costOfSchedule(cbind(rep(25,nrow(priorSchedule)),rep(25,nrow(priorSchedule))))
TargetCost = 140
comfortWeight = 0.5
priorValue = scheduleDiscomfortProbability(priorSchedule,expectedTemp,Importance)*comfortWeight + deltaCost(priorSchedule,TargetCost) * (1-comfortWeight)
currentSchedule = priorSchedule
currentValue = priorValue
print(currentValue)
savedSchedule = currentSchedule
minValue = priorValue
posterior = matrix(ncol=9,nrow=48,0)
u = runif(100000)
for(i in 1:10000)
{
  suggestedSchedule = changeSchedule(currentSchedule,Importance,1,0.01)
  valueOfSchedule = scheduleDiscomfortProbability(suggestedSchedule,expectedTemp,Importance)*comfortWeight + deltaCost(suggestedSchedule,TargetCost) * (1-0.2-comfortWeight)
  #print(paste(currentValue,valueOfSchedule))
  #print(paste(1-(currentValue/valueOfSchedule),u[i]))
  #sprint((valueOfSchedule-currentValue)/i)
  
  if(accept(valueOfSchedule,currentValue,i)){
  #if(valueOfSchedule < currentValue+(currentValue*runif(1,0,0.1)) ){
    currentSchedule = suggestedSchedule
    currentValue = valueOfSchedule
    vv = as.vector(suggestedSchedule)
    for(j in 1:48) posterior[j,which(vv[j] == c(5,17:24))] = posterior[j,which(vv[j] == c(5,17:24))] + 1
    if(currentValue<minValue) {
      minValue = currentValue
      savedSchedule =  currentSchedule
      print(minValue)
    }
    #print(i)
  }
}

print(savedSchedule)
print(costOfSchedule(savedSchedule))

currentScheduleC = currentSchedule
for(x in 1:48)
{
  j = ceiling(x / 24)
  i = x - ((j-1)*24)
  currentScheduleC[i,j] = c(5,17:24)[which(posterior[x,] == max(posterior[x,]))]
}

valueOfScheduleC = scheduleDiscomfortProbability(currentScheduleC,expectedTemp,Importance)*comfortWeight + deltaCost(currentScheduleC,TargetCost) * (1-comfortWeight)
print(currentScheduleC)
print(valueOfScheduleC)
valueOfSchedule = scheduleDiscomfortProbability(currentSchedule,expectedTemp,Importance)*comfortWeight + deltaCost(currentSchedule,TargetCost) * (1-comfortWeight)
print(valueOfSchedule)

# sh = getSchedule(states,ComfortTemperature)
# currentCost = costOfSchedule(sh)
# deltaCost = (currentCost-150)/currentCost
# 
# while(deltaCost > 0){
#   sh = reduceCostofASchedule(sh,Importance,deltaCost)
#   currentCost = costOfSchedule(sh)
#   deltaCost = deltaCost = (currentCost-150)/currentCost
#   print(deltaCost)
# }

