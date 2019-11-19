library(ReinforcementLearning)
data("tictactoe")
head(tictactoe, 5)
env <- gridworldEnvironment
print(env)
states <- c("s1", "s2", "s3", "s4")
actions <- c("up", "down", "left", "right")
data <- sampleExperience(N = 1000, env = env, states = states, actions = actions)

# The following example shows how to teach a reinforcement learning agent using input data in the form of sample 
# sequences consisting of states, actions and rewards. The ‘data’ argument must be a dataframe object in which 
# each row represents a state transition tuple (s,a,r,s_new). Moreover, the user is required to specify the column 
# names of the individual tuple elements in ‘data’.

# Define reinforcement learning parameters
control <- list(alpha = 0.1, gamma = 0.5, epsilon = 0.1)

# Perform reinforcement learning
model <- ReinforcementLearning(data, s = "State", a = "Action", r = "Reward", 
                               s_new = "NextState", control = control)

# Sample N = 1000 sequences from the environment using epsilon-greedy action selection
data_new <- sampleExperience(N = 1000, env = env, states = states, actions = actions, 
                             model = model, actionSelection = "epsilon-greedy", 
                             control = control)
head(data_new)

model_new <- ReinforcementLearning(data_new, s = "State", a = "Action", r = "Reward", 
                                   s_new = "NextState", control = control, model = model)
print(model_new)

#For every current state and next state, you need to learn the associated reward, and best action.
model <- ReinforcementLearning(tictactoe, s = "State", a = "Action", r = "Reward", 
                               s_new = "NextState", iter = 1, control = control)

policy(model)
