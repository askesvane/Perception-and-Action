# Preprocessing of handpriming class data (Hurdle Race experiment)
# F20 Models of Perception and Action

# Load libraries
library(ggplot2)
library(signal)
library(lmerTest)
library(dplyr)

rm(list = ls())


# ---- 1: Planning the analysis ---- 
# Before you start, remind yourself in the group about what the experiment was like and what the conditions were
# (we have a 2 x 2 design). We will focus this analysis on the height of the movement over the target. 
# Discuss in the group to make sure that makes sense to you. 


# ---- 2: Loading data ---- 
# Load the data using this code snippet. Then look at the data frame and make sure you understand what each column means. 

# Working directory
setwd('C:/Users/askes/Dropbox/University/Cognitive Science/4. semester/Models for perception and action/Perception-and-Action')
library("tidyverse","pacman","signal")

# Load data
data_all <- read_csv('data_all.csv')

# ---- 13: Make a loop to preprocess all data ----
# Use loops to go through every participant and every trial, to do the preprocessing steps for all the data.


# Make empty data frame to be filled
data <- tibble(
  ID = factor(), 
  Gender = factor(), 
  Age = factor(), 
  Mouse = factor(), 
  Trial = factor(), 
  Trial_duration = numeric(), 
  Target_height = numeric(),
  Distract = factor(), 
  Target = factor()
)


# Number of unique trials
num_trials <- max(unique(data_all$id))*max(unique(data_all$trial))


crt_id = 1 # there are 19 unique id's
crt_trial = 1 # there are 40 trials


# Loop through all participant
for (i in 1:num_trials){
  
  # ---- 3: First plotting ---- 
  # subset for one trial
  trial <- dplyr::filter(data_all, trial == crt_trial & id == crt_id)
  
  # Plotting 
  #ggplot(trial) + geom_line(aes(x,y))
  

  
  
  
  # ---- 4: Recoding into a more intuitive reference frame ---- 
  # Modify x and y in a way that the fixed starting point in each trial is located at 0/0. Plot the trial again to verify it worked.
  
  # Recoding to new zero
  x_offset = .9
  y_offset = .9
  
  trial <- trial %>% mutate(
    x = x + x_offset,
    y = y + y_offset
  )
  
  # Plotting 
  #ggplot(trial) + geom_line(aes(x,y))
  
  
  
  
  
  # ---- 5: y velocity ---- 
  # Make a new column and calculate y velocity. Remember, velocity is the difference in position (here: y) divided by the difference in time.
  # You will most likely get an error message. Discuss in the group why and what could be done about it. (Think pragmatically!) 
  
  # Make velocity column
  trial <- trial %>% mutate(
    velocity_y = (y - lag(y)) / (t - lag(t))
  )
  
  
  
  
  # ---- 6: Plotting y velocity ---- 
  # Make a plot with t against y and overlay with t against y_vel (ideally, in a different color). What do you notice? 
  
  # Plotting y velocity
  #ggplot(trial) + 
   # geom_line(aes(t,y)) +
   # geom_line(aes(t,velocity_y), col='green')
  
  
  
  
  
  
  # ---- 7: Filtering y velocity ---- 
  # Using the library 'signal' and the function 'filtfilt', apply a Butterworth filter to y velocity. 
  # Plot the filtered y velocity on top of the unfiltered one (and together with y) and look at it. 
  # Play around with the filter settings and observe what it does to the data. 
  # Find a setting that seems appropriate for the given data. 
  
  # Filter settings
  filter_cutoff <- .3
  filter_order <-  1
  bf <- butter(filter_order, filter_cutoff, type ='low') 
  
  # Applying filter to y velocity in a new column
  trial$velocity_fil <- filtfilt(bf, trial$velocity_y)
  
  # Plotting filtered y velocity
  #ggplot(trial, aes(t,y)) + 
   # geom_line(aes(t,velocity_y), col = "green") + 
    #geom_line(aes(t,velocity_fil), col = "blue")# +
    #xlab("Velocity plot of participant 1") + ylab("Black: movement. Green: Velocity. Blue: Velocity filtered")
  
  
  
  
  
  # ---- Bonus: y acceleration ---- 
  # Using y velocity as the starting point, repeat steps 5 - 7 with acceleration (i.e. the change in velocity).
  # Look at the resulting relation of y, y velocity and y acceleration.
  
  # Calculating y acceleration 
  trial$acc_y <- c(0, diff(trial$velocity_fil) / diff(trial$t))
  
  # Applying filter to y acceleration
  trial$acc_fil <- filtfilt(bf, trial$acc_y)
  
  # Plotting filtered y acceleration
  #ggplot(trial) + 
   # geom_line(aes(t,y)) +
    #geom_line(aes(t,acc_y), col='green') +
    #geom_line(aes(t,acc_fil), col='blue') +
    #geom_line(aes(t,velocity_fil), col='red')
  

  
  # ---- 8: Zero crossings ---- 
  # Make another column that finds the places where the y velocity crosses 0. One way to do it is:
  # Make a column that determines whether the value of y velocity is positive or negative. You can use 
  # the function sign for that. Now make a column calculate that subtracts these values. You should have a 
  # vector that is either 2, 0 or -2. Using this vector, create two new columns that code the type of transition: 
  # One codes minima and is TRUE whenever the transition is from negative to positive (so value 2); the other codes
  # maxima and is TRUE whenever the transition is from positive to negative (so value -2).
  
  # Calculate zero crossings
  
  # New column determining making local max +2 and local min -2.
  trial <- trial %>% mutate(zero_cross = sign(velocity_fil)) %>% 
    mutate(zero_cross = zero_cross - lead(zero_cross))
  
  # make two columns specifying local max and min.
  trial$max <- ifelse(trial$zero_cross == 2, T, F)
  trial$min <- ifelse(trial$zero_cross == -2, T, F)
  
  trial$max <- ifelse(trial$max == T, trial$y, NA)
  trial$min <- ifelse(trial$min == T, trial$y, NA)
  
  trial$max <- as.numeric(trial$max)
  trial$min <- as.numeric(trial$min)
  
  
  # ---- 9: Plot zero crossings ---- 
  # Plot the minima and maxima on top of a x versus y graph. You might want to add the filtered velocity as well
  # to confirm what the zero crossings do. 
  
  # Plot zero crossings - (t,y)
  #ggplot(trial) + 
   # geom_line(aes(t,y), col = "red") +
    #geom_line(aes(t,velocity_fil), col='green') + 
    #geom_point(aes(t, max), col = 'blue') + 
    #geom_point(aes(t, min), col = 'blue') + xlab("Time")
  # The local min and max are when velocity is zero.
  
  
  # Plot zero crossings - (x,y)
  #ggplot(trial) + 
   # geom_line(aes(x,y), col = "red") +
  #  geom_line(aes(x,velocity_fil), col='green') + 
  #  geom_point(aes(x, max), col = 'blue') + 
  #  geom_point(aes(x, min), col = 'blue') + xlab("Time")
  
  
  
  
  
  # ---- 10: Target height ---- 
  # Now let's find the height of that particular movement that we are interested in (i.e. the target). 
  # Do this based on x position, i.e. create choose reasonable values on the x axis (I recommend between
  # 1.2 and 1.4). Those zero crossings that are within that range related to the target, the other ones 
  # to the distractor. 
  # You might see two issues arising: 
  # 1) The algorithm might sometimes pick out a value that is not the peak in the trajectory. A simple solution
  # is to add a criterion about a minimal height, so a value on the y axis (e.g. .1), so that only stops in
  # the movement above this value are counted. 
  # 2) In a few trials, no zero crossings for the target are found because participants might not have 
  # performed the trial properly. Do feel free to exclude such trials from further analysis when you run 
  # a loop with all trials, e.g. using the function 'any' to determine whether a target zero crossing is available. 
  
  # Setting constants for x and y boundaries of the target area
  
  
  # Distinguishing targets from distractors
  
  trial$target_height <- ifelse(trial$x >= 1.2 & trial$x <= 1.4 & trial$y > .3 & trial$zero_cross == 2, trial$y, NA)
  
  
  
  # ---- 11: Plot zero crossings ---- 
  # Make a plot of x versus y and plot the target and the distractor zero crossings as circles 
  # in different colors. (See demo plot on Blackboard.)
  
  #ggplot(trial) + geom_line(aes(x,y)) + 
  #  geom_point(aes(x, min), col = 'green')+
  #  geom_point(aes(x, max), col = 'blue') +
  #  geom_point(aes(x,target_height), col = 'red') 
  
  
  # ---- 12: Store derived parameters ---- 
  # Create a data frame that stores information about the current participant and trial, background information 
  # (mouse, gender, age) and target height. If you want, you can include further parameters, e.g. mean distractor
  # height, trial duration, distances travelled, max target velocity etc., but that's not a requirement. 
  
  
  dur <- trial$t[trial$t == max(trial$t)]
  height <- trial$target_height[!is.na(trial$target_height)]
  
  info <- tibble(
    ID = trial$id[1], 
    Gender = trial$Gender[1], 
    Age = trial$Age[1], 
    Mouse = trial$Mouse[1], 
    Trial = trial$trial[1],
    Trial_duration = as.numeric(dur), 
    Target_height = as.numeric(height),
    Distract = trial$distr[1], 
    Target = trial$targ[1]
  )
  
  
  
  #Combine with premade empty dataframe
  if (nrow(data) == 0) {
    data <- info}
  else {
    data <- rbind(data, info)}
  
  
  # correct counters
  if (crt_trial == 40){
    crt_id <- crt_id + 1
  } & {
    crt_trial <- 1
  } else {
    crt_trial <- crt_trial + 1
  }
  
  
  
  
  
}



# ---- 14: Apply linear model ---- 
# If you like, do a statistical analysis of the data, which - after the preprocessing where we derived individual
# parameters - can now be treated like any other data set you've encountered before. 

m <- lmer(data = data, Target_height ~ Target + (1 + Target|ID))
summary(m)

m2 <- lmer(data = data, Target_height ~ Target * Distract + (1 + Target|ID))
summary(m2)




#__________________________________________________________________________________________#

# Data
data_all <- read_csv('data_all.csv')

# Make empty data frame to be filled
data <- tibble(
  ID = factor(), 
  Gender = factor(), 
  Age = factor(), 
  Mouse = factor(), 
  Trial = factor(), 
  Trial_duration = numeric(), 
  Target_height = numeric(),
  Distract = factor(), 
  Target = factor()
)

# loop
for (crt_id in 1:length(unique(data_all$id))){
  
  ID <- dplyr::filter(data_all, id == crt_id)
  
  
  for (crt_trial in 1:length(unique(ID$trial))){
    
    # ---- 3: First plotting ---- 
    # subset for one trial
    trial <- dplyr::filter(ID, trial == crt_trial)
    
    # Plotting 
    #ggplot(trial) + geom_line(aes(x,y))
    
    
    
    
    
    # ---- 4: Recoding into a more intuitive reference frame ---- 
    # Modify x and y in a way that the fixed starting point in each trial is located at 0/0. Plot the trial again to verify it worked.
    
    # Recoding to new zero
    x_offset = .9
    y_offset = .9
    
    trial <- trial %>% mutate(
      x = x + x_offset,
      y = y + y_offset
    )
    
    # Plotting 
    #ggplot(trial) + geom_line(aes(x,y))
    
    
    
    
    
    # ---- 5: y velocity ---- 
    # Make a new column and calculate y velocity. Remember, velocity is the difference in position (here: y) divided by the difference in time.
    # You will most likely get an error message. Discuss in the group why and what could be done about it. (Think pragmatically!) 
    
    # Make velocity column
    trial <- trial %>% mutate(
      velocity_y = (y - lag(y)) / (t - lag(t))
    )
    
    
    
    
    # ---- 6: Plotting y velocity ---- 
    # Make a plot with t against y and overlay with t against y_vel (ideally, in a different color). What do you notice? 
    
    # Plotting y velocity
    #ggplot(trial) + 
    # geom_line(aes(t,y)) +
    # geom_line(aes(t,velocity_y), col='green')
    
    
    
    
    
    
    # ---- 7: Filtering y velocity ---- 
    # Using the library 'signal' and the function 'filtfilt', apply a Butterworth filter to y velocity. 
    # Plot the filtered y velocity on top of the unfiltered one (and together with y) and look at it. 
    # Play around with the filter settings and observe what it does to the data. 
    # Find a setting that seems appropriate for the given data. 
    
    # Filter settings
    filter_cutoff <- .3
    filter_order <-  1
    bf <- butter(filter_order, filter_cutoff, type ='low') 
    
    # Applying filter to y velocity in a new column
    trial$velocity_fil <- filtfilt(bf, trial$velocity_y)
    
    # Plotting filtered y velocity
    #ggplot(trial, aes(t,y)) + 
    # geom_line(aes(t,velocity_y), col = "green") + 
    #geom_line(aes(t,velocity_fil), col = "blue")# +
    #xlab("Velocity plot of participant 1") + ylab("Black: movement. Green: Velocity. Blue: Velocity filtered")
    
    
    
    
    
    # ---- Bonus: y acceleration ---- 
    # Using y velocity as the starting point, repeat steps 5 - 7 with acceleration (i.e. the change in velocity).
    # Look at the resulting relation of y, y velocity and y acceleration.
    
    # Calculating y acceleration 
    trial$acc_y <- c(0, diff(trial$velocity_fil) / diff(trial$t))
    
    # Applying filter to y acceleration
    trial$acc_fil <- filtfilt(bf, trial$acc_y)
    
    # Plotting filtered y acceleration
    #ggplot(trial) + 
    # geom_line(aes(t,y)) +
    #geom_line(aes(t,acc_y), col='green') +
    #geom_line(aes(t,acc_fil), col='blue') +
    #geom_line(aes(t,velocity_fil), col='red')
    
    
    
    # ---- 8: Zero crossings ---- 
    # Make another column that finds the places where the y velocity crosses 0. One way to do it is:
    # Make a column that determines whether the value of y velocity is positive or negative. You can use 
    # the function sign for that. Now make a column calculate that subtracts these values. You should have a 
    # vector that is either 2, 0 or -2. Using this vector, create two new columns that code the type of transition: 
    # One codes minima and is TRUE whenever the transition is from negative to positive (so value 2); the other codes
    # maxima and is TRUE whenever the transition is from positive to negative (so value -2).
    
    # Calculate zero crossings
    
    # New column determining making local max +2 and local min -2.
    trial <- trial %>% mutate(zero_cross = sign(velocity_fil)) %>% 
      mutate(zero_cross = zero_cross - lead(zero_cross))
    
    # make two columns specifying local max and min.
    trial$max <- ifelse(trial$zero_cross == 2, T, F)
    trial$min <- ifelse(trial$zero_cross == -2, T, F)
    
    trial$max <- ifelse(trial$max == T, trial$y, NA)
    trial$min <- ifelse(trial$min == T, trial$y, NA)
    
    trial$max <- as.numeric(trial$max)
    trial$min <- as.numeric(trial$min)
    
    
    # ---- 9: Plot zero crossings ---- 
    # Plot the minima and maxima on top of a x versus y graph. You might want to add the filtered velocity as well
    # to confirm what the zero crossings do. 
    
    # Plot zero crossings - (t,y)
    #ggplot(trial) + 
    # geom_line(aes(t,y), col = "red") +
    #geom_line(aes(t,velocity_fil), col='green') + 
    #geom_point(aes(t, max), col = 'blue') + 
    #geom_point(aes(t, min), col = 'blue') + xlab("Time")
    # The local min and max are when velocity is zero.
    
    
    # Plot zero crossings - (x,y)
    #ggplot(trial) + 
    # geom_line(aes(x,y), col = "red") +
    #  geom_line(aes(x,velocity_fil), col='green') + 
    #  geom_point(aes(x, max), col = 'blue') + 
    #  geom_point(aes(x, min), col = 'blue') + xlab("Time")
    
    
    
    
    
    # ---- 10: Target height ---- 
    # Now let's find the height of that particular movement that we are interested in (i.e. the target). 
    # Do this based on x position, i.e. create choose reasonable values on the x axis (I recommend between
    # 1.2 and 1.4). Those zero crossings that are within that range related to the target, the other ones 
    # to the distractor. 
    # You might see two issues arising: 
    # 1) The algorithm might sometimes pick out a value that is not the peak in the trajectory. A simple solution
    # is to add a criterion about a minimal height, so a value on the y axis (e.g. .1), so that only stops in
    # the movement above this value are counted. 
    # 2) In a few trials, no zero crossings for the target are found because participants might not have 
    # performed the trial properly. Do feel free to exclude such trials from further analysis when you run 
    # a loop with all trials, e.g. using the function 'any' to determine whether a target zero crossing is available. 
    
    # Setting constants for x and y boundaries of the target area
    
    
    # Distinguishing targets from distractors
    
    trial$target_height <- ifelse(trial$x >= 1.2 & trial$x <= 1.4 & trial$y > .3 & trial$zero_cross == 2, trial$y, NA)
    
    
    
    # ---- 11: Plot zero crossings ---- 
    # Make a plot of x versus y and plot the target and the distractor zero crossings as circles 
    # in different colors. (See demo plot on Blackboard.)
    
    #ggplot(trial) + geom_line(aes(x,y)) + 
    #  geom_point(aes(x, min), col = 'green')+
    #  geom_point(aes(x, max), col = 'blue') +
    #  geom_point(aes(x,target_height), col = 'red') 
    
    
    # ---- 12: Store derived parameters ---- 
    # Create a data frame that stores information about the current participant and trial, background information 
    # (mouse, gender, age) and target height. If you want, you can include further parameters, e.g. mean distractor
    # height, trial duration, distances travelled, max target velocity etc., but that's not a requirement. 
    
    
    dur <- trial$t[trial$t == max(trial$t)]
    height <- trial$target_height[!is.na(trial$target_height)]
    
    info <- tibble(
      ID = trial$id[1], 
      Gender = trial$Gender[1], 
      Age = trial$Age[1], 
      Mouse = trial$Mouse[1], 
      Trial = trial$trial[1],
      Trial_duration = as.numeric(dur), 
      Target_height = as.numeric(height),
      Distract = trial$distr[1], 
      Target = trial$targ[1]
    )
    
    
    
    #Combine with premade empty dataframe
    if (nrow(data) == 0) {
      data <- info}
    else {
      data <- rbind(data, info)}
    
    
    # correct counters
    if (crt_trial == 40){
      crt_id <- crt_id + 1
    } & {
      crt_trial <- 1
    } else {
      crt_trial <- crt_trial + 1
    }
    
    
    
    
    
  }
  

}


#__________________________________________________________________________________________#


