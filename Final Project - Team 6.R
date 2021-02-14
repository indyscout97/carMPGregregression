# ------------------------ #
# --- Loading Packages --- #
# ------------------------ #

library(data.table)
library(ggplot2)
library(car)
library(olsrr)
library(lattice)
library(caret)
library(e1071)
library(plotrix)


# --------------------- #
# --- Data cleaning --- #
# --------------------- #

# Reading in data
car <- read.table("/users/Indyscout/Downloads/auto-mpg.data" ,  header = FALSE)

# Setting as data.table.
carMPG <- as.data.table(car)

# Naming the Columns
names(carMPG)[1] <- 'MPG'
names(carMPG)[2] <- 'Cylinders'
names(carMPG)[3] <- 'Displacement'
names(carMPG)[4] <- 'Horsepower'
names(carMPG)[5] <- 'Weight'
names(carMPG)[6] <- 'Acceleration'
names(carMPG)[7] <- 'Model_Year'
names(carMPG)[8] <- 'Origin'  #This is where the manufacturer is from; 1 = North America , 2 = Europe , 3 = Asia
names(carMPG)[9] <- 'Car Name'

# Removing '?' from Horsepower vector
carMPG$Horsepower <- gsub( '\\?' , NA, carMPG$Horsepower)

#setting horsepower as numeric
carMPG$Horsepower <- as.numeric(carMPG$Horsepower)


# --------------------- #
# --- Initial plots --- #
# --------------------- #

std.error(carMPG)

# MPG x Horsepower
ggplot(data = carMPG , aes(x = MPG , y = Horsepower)) +
  geom_point(color = "Purple")+
  geom_smooth(method="lm", color = "Red")+
  xlab("MPG")+
  ylab("Horsepower")+
  theme_bw()

# MPG x Weight
ggplot(data = carMPG , aes(x = MPG , y = Weight)) +
  geom_point(color = "Blue")+
  geom_smooth(method="lm", color = "Red")+
  xlab("MPG")+
  ylab("Weight")+
  theme_bw()

# MPG x Cylinders (as factor)
ggplot(data = carMPG , aes(x = MPG , y = as.factor(Cylinders))) +
  geom_boxplot( outlier.color = "Red")+
  xlab("MPG")+
  ylab("Cylinders")+
  theme_bw()

# MPG x Model_Year
ggplot(data = carMPG , aes(x = MPG , y = as.factor(Model_Year))) +
  geom_boxplot(color = 'aquamarine2')+
  xlab("MPG")+
  ylab("Model_Year")+
  theme_dark()

# Good one here. MPG x Horsepower x Weight
ggplot(data = carMPG , aes(x = MPG , y = Horsepower, color = Weight)) +    ### Has more of a curve to it than a linear relationship
  geom_point()+                                                            ### Might be indicative of a log normal distribution, will do additional testing                                          
  geom_smooth(method="lm" ,  color = "red")+
  xlab("MPG")+
  ylab("Horsepower")+
  theme_bw()


# Visually checking the distribution
hist(carMPG$Horsepower) # Output shows a significant positive skew, confirming the data is closer to a log normal distribution than a normal distribution
hist(carMPG$Weight)

# We will do appropriate log transformations on the variables later, but we will start by making some naive models to get a better understanding of our data


# --------------------------------- #
# --- Initial regression models --- #
# --------------------------------- #

# Start by predicting MPG using Horsepower and other variables that have not been transformed, individually

mod1 <- lm(MPG ~ Horsepower, data = carMPG)
mod1.1 <- lm(MPG ~ Weight, data = carMPG)
mod2 <- lm(MPG ~ factor(Cylinders) , data = carMPG)
mod3 <- lm(MPG ~ Acceleration, data = carMPG)
 

# Lets make a model with a few of the features
mod4 <- lm(MPG ~ Horsepower + Weight + Displacement + Acceleration , data = carMPG)

# Notes on mod4: Displacement and Acceleration have high P-Value, lets remove them
mod4.1 <- lm(MPG ~ Horsepower + Weight , data = carMPG) # This model looks a little better
test4 <- lm(MPG ~ Horsepower * Weight , data = carMPG) # Testing for differences between the models if an interaction term is added


# Based on our prior knowledge of cars and our outputs, we had reason to suspect multicolinearity between Horsepower and Weight

# Test models for multicollinearity
vif(mod4) # Displacement and Weight have VIF > 10, indicating the variables are highly correlated 
          # Based on this and high P-value, it is likely best to remove them from future models
vif(mod4.1) # Looks much better


# Horsepower and model year  
mod5 <- lm(MPG ~ factor(Model_Year)+Horsepower, data=carMPG) # Model year stats seem inconsistent

# Just model year, likely not significant 
test5 <- lm(MPG ~ factor(Model_Year), data=carMPG) # Model year alone produces low Adj R of .41

# Do manufacturer locations matter?
mod6 <- lm(MPG ~ factor(Origin) , data = carMPG) # Adj R is too low, 0.3295


### Mod4.1 seems to fit the best out of the above models. Let's write out an equation to test it
coef(mod4.1)[1] + (coef(mod4.1)[2] * 100) + (coef(mod4.1)[3] * 2800) # 24.68 MPG!!!


# -------------------------------------------------------- #
# --- Making graphs and models based off Log(variable) --- #
# -------------------------------------------------------- #

# Performing log transformations on the variables
carMPG$logMPG <- log(carMPG$MPG)
carMPG$logHorsepower <- log(carMPG$Horsepower)
carMPG$logWeight <- log(carMPG$Weight)
carMPG$logAcceleration <- log(carMPG$Acceleration)
carMPG$logDisplacement <- log(carMPG$Displacement)


# Scatter Plot of logMPG, logHorsepower, logWeight (as color)
logPlot <- ggplot(data = carMPG , aes(x = logMPG , y = logHorsepower, color = logWeight)) +
            geom_point()+
            geom_smooth(method="lm", color = "steelblue2" , se = FALSE)+
            xlab("LogMPG")+
            ylab("LogHorsepower")+
            theme_gray()

# There seems to be a strong negative linear trend between the features, as expected. As weight and horsepower decrease, mpg tends to increase
logPlot + scale_color_gradient(low = "orangered1", high = "purple") # Use this to display graph with different color gradient


# Scatter Plot of logMPG, logHorsepower, logWeight (as color), Cylinders (as shape)

logPlot2 <- ggplot(data = carMPG , aes(x = logMPG , y = logHorsepower, color = logWeight, shape = as.factor(Cylinders))) +
  geom_point(size = 1.75)+
  xlab("LogMPG")+
  ylab("LogHorsepower")+
  theme_gray()

logPlot2 + scale_color_gradient(low = "orange", high = "blue") # Same graph but with cylinders added in the form of shape, 
                                                               # lower cylinder counts tend to have lower MPG, 4 cylinders in the sweet spot


# Log Models; MPG ~ Horsepower, Weight, and a combination of the two
logmod1 <- lm(logMPG ~ logHorsepower, data = carMPG)
logmod2 <- lm(logMPG ~ logWeight, data = carMPG)
logmod3 <- lm(logMPG ~ logHorsepower + logWeight, data = carMPG)


# Test model for multicollinearity between logHorsepower and logWeight
vif(logmod3) # VIF = 4.23, indicating moderate correlation, vs. the high correlation between the untransformed variables

#Log Models; MPG ~ Displacement, Acceleration, and a combination of the two
logmod4 <- lm(logMPG ~ logDisplacement, data = carMPG) # Might be something here
logmod5 <- lm(logMPG ~ logAcceleration, data = carMPG) # This model did not provide useful information to us 

#Log Models; MPG ~ Horsepower, Weight, and Displacement
logmod6 <- lm(logMPG ~ logHorsepower + logWeight + logDisplacement, data = carMPG) # Displacement has very high P-Value

logmod7 <- lm(logMPG ~ logHorsepower + logWeight + Cylinders, data = carMPG) # Creating this model to test collinearity down below, 
                                                                             # as testing it with cylinders as categorical variable produces strange VIF results


# ------------------- #
# --- Final Model --- #
# ------------------- #

logmod8<- lm(logMPG ~ logHorsepower + logWeight + as.factor(Cylinders), data = carMPG) # Adj R is higher than logmod6, this might be one of the better models

# Test models for multicollinearity
ols_vif_tol(logmod7) # This shows that high VIF in logmod 8 is the result of breaking the cylinder variable into different categories 

ols_vif_tol(logmod8) # High collinearity on the cylinders, but wouldn't each cylinder variable have high collinearity given that it is one variable as 5 different factors?


## Plugging numbers into the model to test it: ##

# Testing logmod8; 100 HP car, 2800lbs, with 4 cylinder
c_test1 <- coef(logmod8)[1] + (coef(logmod8)[2]* log(100)) + (coef(logmod8)[3] * log(2800)) + (coef(logmod8)[4] * 1) + (coef(logmod8)[5] * 0) + (coef(logmod8)[6] * 0) + (coef(logmod8)[7] * 0)

exp(c_test1) # 23.747 MPG !!!

# Looking at Logplot2, we've selected some log weights and HP that will produce a good MPG.

exp(7.75) # weight ~2300
exp(4.75) # HP ~115

# Lets try those values in the the model ; 100 HP car, 2300lbs, with 4 cylinder
c_test2 <- coef(logmod8)[1] + (coef(logmod8)[2]* log(100)) + (coef(logmod8)[3] * log(2300)) + (coef(logmod8)[4] * 1) + (coef(logmod8)[5] * 0) + (coef(logmod8)[6] * 0) + (coef(logmod8)[7] * 0)

exp(c_test2) # 26.366 

### IDEAL CAR COULD BE 75-150 HP, 2000-2500 LBS, with a 4 cylinder motor ###

# Based off our plots, raising cylinder count to 6 should decrease efficiency, time to run same equations but with 6 cylinder
c_test3 <- coef(logmod8)[1] + (coef(logmod8)[2]* log(100)) + (coef(logmod8)[3] * log(2800)) + (coef(logmod8)[4] * 0) + (coef(logmod8)[5] * 0) + (coef(logmod8)[6] * 1) + (coef(logmod8)[7] * 0)

exp(c_test3) # 21.102 - decreased with cylinders

c_test4 <- coef(logmod8)[1] + (coef(logmod8)[2]* log(100)) + (coef(logmod8)[3] * log(2300)) + (coef(logmod8)[4] * 0) + (coef(logmod8)[5] * 0) + (coef(logmod8)[6] * 1) + (coef(logmod8)[7] * 0)

exp(c_test4) # 23.428 MPG, however weight is 2300 lbs which is unlikely for a 6 cylinder, these will be closer to 3000 lbs

#  Well if less cylinders is so good, what about a 3-cylinder?

c_test5 <- coef(logmod8)[1] + (coef(logmod8)[2]* log(100)) + (coef(logmod8)[3] * log(2300)) + (coef(logmod8)[4] * 0) + (coef(logmod8)[5] * 0) + (coef(logmod8)[6] * 0) + (coef(logmod8)[7] * 0)

exp(c_test5) # MPG is lower, based on our data, 3 cylinders is not as fuel efficient as we expected. 


# What cars might fit out specifications based on the model?

suitable_cars <- carMPG[carMPG$Horsepower >= 75 & carMPG$Horsepower <= 150 & carMPG$Weight >= 2000 & carMPG$Weight <= 2500 & carMPG$Cylinders == 4, c(1,2,4,5,7,9)]
suitable_cars

hist(carMPG$logHorsepower)
hist(carMPG$logWeight)

