#------------------------------------------------------------------------------------------------#
#---------- Creating a model to predict rain in Australia using weatherAUS.csv dataset ----------#
#---------- that can be finded in https://www.kaggle.com/jsphyg/weather-dataset-rattle-package --#
#------------------------------------------------------------------------------------------------#

# uncommit if you already have not these packages installed

#install.packages('sampling',dependencies = T)
#install.packages('randomForest',dependencies = T)
#install.packages('ggplot2',dependencies = T)
#install.packages('ggpubr',dependencies = T)
#install.packages('dplyr',dependencies = T)
#install.packages('caret',dependencies = T)
#install.packages('devtools')
#install.packages('zoo') #to function travel.R
#install.packages('RColorBrewer') #to function allvar_plots.R

# Importing librarys
library(sampling) # stratifyed sample
library(randomForest) # randomforest model
library(ggplot2) # visualization
library(ggpubr) # visualization
library(dplyr) # data selection
library(caret) # data partition and results
library(devtools) # source of function on  github

# Changing directory to import dataset
setwd("D:/MY OWN R CODES/MODELS")

# Importing weatherAUS.csv from our directory
rain <- read.csv('weatherAUS.csv',header = T,sep = ',')

# First let's remove Risk_MM atribute as was proposed in kanggle page:
# "Note: You should exclude the variable Risk-MM when training a binary 
#  classification model. Not excluding it will leak the answers to your
#  model and reduce its predictability."

rain <- select(rain,-c('RISK_MM')) #removing Risk-MM atribute
names(rain)[23] <- 'class' #naming our class

#--------------------- Exploratory analyses ---------------

dim(rain) # Verifying dimentions

head(rain) # seeing our first 5 rows

summary(rain) # seeing dataset summary

str(rain) # analysing dataset structure

# As we can see, we have many missing values in almost all atributes
# let's analyse whats percentage this missing values represent in each atribute

high_percents <- 0.1 # difyning a cut point for percentages

percents <- data.frame(0) # creating a dataframe to storage percentages
names(percents) <- 'Percentage' #naming dataframe column

# Creaing a loop to take each percentage that exceed high_percents
# and storage in our dataframe 'percents'
for (i in names(rain)) {
  
  if((sum(is.na(rain[i]))/dim(rain)[1])>high_percents){
    
    percents[i,] <- round((sum(is.na(rain[i]))/dim(rain)[1]),2)
  }
}

# Converting indexes into a new columns with atributes names
percents['Atributes'] <- row.names(percents)
row.names(percents) <- NULL

percents <- percents[2:dim(percents)[1],] #removing first row without usefull information

# Ploting a bar graphic with high missing values percentage atributes
ggplot(percents, aes(x = Atributes, y = Percentage)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = Percentage), vjust = -0.3) +
  ggtitle('Atributes with high missing values percentage')+
  theme_pubclean()

# As we see some of our atributes have more than 40% of missing values, almost
# half of our dataset, based on this information let's remove the columns:
# Cloud3pm, Cloud9am, Evaporation and Sunshine that have high missing values 
# percentage, let's remove Location column too as we are predicting rain in all Australia

new_rain <- select(rain,-c('Location','Cloud3pm','Cloud9am','Evaporation','Sunshine')) #dropping columns

head(new_rain)

# Now we also have missing values, let's replacing then by the previews value

# Changing directory to call function travel.R that can be finded in:
# https://github.com/wellingtsilvdev/codes-with-real-utilities/commit/2ac247fd0899d264148022b7ee1ecffcddaebaaa

source_url('https://github.com/wellingtsilvdev/codes-with-real-utilities/blob/master/travel.R?raw=TRUE') # calling travel function

new_rain <- travel(new_rain,element = 0,last.na = T,first_row.na = T) # replacing missing values

summary(new_rain) # verifying if missing values are replaced

head(new_rain)

# we clearly have here a ambalance class problem, let's create the 
# a stratifyed sample to solve the ambalance class problem

#------------- Creating a randomforest model ----------------#

#--------- Spliting dataset into train and test -------------#

summary(new_rain$class)

# We have here a ambalance class problem, to solve this, let's create a stratifyed sample
# to solve this problem with 30000 from class yes and 30000 from class no

# spliting our data set into train and test
set.seed(42)
sampl <- strata(new_rain,'class',c(30000,30000),'srswr')

samplee <- sample(2,60000,replace = T,prob = c(0.5,0.5))

sampl_train <- sampl[samplee==1,]
sampl_test <- sampl[samplee==2,]

training <- new_rain[sampl_train$ID_unit,]
testing <- new_rain[sampl_test$ID_unit,]

summary(training)
str(training)

# creating forest1
set.seed(42)
forest1 <- randomForest(class ~.,training[,2:dim(training)[2]],ntree=100,importance=T)

#------- Making the predicion on testing dataset-------#
pred <- predict(forest1,testing)
pred <- as.factor(pred)

# Importing draw_confusion_matrix function that can be finded:
# https://github.com/wellingtsilvdev/codes-with-real-utilities/commit/1e7cc00ce21b2edd29922de868189bf9779a5b57
# to plot our results

source_url('https://github.com/wellingtsilvdev/codes-with-real-utilities/blob/master/draw_confusion_matrix.R?raw=TRUE') # import the function from my github repository

# Evaluating results using ConfusionMatrix function
confusion_forest1 <- confusionMatrix(testing$class,pred)
draw_confusion_matrix(confusion_forest1)

# We have 83% of accuracy, good levels of Sensitivity and Specificity,
# let's try to improve our accuracy treating and creating some variables

# Exploring variables distributions

# importing allvar_plots.R function that can be finded in:
# https://github.com/wellingtsilvdev/codes-with-real-utilities/commit/5adaf9342aa9830c5b5285d7e5b1564355dfd03f

source('https://github.com/wellingtsilvdev/codes-with-real-utilities/blob/master/allvar_plots.R?raw=TRUE')
allvar_plots(new_rain,'plot') # line plot to numerical and bar plot to categoricals

allvar_plots(new_rain,'hist') # histograms only for numericals

# creating new variables high, low and median

new_rain2 <- new_rain

new_rain2['mintemp_high'] <- ifelse(new_rain2$MinTemp>=25,1,0)
new_rain2['mintemp_low'] <- ifelse(new_rain2$MinTemp<= 0,1,0)
new_rain2['mintemp_median'] <- ifelse(new_rain2$MinTemp<25 & new_rain2$MinTemp> -10,1,0)

new_rain2['maxtemp_high'] <- ifelse(new_rain2$MaxTemp>=40,1,0)
new_rain2['maxtemp_low'] <- ifelse(new_rain2$MaxTemp<= 10,1,0)
new_rain2['maxtemp_median'] <- ifelse(new_rain2$MaxTemp<40 & new_rain2$MaxTemp> 10,1,0)

new_rain2['rainfall_high'] <- ifelse(new_rain2$Rainfall>=50,1,0)
new_rain2['rainfall_low'] <- ifelse(new_rain2$Rainfall<= 20,1,0)
new_rain2['rainfall_median'] <- ifelse(new_rain2$Rainfall<50 & new_rain2$Rainfall> 20,1,0)

new_rain2['windgustspeed_high'] <- ifelse(new_rain2$WindGustSpeed>=90,1,0)
new_rain2['windgustspeed_low'] <- ifelse(new_rain2$WindGustSpeed<= 10,1,0)
new_rain2['windgustspeed_median'] <- ifelse(new_rain2$WindGustSpeed<90 & new_rain2$WindGustSpeed> 10,1,0)

new_rain2$maxtemp_high <- as.factor(new_rain2$mintemp_high)
new_rain2$maxtemp_low <- as.factor(new_rain2$mintemp_low)
new_rain2$maxtemp_median <- as.factor(new_rain2$mintemp_median)

new_rain2$mintemp_high <- as.factor(new_rain2$mintemp_high)
new_rain2$mintemp_low <- as.factor(new_rain2$mintemp_low)
new_rain2$mintemp_median <- as.factor(new_rain2$mintemp_median)

new_rain2$rainfall_high <- as.factor(new_rain2$rainfall_high)
new_rain2$rainfall_low <- as.factor(new_rain2$rainfall_low)
new_rain2$rainfall_median <- as.factor(new_rain2$rainfall_median)

new_rain2$windgustspeed_high <- as.factor(new_rain2$windgustspeed_high)
new_rain2$windgustspeed_low <- as.factor(new_rain2$windgustspeed_low)
new_rain2$windgustspeed_median <- as.factor(new_rain2$windgustspeed_median)

new_rain2$WindSpeed9am <- ifelse(new_rain2$WindSpeed9am>30,30,new_rain2$WindSpeed9am)

new_rain2$WindSpeed3pm <- ifelse(new_rain2$WindSpeed3pm>40,40,new_rain2$WindSpeed3pm)

new_rain2$Humidity9am <- ifelse(new_rain2$Humidity9am<30,30,new_rain2$Humidity9am)

new_rain2$var1 <- new_rain2$MaxTemp/new_rain2$MinTemp
new_rain2$var2 <- new_rain2$WindSpeed9am/new_rain2$WindSpeed3pm
new_rain2$var3 <- new_rain2$Humidity9am/new_rain2$Humidity3pm
new_rain2$var4 <- new_rain2$Pressure9am/new_rain2$Pressure3pm
new_rain2$var5 <- new_rain2$Temp9am/new_rain2$Temp3pm
new_rain2$var6 <- new_rain2$WindGustSpeed/new_rain2$Pressure9am
new_rain2$var7 <- new_rain2$WindSpeed9am/new_rain2$Pressure3pm

# removing original atributes that are used to create the new variables
new_rain2 <- select(new_rain2,-c('MinTemp','MaxTemp','Rainfall'
                                 ,'WindGustSpeed','WindSpeed9am'
                                 ,'WindSpeed3pm','Humidity9am'
                                 ,'Humidity3pm','Pressure9am'
                                 ,'Pressure3pm'))

# removing missing values
new_rain2 <- travel(new_rain2,0,last.na = T,first_row.na = T)

#------------- Creating the second randomforest model ----------------#

#--------- Spliting dataset into train and test -------------#

summary(new_rain2$class)

# We have here a ambalance class problem, to solve this, let's create a stratifyed sample
# to solve this problem with 30000 from class yes and 30000 from class no

# spliting our data set into train and test
set.seed(42)
sampl <- strata(new_rain2,'class',c(30000,30000),'srswr')

samplee <- sample(2,60000,replace = T,prob = c(0.5,0.5))

sampl_train <- sampl[samplee==1,]
sampl_test <- sampl[samplee==2,]

training <- new_rain2[sampl_train$ID_unit,]
testing <- new_rain2[sampl_test$ID_unit,]

summary(training)

# creating forest2
set.seed(42)
forest2 <- randomForest(class ~.,training[,2:dim(training)[2]],ntree=100,importance=T)

#------- Making the predicion on testing dataset-------#
pred <- predict(forest2,testing)
pred <- as.factor(pred)

# Evaluating results using ConfusionMatrix function
confusion_forest2 <- confusionMatrix(testing$class,pred)

draw_confusion_matrix(confusion_forest2)

# Conclusion: Forest1 was much more efficient having the same accuracy of Forest2 but with
# less processing

# using a radomforest we can predict with 83% of accuracy if will rain or not in australia tomorrow

