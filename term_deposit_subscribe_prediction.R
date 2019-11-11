# Creating a classification model to predict if the client will subscribe (yes/no) a term deposit
# using Bank Marketing Data Set from UCI Machine Learning Repository that can be finded: 
# http://archive.ics.uci.edu/ml/datasets/Bank+Marketing

#----------------------------------------------------------------------------------------#
#------------------------ Installing and charging Librarys ------------------------------#
#----------------------------------------------------------------------------------------#

# Uncommit if have not this packages installed yet

#install.packages('tidyr',dependencies = T)
#install.packages('purrr',dependencies = T)
#install.packages('ggplot2',dependencies = T)
#install.packages('caret',dependencies = T)
#install.packages('rpart',dependencies = T)
#install.packages('rpart.plot',dependencies = T)
#install.packages('randomForest',dependencies = T)
#install.packages('dplyr',dependencies = T)
#install.packages('sampling',dependencies = T)
#install.packages('devtools',dependencies = T)
#install.packages('ggpubr',dependencies = T)
#install.packages('tidyverse',dependencies = T)
#install.packages('pROC',dependencies = T)

library(tidyr) # visualization
library(purrr) # visualization
library(ggplot2) # visualization
library(caret) # visualization of results
library(rpart) # decision tree model
library(rpart.plot) # visualization
library(randomForest) # Random Forest model
library(dplyr) # atribute selection
library(sampling) # stratfied samples
library(devtools) # import functions from github
library(ggpubr) # visualization
library(tidyverse) # manipulation
library(pROC) # ROC curve and AUC

#----------------------------------------------------------------------------------------#
#--- Importing dataset, dooing simples analyses and creating the first model ------------#
#----------------------------------------------------------------------------------------#

# Importing the dataset
setwd("D:/MY OWN R CODES/MODELS") #change to your directory
bank <- read.csv('bank-full.csv',header = T,sep = ';')

head(bank) # seeing the first 5 rows

# removing duration atribute because: 
# this attribute highly affects the output target (e.g., if duration=0 then y='no'). 
# Yet, the duration is not known before a call is performed. Also, after the end of the
# call y is obviously known. Thus, this input should only be included for benchmark purposes
# and should be discarded if the intention is to have a realistic predictive model.

bank <- select(bank,-c('duration')) # removing 'duration' atribute

head(bank) # seeing the first 5 rows

str(bank) # data structure

summary(bank) # verifying missing values

#------------ Calculating class proportion and ploting into graphic bars

countt <- data.frame(0) # creating a dataframe to storage classes frequency
names(countt) <- 'proportion' #naming dataframe column

# Creaing a loop to count yes and no categories
y = 0
n = 0
for(i in bank$y){
  if(i=="yes"){
    y=y+1
  }
  if(i=="no"){
    n=n+1
  }
}
countt[1,] <- n # storaging no class
countt[2,] <- y # storaging yes class

# converting indexes in a new column
row.names(countt) <- c('no','yes')
countt['classes'] <- row.names(countt)

row.names(countt) <- NULL # removing indexes

# Ploting a bar graphic with class proportion
ggplot(countt, aes(x = classes, y = proportion)) +
  geom_bar(fill = c('blue','red'), stat = "identity") +
  geom_text(aes(label=proportion), vjust = -0.3) +
  ggtitle('Class proportion')+
  theme_pubclean()

#---------------------------------------------------------------------------------------------------#
#--------------------- Creating the first model using Decision Tree Classification -----------------#
#---------------------------------------------------------------------------------------------------#

# We have here a ambalance class problem, to solve this, let's use Undersampling 
# method to create a stratifyed sample to solve this problem with 4500 from class yes and 4500 from class 

# spliting our data set into train and test
set.seed(244)
sampl <- strata(bank,'y',c(4500,4500),'srswr')

samplee <- sample(2,9000,replace = T,prob = c(0.5,0.5))

sampl_train <- sampl[samplee==1,]
sampl_test <- sampl[samplee==2,]

training <- bank[sampl_train$ID_unit,]
testing <- bank[sampl_test$ID_unit,]

summary(training$y)
summary(testing$y)

# creating the model1
tree1 <- rpart(y ~.,training,method = 'class')

# ploting the tree
rpart.plot(tree1,box.palette = 'RdBu'
           ,shadow.col = 'gray'
           ,nn=TRUE,main='Decision Tree - Model1')

#-----------realizing prediction on training database
pred <- predict(tree1,training)
pred <- as.data.frame(pred)

# converting percentagens predicted into the classes
training$pred <- ifelse(pred$yes>pred$no,'yes','no')
training$pred <- as.factor(training$pred)

# confusionmatrix and results
confusion <- confusionMatrix(training$y,training$pred)
confusion

accuracy <- confusion$overall[1]
accuracy

#------------ realizing prediction on testing database
pred <- predict(tree1,testing)
pred <- as.data.frame(pred)

# converting percentagens predicted into the classes
testing$pred <- ifelse(pred$yes>pred$no,'yes','no')
testing$pred <- as.factor(testing$pred)

# Importing draw_confusion_matrix.R function to plot confusion matrix results finded in:
# https://github.com/wellingtsilvdev/codes-with-real-utilities/commit/1e7cc00ce21b2edd29922de868189bf9779a5b57

source_url('https://github.com/wellingtsilvdev/codes-with-real-utilities/blob/master/draw_confusion_matrix.R?raw=TRUE') # calling travel function

# confusionmatrix and results
confusion1 <- confusionMatrix(testing$y,testing$pred)
draw_confusion_matrix(confusion1)

accuracy_tree1 <- confusion1$overall[1]
accuracy_tree1

# As we can see, the model are not capable to do a good prediction of yes class, 
# let's explore more our dataset to improve this accuracy treating the variables

#----------------------------------------------------------------------------------------#
#------------------------------------ Visualization -------------------------------------#
#----------------------------------------------------------------------------------------#

#----------- Ploting a the relation between age and all numerical atributes
bank %>%
  keep(is.numeric) %>%  # selecting numeric columns
  gather(-age, key = "var", value = "value") %>% # creating a dictionare with our variables and their values and drooping age atribute
  ggplot(aes(x = value, y = age, color = age)) +
  geom_point() +
  geom_smooth(method = "lm",col ='red', se = FALSE)+
  ggtitle('Relation between age and all numerical atributes')+
  facet_wrap(~ var, scales = "free") +
  theme_bw()

#------------ Ploting Categorical Atributes Frequency
bank %>%
  keep(is.factor) %>% # selecting categorical columns from bank dataset
  gather() %>% # creating a dictionare with our atributes and their categories
  ggplot(aes(value,fill = value)) +
  geom_bar()+
  ggtitle('Categorical Atributes Frequency')+
  facet_wrap(~ key, scales = "free") +
  theme_bw()

#----------- Boxploting numerical variables
bank %>%
  keep(is.numeric) %>%
  gather(key = "var", value = "value") %>%
  ggplot(aes(x=var, y=value)) + 
  geom_boxplot(aes(fill=var))+
  facet_wrap( ~ var, scales="free")+
  ggtitle('Numerical variables distribution')

#---------------------------------------------------------------------------------------------------#
#-------------------------------------- Treating Variables -----------------------------------------#
#---------------------------------------------------------------------------------------------------#

bank %>% keep(is.numeric) %>% summary() # seeing a summary of numerical variables

# logging our numerical variables
for(i in 1:dim(bank)[2]){
  if(is.numeric(bank[,i])==T){
    bank[,i] <- log(bank[,i])
  }
}

bank %>% keep(is.numeric) %>% summary() # verifying

# Importing travel.R function to remove and replace missing values finded in:
# https://github.com/wellingtsilvdev/codes-with-real-utilities/commit/2ac247fd0899d264148022b7ee1ecffcddaebaaa

source_url('https://github.com/wellingtsilvdev/codes-with-real-utilities/blob/master/travel.R?raw=TRUE') # calling travel function

bank <- travel(bank,element = 0,last.na = T,first_row.na = T) # replacing missing values

# Treating pdays:  number of days that passed by after the client was last contacted from a previous campaign
# -1 means that this client was never contacted before

summary(bank$pdays)

bank$pasdays <- ifelse(bank$pdays== 0,'f_contact',NA) # transforming pdays into categorical
bank$pasdays <- ifelse(bank$pdays!= 0 & bank$pdays <=2.433,'cont2',bank$pasdays)
bank$pasdays <- ifelse(bank$pdays!= 0 & bank$pdays >5.22,'cont3',bank$pasdays)
bank$pasdays <- as.factor(bank$pasdays)

# Treating campaign: number of contacts performed during this campaign and for this client (numeric, includes last contact)
summary(bank$campaign)

bank$campaign <- ifelse(bank$campaign>1.0986,1.0986,bank$campaign) # removing outlayers

# Treating age
summary(bank$age)

bank$age_cat <-  ifelse(bank$age>=3.871,'old',NA) # converting to categorical
bank$age_cat <-  ifelse(bank$age<=3.497,'young',bank$age_cat)
bank$age_cat <-  ifelse(bank$age>3.497 & bank$age<3.871,'mature',bank$age_cat)
bank$age_cat <- as.factor(bank$age_cat)

# Treating previous: previous: number of contacts performed before this campaign and for this client (numeric)
summary(bank$previous)

bank$cont_before <- ifelse(bank$previous==0,'no','yes')
bank$cont_before <- as.factor(bank$cont_before)

bank <- travel(bank,element = 0,last.na = T,first_row.na = T) # replacing missing values

#---------------------------------------------------------------------------------------------------#
#--------------------- Creating the second model using Decision Tree Classification -----------------#
#---------------------------------------------------------------------------------------------------#

# We have here a ambalance class problem, to solve this, let's use Undersampling 
# method to create a stratifyed sample to solve this problem with 4500 from class yes and 4500 from class 

# spliting our data set into train and test
set.seed(244)
sampl <- strata(bank,'y',c(4500,4500),'srswr')
  
samplee <- sample(2,9000,replace = T,prob = c(0.5,0.5))
  
sampl_train <- sampl[samplee==1,]
sampl_test <- sampl[samplee==2,]
  
training <- bank[sampl_train$ID_unit,]
testing <- bank[sampl_test$ID_unit,]
  
summary(training$y)
summary(testing$y)
  
# creating the model2
tree2 <- rpart(y ~.,training,method = 'class')
  
# ploting the tree
rpart.plot(tree2,box.palette = 'RdBu'
            ,shadow.col = 'gray'
            ,nn=TRUE,main='Decision Tree - Model1')
  
#-----------realizing prediction on training database
pred <- predict(tree2,training)
pred <- as.data.frame(pred)
  
# converting percentagens predicted into the classes
training$pred <- ifelse(pred$yes>pred$no,'yes','no')
training$pred <- as.factor(training$pred)
  
# confusionmatrix and results
confusion <- confusionMatrix(training$y,training$pred)
confusion
  
accuracy <- confusion$overall[1]
accuracy
  
#------------ realizing prediction on testing database
pred <- predict(tree2,testing)
pred <- as.data.frame(pred)
  
# converting percentagens predicted into the classes
testing$pred <- ifelse(pred$yes>pred$no,'yes','no')
testing$pred <- as.factor(testing$pred)
  
# confusionmatrix and results
confusion2 <- confusionMatrix(testing$y,testing$pred)
draw_confusion_matrix(confusion2)

# accuracy
accuracy_tree2 <- confusion2$overall[1]
accuracy_tree2

# A GREAT improvement of our accuracy! Let's test with a random forest

#---------------------------------------------------------------------------------------------------#
#---------------------- Creating the third model using a random forest -----------------------------#
#---------------------------------------------------------------------------------------------------#

# We have here a ambalance class problem, to solve this, let's use Undersampling 
# method to create a stratifyed sample to solve this problem with 4500 from class yes and 4500 from class 

# spliting our data set into train and test
set.seed(244)
sampl <- strata(bank,'y',c(4500,4500),'srswr')

samplee <- sample(2,9000,replace = T,prob = c(0.5,0.5))

sampl_train <- sampl[samplee==1,]
sampl_test <- sampl[samplee==2,]

training <- bank[sampl_train$ID_unit,]
testing <- bank[sampl_test$ID_unit,]

# creating the model
set.seed(244)
forest <- randomForest(y ~.,training,ntree=150,importance=T)

# Ploting the model
plot(forest,main='Forest')

# The prediction of a random forest into train dataset will be close to 100%, 
# lets analyse the test accuracy

#------------ realizing prediction on testing database
pred <- predict(forest,testing)
pred <- as.factor(pred)

# creating the confusion matrix
confusion_forest <- confusionMatrix(testing$y,pred)
draw_confusion_matrix(confusion_forest) # ploting results

#------------ realizing prediction using type 'prob' to analyse ROC curve and AUC
pred <- predict(forest,testing,type='prob') # predicting

ROC_rf <- roc(testing$y, pred[,2])

ROC_rf_auc <- auc(ROC_rf)

plot(ROC_rf, col = "blue", main = "ROC For Random Forest") #ploting the curve

AUC <- paste("Area under curve of random forest: ", round(ROC_rf_auc,4)*100,"%") # seeing AUC
AUC

# With a good treatemant and a emsemble model we improve our accuracy from 0.69% to 79%
# a GREAT improvment, seeing this results the random forest model was showed much more accuratly and a AUC of 86%

