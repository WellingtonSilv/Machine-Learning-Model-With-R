#---------------------------------------------------------------------------------------------------#
#--------------------------------------Importing librarys and datasets------------------------------#
#---------------------------------------------------------------------------------------------------#

#charging librarys
library(tidyverse)
library(ggpubr)
library(gridExtra)
library(GGally)
library(caret)
library(naivebayes)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ROCR)

#changing direcory
setwd("D:/Wellington/Downloads")

#importing the database
bank <- read.csv("bank-full.csv",header = TRUE,sep=";")

# Removing duration(Important note: this attribute highly affects the output 
# target (e.g., if duration=0 then y='no'). Yet, the duration is not known before a call is performed.)
bank$duration <- NULL

#---------------------------------------------------------------------------------------------------#
#----------------------------------------- Exploratory Analyses-------------------------------------#
#---------------------------------------------------------------------------------------------------#

head(bank) # seeing the first 5 rows

dim(bank) # verifying dimentions

str(bank) # verifying data structure

summary(bank) # verifying missing values

# Creating a boxplot for each numerical variables vs target
bp_age <- ggplot(bank, aes(x = y, y = age)) +
  geom_boxplot(fill = "#228B22", colour = "#1F3552", alpha = 0.6) +
  scale_y_continuous(name = "Age") +
  scale_x_discrete(name = "Target") +
  ggtitle("Age") +
  theme_gray() + 
  theme(plot.title = element_text(hjust = 0.5))

bp_balance <- ggplot(bank, aes(x = y, y = balance )) +
  geom_boxplot(fill = "#228B22", colour = "#1F3552", alpha = 0.6) +
  scale_y_continuous(name = "Balance") +
  scale_x_discrete(name = "Target") +
  ggtitle("Balance") +
  theme_gray() + 
  theme(plot.title = element_text(hjust = 0.5))

bp_day <- ggplot(bank, aes(x = y, y = day )) +
  geom_boxplot(fill = "#228B22", colour = "#1F3552", alpha = 0.6) +
  scale_y_continuous(name = "Day") +
  scale_x_discrete(name = "Target") +
  ggtitle("Day") +
  theme_gray() + 
  theme(plot.title = element_text(hjust = 0.5))

bp_campaign <- ggplot(bank, aes(x = y, y = campaign )) +
  geom_boxplot(fill = "#228B22", colour = "#1F3552", alpha = 0.6) +
  scale_y_continuous(name = "Campaign") +
  scale_x_discrete(name = "Target") +
  ggtitle("Campaign") +
  theme_gray() + 
  theme(plot.title = element_text(hjust = 0.5))

bp_pdays <- ggplot(bank, aes(x = y, y = pdays )) +
  geom_boxplot(fill = "#228B22", colour = "#1F3552", alpha = 0.6) +
  scale_y_continuous(name = "pdays") +
  scale_x_discrete(name = "Target") +
  ggtitle("Pdays") +
  theme_gray() + 
  theme(plot.title = element_text(hjust = 0.5))

bp_previous <- ggplot(bank, aes(x = y, y = previous )) +
  geom_boxplot(fill = "#228B22", colour = "#1F3552", alpha = 0.6) +
  scale_y_continuous(name = "Previous") +
  scale_x_discrete(name = "Target") +
  ggtitle("Previous") +
  theme_gray() + 
  theme(plot.title = element_text(hjust = 0.5))

# Plotting all boxplots into a unique graph
grid.arrange(bp_age, bp_balance, bp_day
             ,bp_campaign, bp_pdays, bp_previous
             , nrow = 2, ncol = 4)

# Creating variables with freequency of yes and no for each category of each discrete variables
job <- bank %>% count(job, y)

marital <- bank %>% count(marital, y)

education <- bank %>% count(education, y)

default <- bank %>% count(default, y)

housing <- bank %>% count(housing, y)

loan <- bank %>% count(loan, y)

contact <- bank %>% count(contact, y)

month <- bank %>% count(month, y)

poutcome <- bank %>% count(poutcome, y)

# Creating and storaging in variables a graphic with the freaquencys above
bp_job <- ggplot(job, aes(x = job, y = n )) +
  geom_bar(aes(fill = job), stat = "identity", color = "white") +
  scale_y_discrete(name = "Frequencia") +
  scale_x_discrete(name = "Job") +
  ggtitle("Job") +
  facet_wrap(~y) + 
  theme_gray() + 
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())

bp_marital <- ggplot(marital, aes(x = marital, y = n )) +
  geom_bar(aes(fill = marital), stat = "identity", color = "white") +
  scale_y_discrete(name = "Frequencia") +
  scale_x_discrete(name = "Marital") +
  ggtitle("Marital") +
  facet_wrap(~y) + 
  theme_gray() +
  theme(axis.text.x = element_text(angle = 90),plot.title = element_text(hjust = 0.5))

bp_education <- ggplot(education, aes(x = education, y = n )) +
  geom_bar(aes(fill = education), stat = "identity", color = "white") +
  scale_y_discrete(name = "Frequencia") +
  scale_x_discrete(name = "education") +
  ggtitle("Education") +
  facet_wrap(~y) + 
  theme_gray() + 
  theme(axis.text.x = element_text(angle = 90),plot.title = element_text(hjust = 0.5))

bp_default <- ggplot(default, aes(x = default, y = n )) +
  geom_bar(aes(fill = default), stat = "identity", color = "white") +
  scale_y_discrete(name = "Frequencia") +
  scale_x_discrete(name = "default") +
  ggtitle("Default") +
  facet_wrap(~y) + 
  theme_gray() + 
  theme(axis.text.x = element_text(angle = 90),plot.title = element_text(hjust = 0.5))

bp_housing <- ggplot(housing, aes(x = housing, y = n )) +
  geom_bar(aes(fill = housing), stat = "identity", color = "white") +
  scale_y_discrete(name = "Frequencia") +
  scale_x_discrete(name = "housing") +
  ggtitle("Housing") +
  facet_wrap(~y) + 
  theme_gray() + 
  theme(axis.text.x = element_text(angle = 90),plot.title = element_text(hjust = 0.5))

bp_loan <- ggplot(loan, aes(x = loan, y = n )) +
  geom_bar(aes(fill = loan), stat = "identity", color = "white") +
  scale_y_discrete(name = "Frequencia") +
  scale_x_discrete(name = "loan") +
  ggtitle("Loan") +
  facet_wrap(~y) + 
  theme_gray() + 
  theme(axis.text.x = element_text(angle = 90),plot.title = element_text(hjust = 0.5))

bp_contact <- ggplot(contact, aes(x = contact, y = n )) +
  geom_bar(aes(fill = contact), stat = "identity", color = "white") +
  scale_y_discrete(name = "Frequencia") +
  scale_x_discrete(name = "contact") +
  ggtitle("Contact") +
  facet_wrap(~y) + 
  theme_gray() + 
  theme(axis.text.x = element_text(angle = 90),plot.title = element_text(hjust = 0.5))

bp_month <- ggplot(month, aes(x = month, y = n )) +
  geom_bar(aes(fill = month), stat = "identity", color = "white") +
  scale_y_discrete(name = "Frequencia") +
  scale_x_discrete(name = "month") +
  ggtitle("month") +
  facet_wrap(~y) + 
  theme_gray() + 
  theme(axis.text.x = element_text(angle = 90),plot.title = element_text(hjust = 0.5))

bp_poutcome <- ggplot(poutcome, aes(x = poutcome, y = n )) +
  geom_bar(aes(fill = poutcome), stat = "identity", color = "white") +
  scale_y_discrete(name = "Frequencia") +
  scale_x_discrete(name = "poutcome") +
  ggtitle("Poutcome") +
  facet_wrap(~y) + 
  theme_gray() + 
  theme(axis.text.x = element_text(angle = 90),plot.title = element_text(hjust = 0.5))

# Plotting the graphics together
grid.arrange(bp_marital, bp_education, bp_default, bp_housing
             , nrow = 2, ncol = 2)

grid.arrange(bp_loan, bp_contact, bp_month, bp_poutcome
             , nrow = 2, ncol = 2)

# Ploting class proportion
proportion <- bank %>% count(y)

# Ploting a bar graphic with class proportion
ggplot(proportion, aes(x = y, y = n)) +
  geom_bar(fill = c('blue','red'), stat = "identity") +
  geom_text(aes(label=n), vjust = -0.3) +
  ggtitle('Class proportion')+
  theme_pubclean()

# Ploting correlation between numerical variables
bank %>%
  keep(is.numeric) %>%
  ggcorr(name = 'correlations'
         ,label = T
         ,label_alpha = T
         ,label_color = 'black'
         ,label_round = 2
         ,label_size = 6)

#---------------------------------------------------------------------------------------------------#
#------------------------- Dealing with unbalance class problem ------------------------------------#
#---------------------------------------------------------------------------------------------------#

# Using oversampling
set.seed(42)
bank_oversample <- upSample(bank, bank$y)

str(bank_oversample) # seeing new database structure

# Ploting class proportion
proportion2 <- bank_oversample %>% count(Class)

# Ploting a bar graphic with class proportion
ggplot(proportion2, aes(x = Class, y = n)) +
  geom_bar(fill = c('blue','red'), stat = "identity") +
  geom_text(aes(label=n), vjust = -0.3) +
  ggtitle('Class proportion')+
  theme_pubclean()


# Creating train and test data without oversampling to do validation
set.seed(42)
inTrain <- createDataPartition(bank$y, p = 0.7, list = FALSE)

train_noover <- bank[inTrain, ]

test_noover <- bank[-inTrain, ]

dim(train_noover)
dim(test_noover)

# Creating train and test data with oversampling to training
set.seed(42)
inTrain2 <- createDataPartition(bank_oversample$Class, p = 0.7, list = FALSE)

train_over <- bank_oversample[inTrain2, ]

test_over <- bank_oversample[-inTrain2, ]

dim(train_over)
dim(test_over)

#---------------------------------------------------------------------------------------------------#
#-------------------- Creating the model1 using naive bayes for classification ----------------#
#---------------------------------------------------------------------------------------------------#

# Training the model
set.seed(42)
nb1 = naive_bayes(Class ~ . , laplace = 1, usekernel = F, data = train_over)

# Predicting on train_over dataset
nb_train_pred1 <- predict(nb1, train_over, type = "class")

confusionMatrix(nb_train_pred1, train_over$Class, positive = 'yes')

# Predicting in test_noover dataset
nb_test_pred1 <- predict(nb1, test_noover, type = "class")

confusionMatrix(nb_test_pred1, test_noover$y, positive = 'yes') # plotting confusionmatrix

#---------------------------------------------------------------------------------------------------#
#-------------------------------------- Feature Engenering -----------------------------------------#
#---------------------------------------------------------------------------------------------------#

# Dealing with numerical features

# Treating campaign: number of contacts performed during this campaign and for this client (numeric, includes last contact)
bank$campaign_cat <- as.factor(ifelse(bank$campaign<=4,'priority','not_priority')) # converting into categorys

# Treating age and classifying into young, mature and old
bank$age_cat <-  ifelse(bank$age>=50,'old',NA) # converting to categorical
bank$age_cat <-  ifelse(bank$age<=30,'young',bank$age_cat)
bank$age_cat <-  ifelse(bank$age>30 & bank$age<50,'mature',bank$age_cat)
bank$age_cat <- as.factor(bank$age_cat)

# Classifying previous into contacted before and non_contacted before
bank$cont_before <- as.factor(ifelse(bank$previous==0,'no','yes'))

# Classifying balance of clients into wealth levels
bank$balance_lvl <- ifelse(bank$balance<1500,'negative',NA)
bank$balance_lvl <- ifelse(bank$balance<=10000 & bank$balance>=0,'lvl1',bank$balance_lvl)
bank$balance_lvl <- ifelse(bank$balance>10000 & bank$balance<=40000,'lvl2',bank$balance_lvl)
bank$balance_lvl <- ifelse(bank$balance>40000,'lvl3',bank$balance_lvl)
bank$balance_lvl <- as.factor(bank$balance_lvl)

# Converting day variable into start of month, middle of month and final of month
bank$moth_stage <- ifelse(bank$day<=7,'start_m',NA)
bank$moth_stage <- ifelse(bank$day>=22,'final_m',bank$moth_stage)
bank$moth_stage <- ifelse(bank$day<22 & bank$day>7,'middle_m',bank$moth_stage)
bank$moth_stage <- as.factor(bank$moth_stage)

sum(is.na(bank)) # verifying na's

# Visualizing new categorical features
campaign_cat <- bank %>% count(campaign_cat, y)
age_cat <- bank %>% count(age_cat, y)
cont_before <- bank %>% count(cont_before, y)
balance_lvl <- bank %>% count(balance_lvl, y)
moth_stage <- bank %>% count(moth_stage, y)

# Creating and storaging in variables a graphic with the freaquencys above
campaign_cat <- ggplot(campaign_cat, aes(x = campaign_cat, y = n )) +
  geom_bar(aes(fill = campaign_cat), stat = "identity", color = "white") +
  scale_y_discrete(name = "Frequencia") +
  scale_x_discrete(name = "campaign_cat") +
  ggtitle("campaign_cat") +
  facet_wrap(~y) + 
  theme_gray() + 
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())

age_cat <- ggplot(age_cat, aes(x = age_cat, y = n )) +
  geom_bar(aes(fill = age_cat), stat = "identity", color = "white") +
  scale_y_discrete(name = "Frequencia") +
  scale_x_discrete(name = "age_cat") +
  ggtitle("age_cat") +
  facet_wrap(~y) + 
  theme_gray() + 
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())

cont_before <- ggplot(cont_before, aes(x = cont_before, y = n )) +
  geom_bar(aes(fill = cont_before), stat = "identity", color = "white") +
  scale_y_discrete(name = "Frequencia") +
  scale_x_discrete(name = "cont_before") +
  ggtitle("cont_before") +
  facet_wrap(~y) + 
  theme_gray() + 
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())

balance_lvl <- ggplot(balance_lvl, aes(x = balance_lvl, y = n )) +
  geom_bar(aes(fill = balance_lvl), stat = "identity", color = "white") +
  scale_y_discrete(name = "Frequencia") +
  scale_x_discrete(name = "balance_lvl") +
  ggtitle("balance_lvl") +
  facet_wrap(~y) + 
  theme_gray() + 
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())

moth_stage <- ggplot(moth_stage, aes(x = moth_stage, y = n )) +
  geom_bar(aes(fill = moth_stage), stat = "identity", color = "white") +
  scale_y_discrete(name = "Frequencia") +
  scale_x_discrete(name = "moth_stage") +
  ggtitle("moth_stage") +
  facet_wrap(~y) + 
  theme_gray() + 
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())

# Plotting the graphics together
grid.arrange(campaign_cat, age_cat, balance_lvl, moth_stage,cont_before
             , nrow = 2, ncol = 3)

# Creating new features based on categorical features
bank$no_house_married <- as.factor(ifelse(bank$marital=='married' & bank$housing=='no',1,0))

bank$no_loan_secedu <- as.factor(ifelse(bank$education=='secondary' & bank$loan=='no',1,0))

#bank$no_loan_terc <- as.factor(ifelse(bank$education=='tertiary' & bank$loan=='no',1,0))

bank$no_credit_no_house <- as.factor(ifelse(bank$default=='no' & bank$housing=='no',1,0))

bank$cell_cont_before <- as.factor(ifelse(bank$contact=='cellular' & bank$cont_before=='no',1,0))

bank$mature_lvl1 <- as.factor(ifelse(bank$age_cat=='mature' & bank$balance_lvl=='lvl1',1,0))

# Creating new features with our numerical features
bank$age_balan <- bank$balance/bank$age # creating a new variables with relation between age and balance
bank$previous_pdays <- bank$previous/bank$pdays

# visualizing new numerical features
bp_age_balan <- ggplot(bank, aes(x = y, y = age_balan )) +
  geom_boxplot(fill = "#228B22", colour = "#1F3552", alpha = 0.6) +
  scale_y_continuous(name = "age_balan") +
  scale_x_discrete(name = "Target") +
  ggtitle("age_balan") +
  theme_gray() + 
  theme(plot.title = element_text(hjust = 0.5))

bp_previous_pdays <- ggplot(bank, aes(x = y, y = previous_pdays )) +
  geom_boxplot(fill = "#228B22", colour = "#1F3552", alpha = 0.6) +
  scale_y_continuous(name = "previous_pdays") +
  scale_x_discrete(name = "Target") +
  ggtitle("previous_pdays") +
  theme_gray() + 
  theme(plot.title = element_text(hjust = 0.5))

# Plotting all boxplots into a unique graph
grid.arrange(bp_previous_pdays, bp_age_balan
             , nrow = 1, ncol = 2)

# Converting new numerical features into categorical features
bank$cat_age_balan <- ifelse(bank$age_balan>=100,'out',NA)
bank$cat_age_balan <- ifelse(bank$age_balan<0,'out',bank$cat_age_balan)
bank$cat_age_balan <- ifelse(bank$age_balan>=0 & bank$age_balan<100,'in',bank$cat_age_balan)
bank$cat_age_balan <- as.factor(bank$cat_age_balan)

bank$cat_previous_pdays <- as.factor(ifelse(bank$previous_pdays==0,'inn','outt'))

# removing numerical variables
bank$campaign <- NULL
bank$age <- NULL
bank$balance <- NULL
bank$previous <- NULL
bank$pdays <- NULL
bank$day <- NULL
bank$age_balan <- NULL
bank$previous_pdays <- NULL

#---------------------------------------------------------------------------------------------------#
#-------------------- Creating the model2 using naive bayes for classification ----------------#
#---------------------------------------------------------------------------------------------------#

# Using oversampling
set.seed(42)
bank_oversample2 <- upSample(bank, bank$y)

# Creating train and test data without oversampling to do validation
set.seed(42)
inTrain <- createDataPartition(bank$y, p = 0.7, list = FALSE)

train_noover <- bank[inTrain, ]

test_noover <- bank[-inTrain, ]

# Creating train and test data with oversampling to training
set.seed(42)
inTrain2 <- createDataPartition(bank_oversample2$Class, p = 0.7, list = FALSE)

train_over <- bank_oversample2[inTrain2, ]

test_over <- bank_oversample2[-inTrain2, ]

# Training the model2
set.seed(42)
nb2 = naive_bayes(Class ~ . , laplace = 1, usekernel = F, data = train_over)

# Predicting on train_over dataset
nb_train_pred2 <- predict(nb2, train_over, type = "class")

confusionMatrix(nb_train_pred2, train_over$Class, positive = 'yes')

# Previsao na base de teste sem oversampling

nb_test_pred2 <- predict(nb2, test_noover, type = "class")

confusionMatrix(nb_test_pred2, test_noover$y, positive = 'yes')

# evaluating the model with AUC

#Plotting AUC
probabilits <- predict(nb2, type ='prob', test_noover) 

nb2_probs <- prediction(probabilits[,2], test_noover$y)
plot(performance(nb2_probs, "tpr", "fpr"), col = "red", main = "Area Under the Curve - AUC")
abline(0,1, lty = 8, col = "grey")

#AUC
auc <- performance(nb2_probs, "auc")
value_auc <- slot(auc, "y.values")[[1]]
print("Auc")
value_auc

#---------------------------------------------------------------------------------------------------#
#--------------- Creating the model3 using Decision Trees for classification -----------------#
#---------------------------------------------------------------------------------------------------#

# Training the model2
tree1 = rpart(Class ~ ., data = train_over,method='class')

rpart.plot(tree1,box.palette = 'RdBu'
           ,shadow.col = 'gray'
           ,nn=T,main='Decision Tree')

# Predicting on train_over dataset
tree_train_pred1 <- predict(tree1, train_over, type = "class")

confusionMatrix(tree_train_pred1, train_over$Class, positive = 'yes')

# Predicting in test_noover dataset
tree_test_pred1 <- predict(tree1, test_noover, type = "class")

confusionMatrix(tree_test_pred1, test_noover$y, positive = 'yes')

#Plotting AUC for decision tree model
probabilits <- predict(tree1, type ='prob', test_noover) 

nb2_probs <- prediction(probabilits[,2], test_noover$y)
plot(performance(nb2_probs, "tpr", "fpr"), col = "red", main = "AUC - Decision Tree")
abline(0,1, lty = 8, col = "grey")

#AUC
auc_trees <- performance(nb2_probs, "auc")
value_auc_trees <- slot(auc_trees, "y.values")[[1]]
print("Auc Decision Tree: ")
value_auc_trees

#---------------------------------------------------------------------------------------------------#
#--------------- Creating the model4 using random forest for classification -----------------#
#---------------------------------------------------------------------------------------------------#

# Training the model2
set.seed(42)
forest1 = randomForest(Class ~ ., data = train_over,ntree=100,importance=T)

# Predicting on train_over dataset
forest_train_pred1 <- predict(forest1, train_over, type = "class")

plot(forest1,main='Random Forest Error Decreassing') # Ploting the forest error

# Catting var importance from model
importance <- as.data.frame(forest1$importance)
importance$features <- row.names(importance)
importance$MeanDecreaseGini <- round(importance$MeanDecreaseGini,2)

# Ploting a bar graphic with features importance proportion
ggplot(importance, aes(x = features, y = MeanDecreaseGini)) +
  geom_bar(fill = "#009999", stat = "identity") +
  geom_text(aes(label=MeanDecreaseGini), vjust = -0.3) +
  ggtitle('Features Importance')+
  theme_pubclean()

confusionMatrix(forest_train_pred1, train_over$Class, positive = 'yes')

# Predicting in test_noover dataset
forest_test_pred1 <- predict(forest1, test_noover, type = "class")

confusionMatrix(forest_test_pred1, test_noover$y, positive = 'yes')

#Plotting AUC for decision tree model
prob_forest <- predict(forest1, type ='prob', test_noover)

forest_probs <- prediction(prob_forest[,2], test_noover$y)
plot(performance(forest_probs, "tpr", "fpr"), col = "red", main = "AUC - Random Forest")
abline(0,1, lty = 8, col = "grey")

#AUC from random forests
auc_forest <- performance(forest_probs, "auc")
value_auc_forest <- slot(auc_forest, "y.values")[[1]]
print("AUC Random Forest: ")
value_auc_forest
