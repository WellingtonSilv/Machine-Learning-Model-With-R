#------------------------------------------------------------------------------------------#
#--------------------- Creating a machine learning model to predict shares of -------------#
#--------------------- online news using OnlineNewsPopularity from UCI Machine ------------#
#--------------------- Learning Repository  -----------------------------------------------#
#------- https://archive.ics.uci.edu/ml/datasets/online+news+popularity -------------------#
#------------------------------------------------------------------------------------------#

#---- The researchers achieved 67% accuracy using a random forest model, my challengy here--
#---  is try to get closer of this accuracy using a linear regression

# Charging librarys
library(caret)
library(Metrics)
library(roll)

# Changing directory
setwd("D:/MY OWN R CODES/CURSOS/CURSO OUTSPOKEN/bases de dados") #put directory where OnlineNewsPopularity are storaged

# Importing dataset from directory
pop2 <- read.csv2("OnlineNewsPopularity.csv", sep = ",", dec =".")
head(pop2)

# Removing url column
pop <- pop2[,2:61]

# Analysing dataset
str(pop) # Analysing structure

dim(pop) # Verifying dimentions

correla <- cor(pop) # Verifying correlations between target(shares) and variables
image(correla)

plot(pop$shares) # Analysing the target
summary(pop$shares) # Analysing the target

#---------- Creating model1 using linear regression ----------------#

# Dividing dataset into train and test

set.seed(42) # Set a seed to preserv our model
inTrain <- createDataPartition(y = pop$shares, p = 0.7, list = FALSE) # Dividing between 70% train and 30% test

train <- pop[inTrain,] # Storaging train data
test <- pop[-inTrain,] # Storaging test data

# Creating the model1
model1 <- lm(shares ~.,train)

suma1 <- summary(model1)
r_sq1 <- suma1$r.squared
suma1 #printing a summary of our model
r_sq1 #printing r_squared

# Preciting on train dataset
pred1 <- predict(model1, train)

# Calculating mae to evaluate the model stability
mae_train1 <- mae(train$shares,pred1)
round(mae_train1, 2)
round(mae_train1/mean(train$shares)*100, 2)

# Preciting on test dataset
pred_test1 <- predict(model1, test)

# Calculating mae to evaluate the model stability
mae_test1 <- mae(test$shares,pred_test1)
ratio_mae1 <- round(mae_test1/mean(test$shares)*100, 2)
ratio_mae1

# Calculating accuracy
summary(pop$shares)

pred_test1_class <- as.factor(ifelse(pred_test1 > 1400, 1, 0))

shares_class1 <- as.factor(ifelse(test$shares > 1400, 1, 0))

accu1 <- confusionMatrix(pred_test1_class, shares_class1)
accu1$overall[1] #printing accuracy


# Treating the target replacing outlayers trying to improve results
pop$shares <- ifelse(pop$shares>summary(pop$shares)[5],2900,pop$shares)
pop$shares <- ifelse(pop$shares<summary(pop$shares)[2],50,pop$shares)
pop$shares <- ifelse(pop$shares>=50
                      & pop$shares<=summary(pop$shares)[5]
                      ,pop$shares,pop$shares)

plot(pop$shares) # Seeing the new target

# Removing correlations between variables
target <- pop$shares
pop$shares <- NULL

correlationMatrix <- cor(pop) 

correlationMatrix[is.na(correlationMatrix)] <- 0

highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.7)

print(highlyCorrelated)

pop <- pop[,-highlyCorrelated]

length(names(pop))

pop$shares <- target

#---------- Creating model2 using linear regression ----------------#

# Dividing dataset into train and test

set.seed(42) # Set a seed to preserv our model
inTrain <- createDataPartition(y = pop$shares, p = 0.7, list = FALSE) # Dividing between 70% train and 30% test

train <- pop[inTrain,] # Storaging train data
test <- pop[-inTrain,] # Storaging test data

# Creating the model1
model2 <- lm(shares ~.,train)

suma2 <- summary(model2)
r_sq2 <- suma2$r.squared
suma2 #printing a summary of our model
r_sq2 #printing r_squared

# Here a great improvement of r_squared was observed

# Preciting on train dataset
pred2 <- predict(model2, train)

# Calculating mae to evaluate the model stability
mae_train2 <- mae(train$shares,pred2)
round(mae_train2, 2)
round(mae_train2/mean(train$shares)*100, 2)

# Preciting on test dataset
pred_test2 <- predict(model2, test)

# Calculating mae to evaluate the model stability
mae_test2 <- mae(test$shares,pred_test2)
ratio_mae2 <- round(mae_test2/mean(test$shares)*100, 2)
ratio_mae2

# Calculating accuracy
summary(pop$shares)

pred_test2_class <- as.factor(ifelse(pred_test2 > 1400, 1, 0))

shares_class2 <- as.factor(ifelse(test$shares > 1400, 1, 0))

accu2 <- confusionMatrix(pred_test2_class, shares_class2)
accu2$overall[1] #printing accuracy

# Was a creat accuracy and r squared improvement, mae reduction and mae stability was mantened
# Let's try to improve this results


# ---- Treating variables replacing outlayers like we did in target

# Here I select varialbles that are not binary 0 or 1
pop$global_sentiment_polarity <- ifelse(pop$global_sentiment_polarity>summary(pop$global_sentiment_polarity)[5]
                                        ,summary(pop$global_sentiment_polarity)[5],pop$global_sentiment_polarity)
pop$global_sentiment_polarity <- ifelse(pop$global_sentiment_polarity<summary(pop$global_sentiment_polarity)[2]
                                        ,summary(pop$global_sentiment_polarity)[2],pop$global_sentiment_polarity)
pop$global_sentiment_polarity <- ifelse(pop$global_sentiment_polarity>=summary(pop$global_sentiment_polarity)[2] 
                                        & pop$global_sentiment_polarity<=summary(pop$global_sentiment_polarity)[5]
                                        ,pop$global_sentiment_polarity,pop$global_sentiment_polarity)

pop$global_subjectivity <- ifelse(pop$global_subjectivity>summary(pop$global_subjectivity)[5]
                                  ,summary(pop$global_subjectivity)[5],pop$global_subjectivity)
pop$global_subjectivity <- ifelse(pop$global_subjectivity<summary(pop$global_subjectivity)[2]
                                  ,summary(pop$global_subjectivity)[2],pop$global_subjectivity)
pop$global_subjectivity <- ifelse(pop$global_subjectivity>=summary(pop$global_subjectivity)[2] 
                                  & pop$global_subjectivity<=summary(pop$global_subjectivity)[5]
                                  ,pop$global_subjectivity,pop$global_subjectivity)

pop$max_negative_polarity <- ifelse(pop$max_negative_polarity>summary(pop$max_negative_polarity)[5]
                                    ,summary(pop$max_negative_polarity)[5],pop$max_negative_polarity)
pop$max_negative_polarity <- ifelse(pop$max_negative_polarity<summary(pop$max_negative_polarity)[2]
                                    ,summary(pop$max_negative_polarity)[2],pop$max_negative_polarity)
pop$max_negative_polarity <- ifelse(pop$max_negative_polarity>=summary(pop$max_negative_polarity)[2] 
                                    & pop$max_negative_polarity<=summary(pop$max_negative_polarity)[5]
                                    ,pop$max_negative_polarity,pop$max_negative_polarity)

pop$global_rate_positive_words <- ifelse(pop$global_rate_positive_words>summary(pop$global_rate_positive_words)[5]
                                         ,summary(pop$global_rate_positive_words)[5],pop$global_rate_positive_words)
pop$global_rate_positive_words <- ifelse(pop$global_rate_positive_words<summary(pop$global_rate_positive_words)[2]
                                         ,summary(pop$global_rate_positive_words)[2],pop$global_rate_positive_words)
pop$global_rate_positive_words <- ifelse(pop$global_rate_positive_words>=summary(pop$global_rate_positive_words)[2] 
                                         & pop$global_rate_positive_words<=summary(pop$global_rate_positive_words)[5]
                                         ,pop$global_rate_positive_words,pop$global_rate_positive_words)

pop$num_videos <- ifelse(pop$num_videos>summary(pop$num_videos)[5]
                         ,summary(pop$num_videos)[5],pop$num_videos)
pop$num_videos <- ifelse(pop$num_videos<summary(pop$num_videos)[2]
                         ,summary(pop$num_videos)[2],pop$num_videos)
pop$num_videos <- ifelse(pop$num_videos>=summary(pop$num_videos)[2] 
                         & pop$num_videos<=summary(pop$num_videos)[5]
                         ,pop$num_videos,pop$num_videos)

pop$num_imgs <- ifelse(pop$num_imgs>summary(pop$num_imgs)[5]
                       ,summary(pop$num_imgs)[5],pop$num_imgs)
pop$num_imgs <- ifelse(pop$num_imgs<summary(pop$num_imgs)[2]
                       ,summary(pop$num_imgs)[2],pop$num_imgs)
pop$num_imgs <- ifelse(pop$num_imgs>=summary(pop$num_imgs)[2] 
                       & pop$num_imgs<=summary(pop$num_imgs)[5]
                       ,pop$num_imgs,pop$num_imgs)

pop$kw_avg_min <- ifelse(pop$kw_avg_min>summary(pop$kw_avg_min)[5]
                         ,summary(pop$kw_avg_min)[5],pop$kw_avg_min)
pop$kw_avg_min <- ifelse(pop$kw_avg_min<summary(pop$kw_avg_min)[2]
                         ,summary(pop$kw_avg_min)[2],pop$kw_avg_min)
pop$kw_avg_min <- ifelse(pop$kw_avg_min>=summary(pop$kw_avg_min)[2] 
                         & pop$kw_avg_min<=summary(pop$kw_avg_min)[5]
                         ,pop$kw_avg_min,pop$kw_avg_min)

pop$kw_max_avg <- ifelse(pop$kw_max_avg>summary(pop$kw_max_avg)[5]
                         ,summary(pop$kw_max_avg)[5],pop$kw_max_avg)
pop$kw_max_avg <- ifelse(pop$kw_max_avg<summary(pop$kw_max_avg)[2]
                         ,summary(pop$kw_max_avg)[2],pop$kw_max_avg)
pop$kw_max_avg <- ifelse(pop$kw_max_avg>=summary(pop$kw_max_avg)[2] 
                         & pop$kw_max_avg<=summary(pop$kw_max_avg)[5]
                         ,pop$kw_max_avg,pop$kw_max_avg)

pop$self_reference_min_shares <- ifelse(pop$self_reference_min_shares>summary(pop$self_reference_min_shares)[5]
                                        ,summary(pop$self_reference_min_shares)[5],pop$self_reference_min_shares)
pop$self_reference_min_shares <- ifelse(pop$self_reference_min_shares<summary(pop$self_reference_min_shares)[2]
                                        ,summary(pop$self_reference_min_shares)[2],pop$self_reference_min_shares)
pop$self_reference_min_shares <- ifelse(pop$self_reference_min_shares>=summary(pop$self_reference_min_shares)[2] 
                                        & pop$self_reference_min_shares<=summary(pop$self_reference_min_shares)[5]
                                        ,pop$self_reference_min_shares,pop$self_reference_min_shares)

pop$self_reference_max_shares <- ifelse(pop$self_reference_max_shares>summary(pop$self_reference_max_shares)[5]
                                        ,summary(pop$self_reference_max_shares)[5],pop$self_reference_max_shares)
pop$self_reference_max_shares <- ifelse(pop$self_reference_max_shares<summary(pop$self_reference_max_shares)[2]
                                        ,summary(pop$self_reference_max_shares)[2],pop$self_reference_max_shares)
pop$self_reference_max_shares <- ifelse(pop$self_reference_max_shares>=summary(pop$self_reference_max_shares)[2] 
                                        & pop$self_reference_max_shares<=summary(pop$self_reference_max_shares)[5]
                                        ,pop$self_reference_max_shares,pop$self_reference_max_shares)

pop$global_rate_positive_words <- ifelse(pop$global_rate_positive_words>summary(pop$global_rate_positive_words)[5]
                                         ,summary(pop$global_rate_positive_words)[5],pop$global_rate_positive_words)
pop$global_rate_positive_words <- ifelse(pop$global_rate_positive_words<summary(pop$global_rate_positive_words)[2]
                                         ,summary(pop$global_rate_positive_words)[2],pop$global_rate_positive_words)
pop$global_rate_positive_words <- ifelse(pop$global_rate_positive_words>=summary(pop$global_rate_positive_words)[2] 
                                         & pop$global_rate_positive_words<=summary(pop$global_rate_positive_words)[5]
                                         ,pop$global_rate_positive_words,pop$global_rate_positive_words)

pop$global_rate_negative_words <- ifelse(pop$global_rate_negative_words>summary(pop$global_rate_negative_words)[5]
                                         ,summary(pop$global_rate_negative_words)[5],pop$global_rate_negative_words)
pop$global_rate_negative_words <- ifelse(pop$global_rate_negative_words<summary(pop$global_rate_negative_words)[2]
                                         ,summary(pop$global_rate_negative_words)[2],pop$global_rate_negative_words)
pop$global_rate_negative_words <- ifelse(pop$global_rate_negative_words>=summary(pop$global_rate_negative_words)[2] 
                                         & pop$global_rate_negative_words<=summary(pop$global_rate_negative_words)[5]
                                         ,pop$global_rate_negative_words,pop$global_rate_negative_words)

pop$min_positive_polarity <- ifelse(pop$min_positive_polarity>summary(pop$min_positive_polarity)[5]
                                    ,summary(pop$min_positive_polarity)[5],pop$min_positive_polarity)
pop$min_positive_polarity <- ifelse(pop$min_positive_polarity<summary(pop$min_positive_polarity)[2]
                                    ,summary(pop$min_positive_polarity)[2],pop$min_positive_polarity)
pop$min_positive_polarity <- ifelse(pop$min_positive_polarity>=summary(pop$min_positive_polarity)[2] 
                                    & pop$min_positive_polarity<=summary(pop$min_positive_polarity)[5]
                                    ,pop$min_positive_polarity,pop$min_positive_polarity)

#---------- Creating model3 using linear regression ----------------#

# Dividing dataset into train and test

set.seed(42) # Set a seed to preserv our model
inTrain <- createDataPartition(y = pop$shares, p = 0.7, list = FALSE) # Dividing between 70% train and 30% test

train <- pop[inTrain,] # Storaging train data
test <- pop[-inTrain,] # Storaging test data

# Creating the model1
model3 <- lm(shares ~.,train)

suma3 <- summary(model3)
r_sq3 <- suma3$r.squared
suma3 #printing a summary of our model
r_sq3 #printing r_squared

# Here a great improvement of r_squared was observed from 0.12 to 0.16
# Let's verify mae stability

# Preciting on train dataset
pred3 <- predict(model3, train)

# Calculating mae to evaluate the model stability
mae_train3 <- mae(train$shares,pred3)
round(mae_train3, 2)
round(mae_train3/mean(train$shares)*100, 2)

# Preciting on test dataset
pred_test3 <- predict(model3, test)

# Calculating mae to evaluate the model stability
mae_test3 <- mae(test$shares,pred_test3)
ratio_mae3 <- round(mae_test3/mean(test$shares)*100, 2)
ratio_mae3

# ------Mae continue stable--------

# Calculating accuracy
summary(pop$shares)

pred_test3_class <- as.factor(ifelse(pred_test3 > 1400, 1, 0))

shares_class3 <- as.factor(ifelse(test$shares > 1400, 1, 0))

accu3 <- confusionMatrix(pred_test3_class, shares_class3)
accu3$overall[1] #printing accuracy

# Here we observ a improvement of our accuracy from 0.62 to 0.64
# Let's try to improve this creating new variables

# --- Creatig new variables-------
pop$var1 <- pop$max_positive_polarity/pop$global_subjectivity
pop$var2 <- pop$timedelta/pop$global_sentiment_polarity
pop$var3 <- pop$timedelta/pop$global_rate_positive_words
pop$var4 <- pop$timedelta/pop$global_rate_negative_words
pop$var5 <- pop$num_keywords/pop$avg_negative_polarity
pop$var6 <- pop$timedelta/pop$max_negative_polarity
pop$var7 <- pop$n_tokens_title/pop$min_positive_polarity
pop$var8 <- pop$timedelta/pop$title_subjectivity
pop$var9 <- pop$timedelta/pop$title_sentiment_polarity
pop$var10 <- pop$title_subjectivity/pop$global_subjectivity
pop$var11 <- pop$num_self_hrefs/pop$global_rate_negative_words
pop$var12 <- pop$global_rate_positive_words/pop$global_sentiment_polarity
pop$var13 <- pop$min_positive_polarity/pop$num_hrefs
pop$var14 <- pop$average_token_length/pop$self_reference_max_shares
pop$var15 <- pop$var11/pop$n_tokens_title
pop$var16 <- pop$title_subjectivity/pop$self_reference_min_shares
pop$var17 <- roll_var(pop$global_rate_positive_words,30)
pop$var18 <- roll_sd(pop$avg_negative_polarity,30)/roll_sd(pop$title_subjectivity,30)
pop$var19 <- roll_sd(pop$self_reference_min_shares,30)/roll_sd(pop$title_subjectivity,30)
pop$var20 <- roll_sd(pop$title_subjectivity,20)
pop$var21 <- roll_sd(pop$max_negative_polarity,10)*roll_mean(pop$max_negative_polarity,10)

# Replacing NA's NaN and Inf 
# values by 0 using travel function: 
# https://github.com/wellingtsilvdev/codes-with-real-utilities/commit/2ac247fd0899d264148022b7ee1ecffcddaebaaa

# Changing directory to import our function
setwd("D:/MY OWN R CODES/FUNÇÕES")
source('travel.R')

pop <- travel(pop,element = 0,last.na = T,first_row.na = T)

# Removing correlations between variables
target <- pop$shares
pop$shares <- NULL

correlationMatrix <- cor(pop) 

correlationMatrix[is.na(correlationMatrix)] <- 0

highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.7)

print(highlyCorrelated)

pop <- pop[,-highlyCorrelated]

length(names(pop))

pop$shares <- target

#---------- Creating model4 using linear regression ----------------#

# Dividing dataset into train and test

set.seed(42) # Set a seed to preserv our model
inTrain <- createDataPartition(y = pop$shares, p = 0.7, list = FALSE) # Dividing between 70% train and 30% test

train <- pop[inTrain,] # Storaging train data
test <- pop[-inTrain,] # Storaging test data

# Creating the model1
model4 <- lm(shares ~.,train)

suma4 <- summary(model4)
r_sq4 <- suma4$r.squared
suma4 #printing a summary of our model
r_sq4 #printing r_squared

# Here the r_squared almost are not improved

# Preciting on train dataset
pred4 <- predict(model4, train)

# Calculating mae to evaluate the model stability
mae_train4 <- mae(train$shares,pred4)
round(mae_train4, 2)
round(mae_train4/mean(train$shares)*100, 2)

# Preciting on test dataset
pred_test4 <- predict(model4, test)

# Calculating mae to evaluate the model stability
mae_test4 <- mae(test$shares,pred_test4)
ratio_mae4 <- round(mae_test4/mean(test$shares)*100, 2)
ratio_mae4

# ------Mae continue stable--------

# Calculating accuracy
summary(pop$shares)

pred_test4_class <- as.factor(ifelse(pred_test4 > 1400, 1, 0))

shares_class4 <- as.factor(ifelse(test$shares > 1400, 1, 0))

accu4 <- confusionMatrix(pred_test4_class, shares_class4)
accu4$overall[1] #printing accuracy

# There was not improvement in accuracy
# Let's see all models accuracy, r_squared and mae

# Model1
cat('Model1 R_squared:',round(r_sq1,3),"")
cat('Model1 mae_test:',round(ratio_mae1,3),"")
cat('Model1 accuracy:',round(accu1$overall[1],3),"")

# Model2
cat('Model2 R_squared:',round(r_sq2,3),"")
cat('Model2 mae_test:',round(ratio_mae2,3),"")
cat('Model2 accuracy:',round(accu2$overall[1],3),"")

# Model3
cat('Model3 R_squared:',round(r_sq3,3),"")
cat('Model3 mae_test:',round(ratio_mae3,3),"")
cat('Model3 accuracy:',round(accu3$overall[1],3),"")

# Model4
cat('Model4 R_squared:',round(r_sq4,3),"")
cat('Model4 mae_test:',round(ratio_mae4,3),"")
cat('Model4 accuracy:',round(accu4$overall[1],3),"")

