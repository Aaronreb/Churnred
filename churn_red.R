rm(list = ls())

setwd("C:/Users/ARON/Desktop/edwisor projects/Churn")
getwd()
# #loading Libraries
# x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "e1071",
      "DataCombine", "pROC", "doSNOW", "class", "readxl","ROSE")
# 
# install.packages if not
# lapply(x, install.packages)
#
# #load Packages
# lapply(x, require, character.only = TRUE)
# rm(x)

#Input Train & Test Data Source
train_Original = read.csv('Train_data.csv',header = T,na.strings = c(""," ","NA"))
test_Original = read.csv('Test_data.csv',header = T,na.strings = c(""," ","NA"))
#Creating backup of orginal data 
train = train_Original  
test = test_Original

###################################EXPLORING DATA################################################





# let us see the structure of data 
str(train)  
str(test)
class(train)


#Summary of data 
summary(train)

#unique value of each count
apply(train, 2,function(x) length(table(x)))


unique(train$number.customer.service.calls)

 #as we see that area code has 3 unique variables, so we conert it into factor

train$area.code = as.factor(train$area.code)
test$area.code = as.factor(test$area.code)



# As the phone number is unique and cannot explain about the target variable,let us remove the column phone number
train$phone.number= NULL
test$phone.number= NULL

#Let's see the percentage of our target variable
round(prop.table(table(train$Churn))*100,2)
# False.   True. 
# 85.51   14.49 




################################ VISUALIZING THE DATA ################################
table(train$Churn)

library(ggplot2)
#Target class distribution 
ggplot(data = train,aes(x = Churn))+
  geom_bar() +  labs(y='Churn Count', title = 'Customer Churn or Not')
# WE CAN SEE THAT THERE IS A TARGET CLASS IMBALANCE PROBLEM

# Churning of customer according to State
ggplot(train, aes(fill=Churn, x=state)) +
  geom_bar(position="dodge") + labs(title="Churning ~ State")

# Churning of customer according to Voice Mail Plan
ggplot(train, aes(fill=Churn, x=voice.mail.plan)) +
  geom_bar(position="dodge") + labs(title="Churning ~ Voice Mail Plan")
#WE SEE THAT WHEN THE VOICE MAIL PLAN IS ACTIVE THERE ARE LESS CUSTOMERS CHURNING
round(prop.table(table(train$voice.mail.plan))*100,2)




# Churning of customer according to international plan
ggplot(train, aes(fill=Churn, x=international.plan)) +
  geom_bar(position="dodge") + labs(title="Churning ~ international plan")
#LET US SEE THE CLASS IMBALANCE
round(prop.table(table(train$international.plan))*100,2)
#WE SEE THAT THE 90% HAS NO INTERNATIONAL PLAN


# Churning of customer according to area_code
ggplot(train, aes(fill=Churn, x=area.code)) +
  geom_bar(position="dodge") + labs(title="Churning ~ Area Code")

#CUSTOMERS FROM AREA CODE 415 CHURNS MORE
round(prop.table(table(train$area.code))*100,2)

# Churn ~ international_plan by voice_mail_plan
ggplot(train, aes(fill=Churn, x=international.plan)) +
  geom_bar(position="dodge") + facet_wrap(~voice.mail.plan)+
  labs(title="Churn ~ international_plan by voice_mail_plan")


# churning of customer acording to customer service calls

ggplot(train, aes(fill=Churn, x=number.customer.service.calls)) +
  geom_bar(position="dodge") + labs(title="Churning ~ number customer service calls")




################################EDA################################


#Function for Assigning factors of var to levels
cat_to_num = function(df){
  for(i in 1:ncol(df)){
    if(class(df[,i]) == 'factor'){
      df[,i] = factor(df[,i],labels = (1:length(levels(factor(df[,i])))))
    }
  }
  return(df)
}

#Converting Categorical to level -> factors
train = cat_to_num(train)
test = cat_to_num(test)

str(train)

#getting all numeric varaibles together
num_index = sapply(train, is.numeric)
num_data = train[,num_index]
num_col = colnames(num_data) 

#getting all categorical variables together

cat_index=sapply(train, is.factor)
cat_data=train[,cat_index]
cat_col= colnames(cat_data)

table(train$Churn)


################################CHECKING MISSING VALUE################################

apply(train, 2, function(x) {sum(is.na(x))})     
apply(test, 2, function(x) {sum(is.na(x))}) 

#WE CAN SEE THAT THERE ARE NO MISSING VALUES IN THE GIVEN DATA


################################OUTLIER ANALYSIS################################
 

#let us do box plot to see the outlliers

library(ggplot2)

  for (i in 1:length(num_col))
  {
    assign(paste0("gn",i),
           ggplot(aes_string(y = (num_col[i]), x = 'Churn'),data = train) +
             stat_boxplot(geom = "errorbar", width = 0.5) +
             geom_boxplot(outlier.colour="blue", fill = "skyblue",
                          outlier.shape=18,outlier.size=1, notch=FALSE) +
             labs(y=num_col[i],x="Churn")+
             ggtitle(paste("Box plot of responded for",num_col[i])))
  }

 

 # Plotting plots together
 gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)
 gridExtra::grid.arrange(gn4,gn5,gn6,ncol=3)
 gridExtra::grid.arrange(gn7,gn8,gn9,ncol=3)
 gridExtra::grid.arrange(gn10,gn11,gn12,ncol=3)
 gridExtra::grid.arrange(gn13,gn14,gn15,ncol=3)
#
#  #Removing oulier by replacing with NA and then impute
  for(i in num_col){
    print(i)
    out = train[,i][train[,i] %in% boxplot.stats(train[,i])$out]
    print(length(out))
    train[,i][train[,i] %in% out] = NA
  }
#
 library(DMwR)
library(lattice)
library(grid)
  #checking all the missing values
 library(DMwR)
 sum(is.na(train))
 train = knnImputation(train, k=3)  

 
 str(test)

 ################################FEATURE SELECTION################################
 

#Here we will  find corelation betwwen variables

##Correlation plot
library(corrgram)

corrgram(train[,num_index],
         order = F,  #we don't want to reorder
         upper.panel=panel.pie,
         lower.panel=panel.shade,
         text.panel=panel.txt,
         main = 'CORRELATION PLOT')
#We can see var the highly corr related variables marked dark blue which means they are highly correlated. 



#chi Square test

for(i in cat_col){
  print(names(cat_data[i]))
  print((chisq.test(table(cat_data$Churn,cat_data[,i])))[3])  
}

##removing Highly Corelated and Independent variables
train = subset(train,select= -c(total.day.charge,total.eve.charge,
                                total.night.charge,total.intl.charge,state))

test = subset(test,select= -c(total.day.charge,total.eve.charge,
                                total.night.charge,total.intl.charge,state))


################################feature Scaling################################





#Checking Data of Continuous Variable



hist(train$total.day.calls)
hist(train$total.day.minutes)
hist(train$account.length)
#storing numeric variables
num_index = sapply(train, is.numeric)
num_data = train[,num_index]
num_col = colnames(num_data)



#Most of the data is uniformally distributed so we use standarizaion



num_col

for(i in num_col){
  print(i)
   train[,i] = (train[,i] - mean(train[,i]))/sd(train[,i])
   test[,i] = (test[,i] - mean(test[,i]))/sd(test[,i])
}


str(test)


################################SAMPLING OF DATA################################


# #Divide data into train and test using stratified sampling method

 library(caret)
set.seed(101)
split_index = createDataPartition(train$Churn, p = 0.66, list = FALSE)
trainset = train[split_index,]
testset  = train[-split_index,]

#Checking Train Set Target Class
table(trainset$Churn)

#we see that class is imbalanced so,we have to use under sampling and over sampling to have a balanced training set

library(ROSE) 

trainset = ROSE(Churn~.,data = trainset,p = 0.5,seed = 101)$data     

table(trainset$Churn) 

#now we see that data is balanced




################# MODEL DEVELOPEMENT##############################




# #function for calculating error metrics
calc = function(cm){
  TN = cm[1,1]
  FP = cm[1,2]
  FN = cm[2,1]
  TP = cm[2,2]
  print(paste0('Accuracy- ',((TN+TP)/(TN+TP+FN+FP))*100))
  print(paste0('FNR- ',((FN)/(TP+FN))*100))
  print(paste0('FPR- ',((FP)/(TN+FP))*100))
  print(paste0('precision-  ',((TP)/(TP+FP))*100)) 
  print(paste0('recall//TPR-  ',((TP)/(TP+FP))*100))
  print(paste0('Sensitivity-  ',((TP)/(TP+FN))*100))
  print(paste0('Specificity-  ',((TN)/(TN+FP))*100))
}



######################DECISION TREE##################################################

library(C50)
#Develop Model on training data
DT_model = C5.0(Churn ~., trainset, trials = 100, rules = TRUE)

#Summary of DT model
summary(DT_model)


#Lets predict for test cases
DT_Predictions = predict(DT_model, testset[,-15], type = "class")

##Evaluate the performance of classification model
Cm_DT = table(testset$Churn, DT_Predictions)
confusionMatrix(Cm_DT)

calc(Cm_DT)



# [1] "Accuracy :- 86.0547219770521"
# [1] "FNR :- 21.9512195121951"
# [1] "FPR :- 12.5902992776058"
# [1] "precision :-  51.2"
# [1] "recall//TPR :-  51.2"
# [1] "Sensitivity :-  78.0487804878049"
# [1] "Specificity :-  87.4097007223942"


#####################################RANDOM FOREST##############################################
library(randomForest)


set.seed(101)
RF_model = randomForest(Churn ~ ., trainset,importance=TRUE,ntree= 500,type='class')

RF_Predictions = predict(RF_model, testset[,-15])

##Evaluate 
cm_RF = table(testset$Churn,RF_Predictions)
confusionMatrix(cm_RF)
calc(cm_RF)
# 
# [1] "Accuracy :- 87.0255957634598"
# [1] "FNR :- 15.2439024390244"
# [1] "FPR :- 12.5902992776058"
# [1] "precision :-  53.2567049808429"
# [1] "recall//TPR :-  53.2567049808429"
# [1] "Sensitivity :-  84.7560975609756"
# [1] "Specificity :-  87.4097007223942"

##################################LOGISTIC REGRESSION ##############################################
set.seed(101)
lm_model = glm(Churn ~., data = trainset, family ="binomial") 
summary(lm_model)

lm_prediction = predict(lm_model,newdata = testset[,-15],type = 'response')

#Convert prob 
lm_prediction = ifelse(lm_prediction > 0.5, 2,1)

##Evaluate 
cm_lm = table(testset$Churn, lm_prediction)

calc(cm_lm)


# [1] "Accuracy :- 84.9955869373345"
# [1] "FNR :- 18.9024390243902"
# [1] "FPR :- 14.3446852425181"
# [1] "precision :-  48.8970588235294"
# [1] "recall//TPR :-  48.8970588235294"
# [1] "Sensitivity :-  81.0975609756098"
# [1] "Specificity :-  85.6553147574819"

####################################### KNN ###############################
set.seed(101)
## KNN impletation
library(class)

##Predicting Test data

knn_Prediction = knn(trainset[,1:14],testset[,1:14],trainset$Churn, k = 5)
#Confusion matrix
cm_knn = table(testset$Churn,knn_Prediction)
confusionMatrix(cm_knn)
calc(cm_knn)



# [1] "Accuracy :- 65.7546337157988"
# [1] "FNR :- 41.4634146341463"
# [1] "FPR :- 33.0237358101135"
# [1] "precision :-  23.0769230769231"
# [1] "recall//TPR :-  23.0769230769231"
# [1] "Sensitivity :-  58.5365853658537"
# [1] "Specificity :-  66.9762641898865"

#############################Naive Bayes#########################################
library(e1071)
set.seed(101)
#Model Development 
nb_model = naiveBayes(Churn ~., data = trainset, type = 'class')
#prediction
nb_prediction = predict(nb_model,testset[,1:14])

#Confusion matrix
cm_nb = table(testset[,15],nb_prediction)
confusionMatrix(cm_nb)
calc(cm_nb)

# [1] "Accuracy :- 81.8181818181818"
# [1] "FNR :- 29.8780487804878"
# [1] "FPR :- 16.2022703818369"
# [1] "precision :-  42.2794117647059"
# [1] "recall//TPR :-  42.2794117647059"
# [1] "Sensitivity :-  70.1219512195122"
# [1] "Specificity :-  83.797729618163"

####################################################################################################

# As according to out problem statement we need to find out the customer which will Move or Not.

# # So according to requirement we are selecting RandomForest as our Final model as under train data set in random forest model has better FNR,FPR and Accuracy.








set.seed(101)
final_model = randomForest(Churn~.,data = trainset,ntree=800,mtry=4,importance=TRUE,type = 'class')
final_validation_pred = predict(final_model,testset[,-15])
cm_final_valid = table(testset[,15],final_validation_pred)
confusionMatrix(cm_final_valid)
calc(cm_final_valid)


# [1] "Accuracy :- 86.4960282436011"
# [1] "FNR :- 17.0731707317073"
# [1] "FPR :- 12.8998968008256"
# [1] "precision :-  52.1072796934866"
# [1] "recall//TPR :-  52.1072796934866"
# [1] "Sensitivity :-  82.9268292682927"
# [1] "Specificity :-  87.1001031991744"



################ Final Prediction On original test Data set ##################################




set.seed(101)
final_test_pred = predict(final_model,test[,-15])
cm_final_test = table(test[,15],final_test_pred)
confusionMatrix(cm_final_test)
calc(cm_final_test)

# #Final Test Prediction
# [1] "Accuracy :- 85.9028194361128"
# [1] "FNR :- 15.625"
# [1] "FPR :- 13.8600138600139"
# [1] "FPR :- 13.8600138600139"
# [1] "precision :-  48.586118251928"
# [1] "recall//TPR :-  48.586118251928"
# [1] "Sensitivity :-  84.375"
# [1] "Specificity :-  86.1399861399861"




###############Saving output to file###########################



#adding a column of prediced output to the test data
test_Original$predicted_output = final_test_pred

#changing 1 and 0 back to yes and no in the target variable
test_Original$predicted_output = gsub(1,"False",test_Original$predicted_output)
test_Original$predicted_output = gsub(2,"True",test_Original$predicted_output)



write.csv(test_Original, "final_test_data.csv", row.names = F)
rm(list = ls())







