rm(list=ls())
getwd()

#Load Required Libraries
library(ggplot2)
library(corrgram)
library(DMwR)
library(caret)
library(randomForest)
library(unbalanced)
library(dummies)
library(e1071)
library(Information)
library(MASS)
library(rpart)
library(gbm)
library(ROSE)
library(xlsx)
library(DataCombine)
library(rpart)

#set working directory

setwd("C:/Users/Esha Rudra")

#Load data
absent_data = read.xlsx("Absenteeism_at_work_Project.xls", sheetIndex = 1, header = TRUE)

#****************EXPLORATORY DATA ANALYSIS******************
#Dimension of the data
dim(absent_data)

#Structure of the data
str(absent_data)

#Lets check out the variable names of the data

colnames(absent_data)

# Separating Continuous & Categorical Variables

continuous_vars = c('Distance.from.Residence.to.Work', 'Service.time', 'Age',
                    'Work.load.Average.day.', 'Transportation.expense',
                    'Hit.target', 'Weight', 'Height', 
                    'Body.mass.index', 'Absenteeism.time.in.hours')

catagorical_vars = c('ID','Reason.for.absence','Month.of.absence','Day.of.the.week',
                     'Seasons','Disciplinary.failure', 'Education', 'Social.drinker',
                     'Social.smoker', 'Son', 'Pet')

#*************MISSING VALUE ANALYSIS**************

# creating a dataframe for missing values present in each vriable

missing_val = data.frame(apply(absent_data,2,function(x){sum(is.na(x))}))

#Convert row into column

missing_val$Columns = row.names(missing_val)

row.names(missing_val) = NULL

#Rename variable

names(missing_val)[1] =  "Missing_percentage"

#Calculate missing value percentage
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(absent_data)) * 100


#Sort missing value proportion in descending order
missing_val = missing_val[order(-missing_val$Missing_percentage),]

#Rearrange columns
missing_val = missing_val[,c(2,1)]

#lets view the dataframe
missing_val

# Save output result as csv file
write.csv(missing_val, "Missing_percentage_R.csv", row.names = F)

#Plot top 3 variables with missing values

ggplot(data = missing_val[1:3,], aes(x=reorder(Columns, -Missing_percentage),y = Missing_percentage))+
  geom_bar(stat = "identity",fill = "pink")+xlab("Variables")+
  ggtitle("Percentage of missing values") + theme_bw()

#Lets create missing values so that we can impute it with the best suitable method

#Check value
absent_data[95,20]

#so the data value in the above location is 32.

#Create missing value
absent_data[95,20] = NA

# Actual Value = 32
# Mean = 26.67655
# Median = 25
# KNN = 32

#Mean Method

#absent_data$Body.mass.index[is.na(absent_data$Body.mass.index)] = mean(absent_data$Body.mass.index, na.rm = T)

#check value

absent_data[95,20]

#Create missing value again in the same location


absent_data[95,20] = NA

#Median Method
#absent_data$Body.mass.index[is.na(absent_data$Body.mass.index)] = median(absent_data$Body.mass.index, na.rm = T)
#Check value
absent_data[95,20]

#Create missing value again and now lets try KNN method

absent_data[95,20] = NA

#KNN Imputation
absent_data = knnImputation(absent_data, k = 3)

#Check value
absent_data[95,20]

# So as we can see that KNN imputation give the most accurate result so we will freeze this method
# missing values imputation for the entire dataset.

#Lets check if all the missing values are imputed or not

sum(is.na(absent_data))

#*************OULIER ANALYSIS**************

# BoxPlots - Distribution and Outlier Check

# Boxplot for continuous variables
for (i in 1:length(continuous_vars)) {
  assign(paste0("gn",i), ggplot(aes_string(y = (continuous_vars[i]), x = "Absenteeism.time.in.hours"), data = subset(absent_data))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=continuous_vars[i],x="Absenteeism.time.in.hours")+
           ggtitle(paste("Box plot for outliers in absenteeism data variables",continuous_vars[i])))
}

# Plotting plots together
gridExtra::grid.arrange(gn1,gn2,ncol=2)
gridExtra::grid.arrange(gn3,gn4,ncol=2)
gridExtra::grid.arrange(gn5,gn6,ncol=2)
gridExtra::grid.arrange(gn7,gn8,ncol=2)
gridExtra::grid.arrange(gn9,gn10,ncol=2)

#Outlier removal using boxplot method
#loop to remove outliers from all variables
for(i in continuous_vars)
{
  print(i)
  #Extract outliers and store in val
  val = absent_data[,i][absent_data[,i] %in% boxplot.stats(absent_data[,i])$out]
  #Remove outliers and store cleaned data back in data
  absent_data = absent_data[which(!absent_data[,i] %in% val),]
}

#Replace all outliers with NA 
#for(i in continuous_vars)
{
  #Extract outliers and store in val
  #val = absent_data[,i][absent_data[,i] %in% boxplot.stats(absent_data[,i])$out]
  #Replace outliers with NA
  #absent_data[,i][absent_data[,i] %in% val] = NA
}

# Imputing missing values using KNN 
#absent_data = knnImputation(absent_data, k=3)  

#**********FEATURE SELECTION********


#Correlation Plot for continuous variables
corrgram(absent_data[,continuous_vars], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#ANOVA test for Categprical variable
summary(aov(formula = Absenteeism.time.in.hours~ID,data = absent_data))
summary(aov(formula = Absenteeism.time.in.hours~Reason.for.absence,data = absent_data))
summary(aov(formula = Absenteeism.time.in.hours~Month.of.absence,data = absent_data))
summary(aov(formula = Absenteeism.time.in.hours~Day.of.the.week,data = absent_data))
summary(aov(formula = Absenteeism.time.in.hours~Seasons,data = absent_data))
summary(aov(formula = Absenteeism.time.in.hours~Disciplinary.failure,data = absent_data))
summary(aov(formula = Absenteeism.time.in.hours~Education,data = absent_data))
summary(aov(formula = Absenteeism.time.in.hours~Social.drinker,data = absent_data))
summary(aov(formula = Absenteeism.time.in.hours~Social.smoker,data = absent_data))
summary(aov(formula = Absenteeism.time.in.hours~Son,data = absent_data))
summary(aov(formula = Absenteeism.time.in.hours~Pet,data = absent_data))

# Dimension Reduction
absent_data = subset(absent_data, select = -c(Weight))

#--------------------------------Feature Scaling--------------------------------#

#Check Normality of target variable
qqnorm(absent_data$Absenteeism.time.in.hours)
hist(absent_data$Absenteeism.time.in.hours)

#Check range of target variable
range(absent_data$Absenteeism.time.in.hours)

#Updating the continuous variable for further processing
continuous_vars = c('Distance.from.Residence.to.Work', 'Service.time', 'Age',
                    'Work.load.Average.day.', 'Transportation.expense',
                    'Hit.target', 'Height', 
                    'Body.mass.index','Absenteeism.time.in.hours')

orig_data = absent_data    #stage-1 backup

# Normalization
for(i in continuous_vars)
{
  print(i)
  absent_data[,i] = (absent_data[,i] - min(absent_data[,i]))/(max(absent_data[,i])-min(absent_data[,i]))
}

df1=absent_data   #stage-2 backup

#Check range of target variable
range(absent_data$Absenteeism.time.in.hours)

#Create back up of data
absent_backup = absent_data

#Creating dummy data for categorical variables
library(mlr)
absent_data = dummy.data.frame(absent_data, catagorical_vars)

#**************Sampling*******************
#Stratified sampling method
#Divide data into train and test 
set.seed(123)
train.index = createDataPartition(absent_data$Absenteeism.time.in.hours, p = .80, list = FALSE)
absent_train = absent_data[ train.index,]
absent_test  = absent_data[-train.index,]

#*************Decision tree for classification**************
#Develop Model on training data
fit_DT = rpart(Absenteeism.time.in.hours ~., data = absent_train, method = "anova")

#Summary of DT model
summary(fit_DT)


#write rules into disk
write(capture.output(summary(fit_DT)), "absent_data_rules.txt")

#Lets predict for train data
pred_DT_train = predict(fit_DT, absent_train[,names(absent_test) != "Absenteeism.time.in.hours"])

#Lets predict for test data
pred_DT_test = predict(fit_DT,absent_test[,names(absent_test) != "Absenteeism.time.in.hours"])

#Error metrics
#For training data 
print(postResample(pred = pred_DT_train, obs = absent_train[,107]))

#For testing data 
print(postResample(pred = pred_DT_test, obs = absent_test[,107]))


#*************LINEAR REGRESSION*************

set.seed(123)

#Develop Model on training data
fit_LR = lm(Absenteeism.time.in.hours ~ ., data = absent_train)

#Lets predict for train data
pred_LR_train = predict(fit_LR, absent_train[,names(absent_test) != "Absenteeism.time.in.hours"])

#Lets predict for test data
pred_LR_test = predict(fit_LR,absent_test[,names(absent_test) != "Absenteeism.time.in.hours"])

#Error Metrics
# For training data 
print(postResample(pred = pred_LR_train, obs = absent_train[,107]))

# For testing data 
print(postResample(pred = pred_LR_test, obs = absent_test[,107]))

#***********Random Forest***********

set.seed(123)

#Develop Model on train data
fit_RF = randomForest(Absenteeism.time.in.hours~., data = absent_train)

#Lets predict for training data
pred_RF_train = predict(fit_RF, absent_train[,names(absent_test) != "Absenteeism.time.in.hours"])

#Lets predict for test data
pred_RF_test = predict(fit_RF,absent_test[,names(absent_test) != "Absenteeism.time.in.hours"])

# For training data 
print(postResample(pred = pred_RF_train, obs = absent_train[,107]))

# For testing data 
print(postResample(pred = pred_RF_test, obs = absent_test[,107]))


#************************XGBoost***********************

set.seed(123)

#Develop Model on training data
fit_XGB = gbm(Absenteeism.time.in.hours~., data = absent_train, n.trees = 500, interaction.depth = 2)

#Lets predict for train data
pred_XGB_train = predict(fit_XGB, absent_train[,names(absent_test) != "Absenteeism.time.in.hours"], n.trees = 500)

#Lets predict for test data
pred_XGB_test = predict(fit_XGB,absent_test[,names(absent_test) != "Absenteeism.time.in.hours"], n.trees = 500)

# For training data 
print(postResample(pred = pred_XGB_train, obs = absent_train[,107]))

# For testing data 
print(postResample(pred = pred_XGB_test, obs = absent_test[,107]))


#****************Dimensionality Reduction using PCA***************

#Principal Component Analysis
absent_pca = prcomp(absent_train)

#compute standard deviation of each principal component
absent_stddev = absent_pca$sdev

#compute variance
absent_var = absent_stddev^2

#proportion of variance explained
prop_var = absent_var/sum(absent_var)

#Plot
plot(cumsum(prop_var), xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

#add a training set with principal components
train.data = data.frame(Absenteeism.time.in.hours = absent_train$Absenteeism.time.in.hours, absent_pca$x)

# From the above plot selecting 45 components since it explains almost 95+ % data variance
train.data =train.data[,1:45]

#transform test into PCA
test.data = predict(absent_pca, newdata = absent_test)
test.data = as.data.frame(test.data)

#select the first 45 components
test.data=test.data[,1:45]


#************************Model Development after Dimensionality Reduction************************


##Decision tree for classification
#Develop Model on training data
fit_DT = rpart(Absenteeism.time.in.hours ~., data = absent_train, method = "anova")

#Summary of DT model
summary(fit_DT)

#write rules into disk
write(capture.output(summary(fit_DT)), "absent_data_rules.txt")

#Lets predict for train data
pred_DT_train = predict(fit_DT, absent_train[,names(absent_test) != "Absenteeism.time.in.hours"])

#Lets predict for test data
pred_DT_test = predict(fit_DT,absent_test[,names(absent_test) != "Absenteeism.time.in.hours"])

#Error metrics
#For training data 
print(postResample(pred = pred_DT_train, obs = absent_train[,107]))

#For testing data 
print(postResample(pred = pred_DT_test, obs = absent_test[,107]))


#*************LINEAR REGRESSION*************
set.seed(123)

#Develop Model on training data
fit_LR = lm(Absenteeism.time.in.hours ~ ., data = absent_train)

#Lets predict for train data
pred_LR_train = predict(fit_LR, absent_train[,names(absent_test) != "Absenteeism.time.in.hours"])

#Lets predict for test data
pred_LR_test = predict(fit_LR,absent_test[,names(absent_test) != "Absenteeism.time.in.hours"])

#Error Metrics
# For training data 
print(postResample(pred = pred_LR_train, obs = absent_train[,107]))

# For testing data 
print(postResample(pred = pred_LR_test, obs = absent_test[,107]))


#***********Random Forest***********

set.seed(123)

#Develop Model on train data
fit_RF = randomForest(Absenteeism.time.in.hours~., data = absent_train)

#Lets predict for training data
pred_RF_train = predict(fit_RF, absent_train[,names(absent_test) != "Absenteeism.time.in.hours"])

#Lets predict for test data
pred_RF_test = predict(fit_RF,absent_test[,names(absent_test) != "Absenteeism.time.in.hours"])

# For training data 
print(postResample(pred = pred_RF_train, obs = absent_train[,107]))

# For testing data 
print(postResample(pred = pred_RF_test, obs = absent_test[,107]))


#************************XGBoost***********************

set.seed(123)

#Develop Model on training data
fit_XGB = gbm(Absenteeism.time.in.hours~., data = absent_train, n.trees = 500, interaction.depth = 2)

#Lets predict for train data
pred_XGB_train = predict(fit_XGB, absent_train[,names(absent_test) != "Absenteeism.time.in.hours"], n.trees = 500)

#Lets predict for test data
pred_XGB_test = predict(fit_XGB,absent_test[,names(absent_test) != "Absenteeism.time.in.hours"], n.trees = 500)

# For training data 
print(postResample(pred = pred_XGB_train, obs = absent_train[,107]))

# For testing data 
print(postResample(pred = pred_XGB_test, obs = absent_test[,107]))

#*******************Visualisations*******************

library(ggplot2)
#barplot

#Education
ggplot(absent_backup, aes_string(x = absent_backup$Education,y = absent_data$Absenteeism.time.in.hours)) +
  geom_bar(stat="identity",fill =  "pink") + theme_bw() +  xlab("Education") + ylab('absenteeism')

#Id
ggplot(absent_backup, aes_string(x = absent_backup$ID,y = absent_data$Absenteeism.time.in.hours)) +
  geom_bar(stat="identity",fill =  "pink") + theme_bw() +  xlab("ID") + ylab('absenteeism')

#Social Smoker
ggplot(absent_backup, aes_string(x = absent_backup$Social.smoker,y = absent_data$Absenteeism.time.in.hours)) +
  geom_bar(stat="identity",fill =  "purple") + theme_bw() +  xlab("Social.smoker") + ylab('absenteeism')

#Reason for absence
ggplot(absent_backup, aes_string(x = absent_backup$Reason.for.absence,y = absent_data$Absenteeism.time.in.hours)) +
  geom_bar(stat="identity",fill =  "blue") + theme_bw() +  xlab("Reason.for.absence") + ylab('absenteeism')

#Distance from Residence to Work
ggplot(absent_backup, aes_string(x = absent_backup$Distance.from.Residence.to.Work,y = absent_data$Absenteeism.time.in.hours)) +
  geom_bar(stat="identity",fill =  "blue") + theme_bw() +  xlab("Distance.from.Residence.to.Work") + ylab('absenteeism')

#Age
ggplot(absent_backup, aes_string(x = absent_backup$Age,y = absent_data$Absenteeism.time.in.hours)) +
  geom_bar(stat="identity",fill =  "blue") + theme_bw() +  xlab("Age") + ylab('absenteeism')

#Day of the week
ggplot(absent_backup, aes_string(x = absent_backup$Day.of.the.week,y = absent_data$Absenteeism.time.in.hours)) +
  geom_bar(stat="identity",fill =  "blue") + theme_bw() +  xlab("Day.of.the.week") + ylab('absenteeism')

#Disciplinary failure
ggplot(absent_backup, aes_string(x = absent_backup$Disciplinary.failure,y = absent_data$Absenteeism.time.in.hours)) +
  geom_bar(stat="identity",fill =  "blue") + theme_bw() +  xlab("Disciplinary.failure") + ylab('absenteeism')

#Hit target
ggplot(absent_backup, aes_string(x = absent_backup$Hit.target,y = absent_data$Absenteeism.time.in.hours)) +
  geom_bar(stat="identity",fill =  "blue") + theme_bw() +  xlab("Hit.target") + ylab('absenteeism')

#Month of absence
ggplot(absent_backup, aes_string(x = absent_backup$Month.of.absence,y = absent_data$Absenteeism.time.in.hours)) +
  geom_bar(stat="identity",fill =  "blue") + theme_bw() +  xlab("Month of absence") + ylab('absenteeism')

#Pet
ggplot(absent_backup, aes_string(x = absent_backup$Pet,y = absent_data$Absenteeism.time.in.hours)) +
  geom_bar(stat="identity",fill =  "blue") + theme_bw() +  xlab("Pet") + ylab('absenteeism')

#Seasons
ggplot(absent_backup, aes_string(x = absent_backup$Seasons,y = absent_data$Absenteeism.time.in.hours)) +
  geom_bar(stat="identity",fill =  "blue") + theme_bw() +  xlab("Seasons") + ylab('absenteeism')

#Service time
ggplot(absent_backup, aes_string(x = absent_backup$Service.time,y = absent_data$Absenteeism.time.in.hours)) +
  geom_bar(stat="identity",fill =  "blue") + theme_bw() +  xlab("Service.time") + ylab('absenteeism')

#Social drinker
ggplot(absent_backup, aes_string(x = absent_backup$Social.drinker,y = absent_data$Absenteeism.time.in.hours)) +
  geom_bar(stat="identity",fill =  "blue") + theme_bw() +  xlab("Social.drinker") + ylab('absenteeism')

#Transportation expense
ggplot(absent_backup, aes_string(x = absent_backup$Transportation.expense,y = absent_data$Absenteeism.time.in.hours)) +
  geom_bar(stat="identity",fill =  "blue") + theme_bw() +  xlab("Transportation.expense") + ylab('absenteeism')

#Work load Average/day
ggplot(absent_backup, aes_string(x = absent_backup$Work.load.Average.day.,y = absent_data$Absenteeism.time.in.hours)) +
  geom_bar(stat="identity",fill =  "blue") + theme_bw() +  xlab("Work load Average/day") + ylab('absenteeism')


#####################################2010##########################################
####Looking at models performance, we can say that 'Random forest' is the best model based on RMSE value
##TEST DATA PREDICTIONS FOR YEAR 2010
Absent_prediction=df1[-train.index,]
Absent_prediction$Predicted.Absent.hours=pred_RF_test
head(Absent_prediction)  #Sample output(with actual counts and predicted counts)

#Predicted absence hours of 2010
sum(Absent_prediction$Predicted.Absent.hours)

#Actual absence hours of 2010
sum(Absent_prediction$Absenteeism.time.in.hours)


######################## PREDICTIONS FOR YEAR 2011 #########################################
#data for 2011
#Service and Age will be added by 1

data_2011 = absent_backup
data_2011$Service.time = data_2011$Service.time + 1
data_2011 = subset(data_2011, select = -c(Absenteeism.time.in.hours))

cnames=continuous_vars[-9] 

# Normalization
for(i in cnames)
{
  print(i)
  data_2011[,i] = (data_2011[,i] - min(data_2011[,i]))/(max(data_2011)-min(absent_data[,i]))
}


#Creating dummy data for categorical variables
library(mlr)
emp_2011 = dummy.data.frame(data_2011, catagorical_vars)


#Using PCA
prin_comp = prcomp(emp_2011)
emp_2011 = data.frame( prin_comp$x)
emp_2011 =emp_2011[,1:45]  # selecting 45 components since it explains almost 95+ % data variance



#predicting the 2011 model
predict_2011_absence = predict(fit_RF,emp_2011)

#Absent prediction 2011
Predit_2011=data_2011
Predit_2011$Absent_hours_2011=predict_2011_absence
head(Predit_2011)

#Predicted absence hours per month of 2011
monthly_absence =  ddply(Predit_2011, c("Month.of.absence"), function(x) colSums(x[c("Absent_hours_2011")]))
monthly_absence

###*******************MONTHLY LOSSES PREDICTED FOR YEAR 2011 PER MONTH*************##########

#In a month excluding weekend 22 days are working days.
#there are 36 employee in the xyz company            
#8 hoursof work per day 
Total_Monthly_hours = 22*8*36

# total losses % = (absent_hours / Total_Monthly_hours)*100
monthly_absence$monthly_loss_percentage = (monthly_absence$Absent_hours_2011 /Total_Monthly_hours) * 100
print(monthly_absence)


#saving output results
write.csv(Predit_2011,"R_Employee_Absenteeism_2011.csv",row.names = FALSE)   
write.csv(monthly_absence,"R_Monthly_loss.csv",row.names = FALSE)


