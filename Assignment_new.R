#Setting the right working directory:
setwd("C:/Users/ADMIN/Desktop/GreatLakes/Data Mining")

#Importing the HR Employee Attrition Data file:
dataset=read.csv("HR_Employee_Attrition_Data.csv")

#Exploratory Data Analysis:
str(dataset)
summary(dataset)

#Plots:
library(ggplot2)
g <- ggplot(dataset)

#Attrition:
g+geom_bar(aes(Attrition,fill=Attrition))
#People in the company are 5 times more than the people leaving the company

#Yearssincelastpromotion vs Attrition
g+geom_bar(aes(YearsSinceLastPromotion,fill=Attrition))
#People recently promoted quit the company more than the ones not promoted

#YearsWithCurrentManager vs Attrition:
g+geom_bar(aes(YearsWithCurrManager,fill=Attrition))
#As the number of years with Current Manager increases, Attrition decreases

#TrainingTimeLastYear vs Attrition:
g+geom_bar(aes(TrainingTimesLastYear,fill=Attrition))
#Attrition seen in employees trained betweeb 2-4times last year.

#YearsatCompany vs Attrition
g+geom_point(aes(YearsAtCompany,Attrition,size=YearsAtCompany))
#People with less no of years tend to quit the company more.

#TotalWorking Years vs Attrition
g+geom_bar(aes(TotalWorkingYears,fill=Attrition))
#People with less Experience are leaving the job more.

#Present Salary Hike vs Attrition
g+(aes(PercentSalaryHike,Attrition))+geom_point(alpha=0.01)
#People with Less Percent Hike leave the company.

#OverTime vs Attrition
g+geom_bar(aes(OverTime,fill=Gender,colour=Attrition))
#Male employees working overtime leave the company more

#WorkLifeBalance vs Attrition
g+geom_bar(aes(WorkLifeBalance,fill=Attrition))
#People with better work life balance may tend to quit more.

#Marital Status vs Attrition
g+geom_bar(aes(MaritalStatus,fill=Attrition))
#Attrtion higest in Employees who are single,medium in Employees who are married and least in Divorced Employees.

#JObRole vs Attrition
g+(aes(JobRole))+geom_bar(aes(fill=Attrition))
#Job ROle of Sales Representative has the most attrition in various job roles present.

#JobInvolvement vs Attrition
g+(aes(JobInvolvement))+geom_bar(aes(fill=Attrition))
#People leaving the company are highly involved in their jobs

#JobSatisfaction vs Attrition:
g+(aes(JobSatisfaction))+geom_bar(aes(fill=Attrition))
#Low JobSatisfaction results in people leaving the company.

#StockOptionLevels vs Attrition:
g+(aes(StockOptionLevel))+geom_bar(aes(fill=Attrition))
#Attrition high in people with No or Less Stock Options 

#Gender vs Attrition
g+geom_bar(aes(Attrition,fill=Gender))
#More in Male employees

#DistanceFromHome vs Attrition
g+geom_histogram(binwidth=40,aes(DistanceFromHome,fill=Attrition))
#People living in shorter distances from office leave more.

#Hourly,Daily & Monthly Rates vs Attrition:
g+geom_point(aes(DailyRate,Attrition),alpha = 0.05)
g+geom_point(aes(HourlyRate,Attrition),alpha = 0.05)
g+geom_point(aes(MonthlyRate,Attrition),alpha = 0.05)

#Education Field, Education vs Attrition
g+geom_bar(aes(Education,fill=Attrition))
g+geom_bar(aes(EducationField,fill=Attrition))
#Attrtion seems to be higher in Bachelors, Life Sciences and Medical

#Department vs Attrition
g+aes(Department,fill=Attrition)+geom_density(position = "stack")
#People from Reasearch&Development and Sales tend to quit more compared to HR

#Business Travel vs Attrition
g+geom_bar(aes(BusinessTravel,fill=Attrition))
#Attrition is directly proportional to Travel among the employees.

#Age vs Attrition
g+geom_histogram(binwidth = 10,aes(Age,fill=Attrition),colour="Black")
#Employees around 30years leave the company more

#Others:
g+geom_bar(aes(OverTime,fill=Attrition))+facet_grid(.~JobRole,scales = "free")
#High Attrition among Sales Representatives and Lab Technicians who work Overtime.

#Reordering the dataset:
dataset<-dataset[c(2,1,4,6,7,9,10,11,13,14,15,17,19,20,21,24:35,3,5,8,12,16,18,23)]

#Checking for Correlation between variables:
library(corrplot)
library(psych)

corrplot(cor(dataset[,2:27]))

#Removing Unnecessary and correlated columns:
# 1.Over18 field can be calculated from the Age column:
dataset$Over18 <- NULL
#Employee Count and Employee Number fields do not carry any useful information:
dataset$EmployeeCount <-NULL
dataset$EmployeeNumber <-NULL
#Standard Hours is same across all the columns:
dataset$StandardHours<-NULL
#From Correlation table:
dataset$YearsAtCompany <-NULL
dataset$YearsInCurrentRole<-NULL
dataset$YearsSinceLastPromotion<-NULL
dataset$MonthlyIncome<-NULL
dataset$PerformanceRating<-NULL

#Target Varaible:
dataset$Attrition <- ifelse(dataset$Attrition =="No",0,1)

#Splitting the data into Training and Test sets:
library(caTools)
set.seed(123)
split=sample.split(dataset$Attrition,SplitRatio =0.7)
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)

#Hypothesis:F-Statistic is significant which shows regression is significant i.e. Dependent varaible is dependant on the given independent varaibles in
regressor<-lm(formula=training_set$Attrition~.,
              data=training_set)
summary(regressor)

#CART Model:
## setting the control paramter inputs for rpart:
library(rpart)
library(rpart.plot)
ctrl = rpart.control(minsplit=100, minbucket = 10, cp = 0, xval = 10)
destree <- rpart(formula = training_set$Attrition ~ ., 
            data = training_set[,2:26], method = "class", 
            control =ctrl)

rpart.plot(destree)
fancyRpartPlot(destree)

printcp(destree)
plotcp(destree)

#Predicting the test set results:
ypred1 <- predict(destree, test_set[,2:26], type="prob")
ypred1<-as.data.frame(ypred1)
ycm1<-as.data.frame(ypred1)
ypred1<-ifelse(ypred1$`1`>ypred1$`0`,ypred1$`1`,ypred1$`0`)
y_cm1<-ifelse(ycm1$`1`>ycm1$`0`,1,0)

#Confusion Matrix:
cm1=table(test_set[,1],y_cm1)

#Hypothesis:
#We stop when the Null hypothesis-independence between any of the input variables and the response variable cannot be rejected

#Encoding Categorical variables as factors:
train_nn<-training_set
test_nn<-test_set
train_nn$BusinessTravel <- as.numeric(factor(train_nn$BusinessTravel,
                                             levels=c('Non-Travel','Travel_Rarely','Travel_Frequently'),
                                             labels=c(1,2,3)))
train_nn$EducationField <- as.numeric(factor(train_nn$EducationField,
                                             levels=c('Human Resources','Life Sciences','Medical','Marketing','Technical Degree','Other'),
                                             labels=c(1,2,3,4,5,6)))
train_nn$Department <- as.numeric(factor(train_nn$Department,
                                         levels=c('Sales','Research & Development','Human Resources'),
                                         labels=c(1,2,3)))
train_nn$Gender <- as.numeric(factor(train_nn$Gender,
                                     levels=c('Female','Male'),
                                     labels=c(1,2)))
train_nn$JobRole <- as.numeric(factor(train_nn$JobRole,
                                      levels=c('Healthcare Representative','Human Resources','Laboratory Technician','Manager','Manufacturing Director','Research Director','Research Scientist','Sales Executive','Sales Representative'),
                                      labels=c(1,2,3,4,5,6,7,8,9)))
train_nn$MaritalStatus <- as.numeric(factor(train_nn$MaritalStatus,
                                             levels=c('Married','Single','Divorced'),
                                             labels=c(1,2,3)))
train_nn$OverTime <- as.numeric(factor(train_nn$OverTime,
                                       levels=c('Yes','No'),
                                       labels=c(1,2)))
test_nn$BusinessTravel <- as.numeric(factor(test_nn$BusinessTravel,
                                            levels=c('Non-Travel','Travel_Rarely','Travel_Frequently'),
                                            labels=c(1,2,3)))
test_nn$EducationField <- as.numeric(factor(test_nn$EducationField,
                                            levels=c('Human Resources','Life Sciences','Medical','Marketing','Technical Degree','Other'),
                                            labels=c(1,2,3,4,5,6)))
test_nn$Department <- as.numeric(factor(test_nn$Department,
                                        levels=c('Sales','Research & Development','Human Resources'),
                                        labels=c(1,2,3)))
test_nn$Gender <- as.numeric(factor(test_nn$Gender,
                                    levels=c('Female','Male'),
                                    labels=c(1,2)))
test_nn$JobRole <- as.numeric(factor(test_nn$JobRole,
                                     levels=c('Healthcare Representative','Human Resources','Laboratory Technician','Manager','Manufacturing Director','Research Director','Research Scientist','Sales Executive','Sales Representative'),
                                     labels=c(1,2,3,4,5,6,7,8,9)))
test_nn$MaritalStatus <- as.numeric(factor(test_nn$MaritalStatus,
                                           levels=c('Married','Single','Divorced'),
                                           labels=c(1,2,3)))
test_nn$OverTime <- as.numeric(factor(test_nn$OverTime,
                                      levels=c('Yes','No'),
                                      labels=c(1,2)))

#For xgboost:
train_x<-train_nn
test_x<-test_nn

#Feature Scaling:
train_nn[,2:26]=scale(train_nn[,2:26])
test_nn[,2:26]=scale(test_nn[,2:26])

#Neural Network 1:
library(h2o)
h2o.init(nthreads = -1)
feature_names<-names(train_nn)[2:26]
y1="Attrition"
train_h2o<-as.h2o(train_nn)
test_h2o<-as.h2o(test_nn)

train_h2o[,y1]<-as.factor(as.h2o(train_nn[,y1]))
test_h2o[,y1]<-as.factor(as.h2o(test_nn[,y1]))

nn1<-h2o.deeplearning(x=feature_names,
                             y=y1,
                             training_frame = train_h2o,
                             activation = 'Rectifier',
                             hidden=c(13,13),
                             epochs = 100,
                             ignore_const_cols = FALSE,
                             stopping_metric = "logloss",
                             train_samples_per_iteration = -2)


#Predicting the test set results:
prob_pred=h2o.predict(nn1,newdata = test_h2o)
prob_pred<-as.data.frame(prob_pred)
probs<-ifelse(prob_pred$p1>prob_pred$p0,prob_pred$p1,prob_pred$p0)

#Confusion Matrix:
cm3<-table(test_set[ ,1],prob_pred$predict)

h2o.shutdown()

#Neural Network 2:                      
library(neuralnet)
nn2 <- neuralnet(formula =train_nn$Attrition ~ Age  + BusinessTravel +	DailyRate +	Department+
                 DistanceFromHome +Education +EducationField +EnvironmentSatisfaction +
                 Gender +	HourlyRate +	JobInvolvement +JobLevel +JobRole +JobSatisfaction +
                 MaritalStatus +	MonthlyRate +	NumCompaniesWorked +OverTime +
                 PercentSalaryHike +RelationshipSatisfaction+	StockOptionLevel+TotalWorkingYears+
                 TrainingTimesLastYear+	WorkLifeBalance	+	YearsWithCurrManager,
                 data = train_nn[ ,2:26], 
                 hidden = 2,
                 err.fct = "sse",
                 linear.output = FALSE,
                 lifesign = "full",
                 lifesign.step = 10,
                 threshold = 0.1,
                 stepmax = 2000)

plot(nn2)

#Predicting test set results:
ypred3<- compute(nn2,test_nn[ ,2:26])$net.result
y_cm4<-ifelse(ypred3>0.5,1,0)

#Confusion Matrix:
cm4<-table(test_nn[ ,1],y_cm4)

#Optimization-Random Forest,Averaging & XGBOOST:

#1.Averaging:Combining Decision Tree and Neural Network 2:
ensembleavg<-(ypred1+ypred3)/2
test_set$ensembleavg<-ensembleavg
ypredavg<-(ifelse(test_set$ensembleavg>0.5,1,0))

#Confusion Matrix:
cm_avg<-table(test_set[ ,1],ypredavg)

#2.Using xgboost:
library(lattice)
library(caret)
library(readr)
library(stringr)
library(car)
library(xgboost)

classifier<-xgboost(data=as.matrix(train_x[ ,2:26]),
                    label = train_x$Attrition,
                    max_depth=5,
                    objective="binary:logistic",
                    eval_metric="auc",
                    nrounds=10)

#Predicting on test set:
y_xgb<-predict(classifier,as.matrix(test_x[ ,2:26]))

#Confusion Matrix:
y_cm5<-ifelse(y_xgb>0.5,1,0)
cm5<-table(test_x[ ,1],y_cm5)

#4.RandomForest:
#Fininding optimal mtry values:
library(randomForest)
tclassifier <- tuneRF(x = training_set[,2:26],
                      y=training_set$Attrition, 
                      mtryStart = 3, 
                      ntreeTry=100,
                      stepFactor = 1.5, 
                      improve = 0.0001, 
                      trace=TRUE, 
                      plot = TRUE, 
                      doBest = TRUE, 
                      nodesize = 10, 
                      importance=TRUE )
#Optimal mtry Value=13
set.seed(1234)
rantree = randomForest(x = training_set[,2:26],
                       y = training_set$Attrition,
                       ntree = 100,
                       nodesize = 10,
                       mtry=13,
                       importance = TRUE)
print(rantree)

# Predicting the Test set results
y_pred2 = predict(rantree, newdata = test_set[,2:26],type="class")
y_cm2<-ifelse(y_pred2>0.5,1,0)

#Confusion Matrix:
cm2=table(test_set[,1],y_cm2)


#Comparision of CART,Neural Network and Ensemble Model:
#1.Accuracy from Confusion Matrix:(Correct Predictions/Total predictions)*100

#2:Classification Error from Confusion Matrix:(FalsePositives+FalseNegatives/Total predictions)*100

#3.Others:
#Gini:
library(ineq)
gini_destree=ineq(ypred1, type="Gini")
gini_rantree=ineq(y_pred2,type="Gini")
gini_nn=ineq(ypred3,type="Gini")
gini_en=ineq(ypredavg,type="Gini")
gini_nn1=ineq(probs,type="Gini")

#Lorenz curve:
plot(Lc(ypred1))
plot(Lc(y_pred2))
plot(Lc(ypred3))
plot(Lc(probs))
plot(Lc(ypredavg))


