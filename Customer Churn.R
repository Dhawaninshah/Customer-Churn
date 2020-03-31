setwd("C:/Users/Dhawani/Desktop/22.08.19/Project")
getwd()



library(readxl)
library(psych)
library(car)
####################Q1EDA###########

mydata<-read_excel("Cellphone.xlsx", sheet = 2)
View(mydata)
dim(mydata)

head(mydata)
tail(mydata)
names(mydata)
str(mydata)
##All are numeric


attach(mydata)
data.frame(mydata)
class(mydata)



####CHECKING FOR MISSING DATA
sum(is.na(mydata))
rowSums(is.na(mydata))

colSums(is.na(mydata))

##No missing data
describe(mydata)


###univariate Analysis
plot(mydata, col="Blue")


churnrate<-table(mydata$Churn) / nrow(mydata)
churnrate
##over entire dataset , 14.49% of customers churned

## here churn is the response variable and all other are prediction variables

##graphs
boxplot(AccountWeeks, horizontal = TRUE, col="Blue", main = "Boxplot for Number of Weeks customer has had active account")

boxplot(CustServCalls, horizontal = TRUE, col="Blue", main = "Boxplot for Number of calls made to customer service")

boxplot(DayMins, horizontal = TRUE, col="Blue", main = "Boxplot for average day time minutes per month")

boxplot(DayCalls, horizontal = TRUE, col="Blue", main = "Boxplot for average number of daytime calls")

boxplot(OverageFee, horizontal = TRUE, col="Blue", main = "Boxplot for largest overage fees in last 12 months")

boxplot(RoamMins, horizontal = TRUE, col="Blue", main = "Boxplot for average number of Roaming Minutes")


table(mydata2$ContractRenewal)

library(rpivotTable)

rpivotTable(mydata2)
attach(mydata2)
##from pivot table , 1- churn yes no, 2- contract renewal yes no, 3- data plan yes noe

boxplot(CustServCalls~Churn, col="BLue", main = "Boxplot for Churn w.r.t Data Plan")

cor(CustServCalls,Churn)

by(mydata2,INDICES = Churn, FUN=summary)

###Regression
reg<-lm(Churn~.,data = mydata)
summary(reg)
vif(reg)

###VIF greater than 4 will be removed one by one

##removing monthly charge
mydata1<-mydata[!names(mydata) %in% c("MonthlyCharge")]
mydata1

dim(mydata1)

reg1<-lm(Churn~.,data = mydata1)
summary(reg1)
vif(reg1)

#removing data usage
mydata2<-mydata1[!names(mydata1) %in% c("DataUsage")]
mydata2

dim(mydata2)

reg2<-lm(Churn~.,data = mydata2)
summary(reg2)
vif(reg2)


###All VIF's are within range

###Removal of outliers and influential varaibles using cooks distance

cd = cooks.distance(reg2)
max(cd)
plot(cd, pch = "*", cex = 2, main = "Influential obs  by cooks distance")
#cutoff will be 0.5
#no need to remove any observation

View(mydata2)



library(corrplot)
corrplot(cor(mydata2), method="shade")




#=====================Question 2=================================
#Buidling Models=================================================


###Logistic Regression
Predict = predict(reg2)
data.frame(Churn, Predict)

##Step 1 : Working out log likelihood ratio test
library(lmtest)
logit= glm(Churn~.,data = mydata2, family = binomial)
lrtest((logit))
##pvalue is very low, less than 0.05 thus it is significant
#thus churn depends on the given variables

##Step 2: Rsquare computatoin and interpretation
install.packages("pscl")
library(pscl)
pR2(logit)
##As MCfadden is in range of 0.10-0.3 , it is good model
##uncertaininty of model 2 is explained by model 1
#model 2 is intercept model

#Step 3- Test of individual coefficients (betas) and interpretation
summary(logit)

##Step 4: Explanatory power of odd ratio
odds = exp(coef(logit))
odds
Probability = odds/(1+odds)
Probability

##Odds and its probability are important
#lets interpret each variable individually
##If attitute towards accountweeks increases by 1 unit then proability of churn increases by 1.00006666 times comapres to not churn keeping other variables cconstant
##If attituted towards churn increases by 1 unit then odds for accountweeks goes up by 50% proability of becoming churn keeping other variables constant

###so now we can say that all factors are significatn ffor predicting churning

##Step 5: Confusion matrix for measuring predictive accuracy
predict(logit, type="response")
##it is giving predicted value of churn for each record

#our aim is to get outcome 0 or 1 and not decimal
attach(mydata2)
Pred = fitted(logit)
data.frame(Churn, Pred)
gg1 = floor(Pred + 0.50)
gg1

table(Actual = Churn , Prediction = gg1)
##2781 + 89 = 2870 correct prediction and 463 wrong classificed
#thus accuracy is 2870/3333 = 86% accuracy 
##find accuracy  sensitivity and specificity from confusion matrix



##Step 6 : ROC Plot adn interpretation
##From logistic regression we get binary outcomes

library(ROCR)
pred1 = prediction(gg1, Churn)
perf1 = performance(pred1,"tpr", "fpr")
plot(perf1, col = "Blue", main = "ROC Plot")
abline(0,1, lty = 8, col = "red")
auc = performance(pred1, "auc")
auc = auc@y.values
auc

##auc is 0.58 

#=======KNN Method
#Choose value of k based on lowest error rate in validation data

##when k=n (that is entire dataset) is the same thing as "naive rule" (classify all recordes according to majority class)
#we use average response values in KNN


View(mydata2)
mydata3<-mydata2[, -1]

View(mydata3)
mydata4<-mydata3[,-2]
View(mydata4)
mydata5<-mydata4[,-2]
View(mydata5)

library(dplyr)
##step 1 normalise the data
norm = function(x) { (x-min(x))/(max(x)-min(x))}
norm.data = as.data.frame(lapply(mydata5,norm))

View(mydata5)
View(norm.data)

usable.data = cbind(mydata2[,1], norm.data)

str(usable.data)
View(usable.data)

# Data partitioning
library(caTools)
spl = sample.split(usable.data$Churn, SplitRatio = 0.7)
train = subset(usable.data, spl == T)
test = subset(usable.data, spl == F)
dim(train)
dim(test)

#Use KNN Classifier 
library(class)
pred = knn(train[-1], test[-1], train[,1], k=19)
table.knn = table(test[,1], pred)
table.knn  ##this is confusion matrix
sum(diag(table.knn))/sum(table.knn)



pred = knn(train[-1], test[-1], train[,1], k=7)
table.knn = table(test[,1], pred)
table.knn
sum(diag(table.knn))/sum(table.knn)

pred = knn(train[-1], test[-1], train[,1], k=15)
table.knn = table(test[,1], pred)
table.knn
sum(diag(table.knn))/sum(table.knn)

pred = knn(train[-1], test[-1], train[,1], k=13)
table.knn = table(test[,1], pred)
table.knn
sum(diag(table.knn))/sum(table.knn)

pred = knn(train[-1], test[-1], train[,1], k=17)
table.knn = table(test[,1], pred)
table.knn
sum(diag(table.knn))/sum(table.knn)
###Choose value of k which gives highest accuracy
##choose the optimum


##==================Naive Bayes


#Use NB Classifier 
library(e1071)

NB = naiveBayes(Churn~., data=train)


predNB = predict(NB, test, type = "class")
tab.NB = table(test[,1], pred)
tab.NB
sum(diag(tab.NB)/sum(tab.NB))

##Accuracy 87.4%




#====================Model Comparison==================
##Cross validation
library(dplyr)
folds <- createFolds(factor(train$Churn), k = 10, list = FALSE)
folds
folds <- createFolds(factor(train$Churn), k = 10, list = TRUE)
folds


library(caret)
library(klaR)
train_control = trainControl(method = "cv", number = 10)
modelKNN = train(as.factor(Churn)~., data=train , trControl = train_control, method = "knn")


summary(modelKNN)


print(modelKNN)


modelNB = train(as.factor(Churn)~., data=train, trControl = train_control, method = "nb")


summary(modelNB)


print(modelNB)

### collect resamples
results = resamples(list(KNN = modelKNN, NB = modelNB))


summary(results)


bwplot(results)

###KNN model has higher median in accuracy
##kappa is also mode in KNN then NB