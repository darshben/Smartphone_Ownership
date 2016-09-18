```{r}
#Install packages#
library(adabag)
library(arules)
library(arulesViz)
library(car)
library(caret)
library(class)
library(cluster)
library(doSNOW)
library(dummies)
library(e1071)
library(fastcluster)
library(fpc)
library(ISLR)
library(lmtest)
library(MASS)
library(nnet)
library(randomForest)
library(ROCR)
library(rpart)
library(rpart.plot)

#Import Bank dataset#
Cell <- read.csv("~/downloads/January-2013--Mobile-Shopping-(omnibus)/Omnibus_Jan_2013_csv.csv")
Cell<-Cell[2:25]
Cell$Model_ <- Cell$Model
CellN <- dummy.data.frame(Cell, Cell$Model_)
Cell$pial2 <- factor(Cell$pial2)

#Basic Views of the data#
dim(Cell) #shows the dimensions of the data
names(Cell) #shows the names of each variable/attribute
summary(Cell) #summary of each variable/attribute
str(Cell) #To see the structure of the dataset and variables

#Exploratory Data Analysis#
boxplot(CellN$pial2) #boxplot
cor(Cell$sex, Cell$employ) #correlation of two variables
cov(Cell$employ, Cell$race) #covariance of two variables
fivenum(Cell$weight) #minimum, lower-hinge, median, upper-hinge, maximum)
head(Cell$age) #summary of continuous variable 
head(Cell$educ2) #summary of continuous variable 
hist(CellN$inc) #histogram of race
hist(Cell$age) #histogram of age
summary(Cell$inc) #summary of continuous variable 
table(Cell$pial2) #this is a 3 level factor

#Test/Train# 70% in Training set
set.seed(99)
Cell_rand <- Cell[order(runif(1003)), ]
Cell_train <- Cell_rand[1:700, ]
Cell_test  <-Cell_rand[701:1003, ]

#See the dimensions of the traning and validation sets
dim(Cell_train) #should have 70%
dim(Cell_test) #should have 30%

# check the proportion of pial2
prop.table(table(Cell_train$pial2))
prop.table(table(Cell_test$pial2))

#Multilinear Model
cell_model <- lm(pial2~pial1c+age+educ2+inc,data=CellN) #set model to be a linear function of all remaining variables#
cell_model #Coefficient of linear model 1#
summary(cell_model) #summary of linear model 1#
names(cell_model) #components in the SLRM
coef(cell_model) #shows coeffients only
confint(cell_model) #shows confidence intervals
cell_model2 <- lm(pial2~pial3b^2+pial3c+age+par+pial1e+inc,data=CellN) #set age to be quadratic and all else same for linear model 2#
summary(cell_model2) #summary of linear model 2#
anova(cell_model,cell_model2) #Anova of insmodel and insmodel2#
vif(cell_model) #Test for Multicollinearity#
bptest(cell_model) #heteroskedaskity#
durbinWatsonTest(cell_model) #Auto correlation#
ncvTest(cell_model) 
plot(cell_model) #Residual Graphs#
#Predict#
Cell$pial2e <- predict(cell_model,CellN,interval="prediction")
median(Cell$pial2e)

#Decision Tree#
Cell_rpart <- rpart(pial2~pial3b^2+pial3c+age+par+pial1e+inc,data=Cell)
#Summary of splits
summary(Cell_rpart)
#Viz the tree. Based plot in rpart. 
plot(Cell_rpart, uniform=TRUE,main="Classification Tree for Smartphone Ownership")
text(Cell_rpart, use.n=TRUE, all=TRUE, cex=.6)
rpart.plot(Cell_rpart, type=1, extra=2, cex=.6, main="Classification Tree for Smartphone Ownership")
# visualize cross-validation results 
printcp(Cell_rpart)
plotcp(Cell_rpart)
#CREATE A CONFUSION MATRIX 
require(caret)
actual <- Cell_test$pial2
predicted <- predict(Cell_rpart, Cell_test, type="class")
results.matrix <- confusionMatrix(predicted, actual)
print(results.matrix)

#Random Forest#
set.seed(99) 
registerDoSNOW(makeCluster(4, type = "SOCK"))
Cell_rf <- randomForest(pial2~pial3b^2+pial3c+age+par+pial1e+inc,data=Cell, ntree=500,na.action = na.omit, importance=TRUE)
print(Cell_rf) #shows OOB of model and confusion matrix
importance(Cell_rf) #shows the importance of each variable
varImpPlot(Cell_rf) #plots the importance of each variable

Cell_rf_predict<-predict(Cell_rf, Cell_test,type="response") 
Cell_results.matrix <- confusionMatrix(Cell_rf_predict, actual,positive="yes") #the model vs the actual holdout data.
print(Cell_results.matrix)
```







