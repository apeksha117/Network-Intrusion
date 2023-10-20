#1)	Setting the working directory

getwd()


#2)	Reading data in R memory (training, validation and testing datasets)
train_data = read.csv("Network_Intrusion_Train_data.csv",header = T )
train_data1 = read.csv("Network_Intrusion_Train_data.csv",header = T )
test_data = read.csv("Network_Intrusion_Test_data.csv",header = T )
validate_data = read.csv("Validate_data.csv",header = T )

#3)	Checking the structure of the data
str(train_data)
str(test_data)
str(validate_data)

#Checking the summary of the data
summary(train_data)
summary(test_data)
summary(validate_data)

#4)	If data contains NA, then perform imputation.
#a.	For categorical data – replace NA with mode
#b.	For numeric data – replace NA with median

#Method 1
train_data_NA = for (i in length(train_data)){
  ifelse(is.element('TRUE',is.na(train_data[,i]))=='TRUE', print(colnames(train_data)[i]) , NA)}

test_data_NA = for (i in length(test_data)){
  ifelse(is.element('TRUE',is.na(test_data[,i]))=='TRUE', print(colnames(test_data)[i]) , NA)}


validate_data_NA= for (i in length(validate_data)){
  ifelse(is.element('TRUE',is.na(validate_data[,i]))=='TRUE', print(colnames(validate_data)[i]) , NA)}

#Method 2
indx_train <- apply(train_data, 2, function(x) any(is.na(x)))
colnames(indx_train)

indx_test <- apply(test_data, 2, function(x) any(is.na(x)))
colnames(indx_test)

indx_validate <- apply(validate_data, 2, function(x) any(is.na(x)))
colnames(indx_validate)

#method 3

##visualize the patterns of NAs
#install.packages("mice")
library(mice)
aqty2=train_data
md.pattern(aqty2)
#111 observations with no values

#install.packages("VIM")
library(VIM) #visualize the pattern of NAs
mp <- aggr(aqty2, col=c('navyblue','yellow'),
           numbers=TRUE, sortVars=TRUE,
           labels=names(aqty2), cex.axis=.7,
           gap=3, ylab=c("Missing data","Pattern"))

#checking if any na
summary(train_data)
summary(test_data)
summary(validate_data)

#creating a vector for character variables
vec= vector()
for(i in 1:ncol(train_data)){
  if(!is.numeric(train_data[,i]) & class(train_data[,i])=="factor" ){
    vec[i]=  colnames(train_data)[i]
  }else{print("numeric")}}
vec <- vec[!is.na(vec)]
vec= as.vector(vec)
class(vec)

#replacing numerical with median
#method 1 for numeric
impute.med <- function(x) {
  z <- median(x, na.rm = TRUE)
  x[is.na(x)] <- z
  return(x)
}

train_data_numeric= train_data[ , !(names(train_data) %in% vec)]

train_data_numeric <- sapply(train_data_numeric, function(x){
  if(is.numeric(x) & any(is.na(x))){
    impute.med(x)
  } else {
    x
  }
}
)

train_data_numeric= as.data.frame(train_data_numeric)

#Method 2 for numeric 
train_data_numeric= train_data[ , !(names(train_data) %in% vec)]


summary(train_data_numeric)
library(plyr)
impute.med <- function(x) replace(x, is.na(x), median(x, na.rm = TRUE))
train_data_numeric <- sapply(train_data_numeric, function(x){
  if(is.numeric(x)){
    impute.med(x)
  } else {
    x
  }
}
)

train_data_numeric= data.frame(train_data_numeric)

summary(train_data_numeric)

#replacing categorical with mode
#Method 1
train_data_factor= train_data[ , (names(train_data) %in% vec)]

impute.mode <- function (x, na.rm) { 
  xtab <- table(x) 
  xmode <- names(which(xtab == max(xtab))) 
  if (length(xmode) > 1) 
    xmode <- ">1 mode" 
  return(xmode) }

#class(train_data_factor$protocol_type)
summary(train_data_factor)

train_data_factor <- sapply(train_data_factor, function(x){
  if(!is.numeric(x) & class(x)=="factor" & any(is.na(x))){
    impute.mode(x, na.rm='TRUE')
  } else {
    x
  }
}
)
summary(train_data_factor)
train_data_factor= as.data.frame(train_data_factor)

train_data= cbind(train_data_factor, train_data_numeric)
summary(train_data)
#install.packages("dplyr")
library(dplyr)
train_data= as.data.frame(train_data)

# treating NA in validate

#Method 1
validate_data_NA = for (i in length(validate_data)){
  ifelse(is.element('TRUE',is.na(validate_data[,i]))=='TRUE', print(colnames(validate_data)[i]) , NA)}

test_data_NA = for (i in length(test_data)){
  ifelse(is.element('TRUE',is.na(test_data[,i]))=='TRUE', print(colnames(test_data)[i]) , NA)}


validate_data_NA= for (i in length(validate_data)){
  ifelse(is.element('TRUE',is.na(validate_data[,i]))=='TRUE', print(colnames(validate_data)[i]) , NA)}

#Method 2
indx_train <- apply(validate_data, 2, function(x) any(is.na(x)))
colnames(indx_train)

indx_test <- apply(test_data, 2, function(x) any(is.na(x)))
colnames(indx_test)

indx_validate <- apply(validate_data, 2, function(x) any(is.na(x)))
colnames(indx_validate)

#method 3

##visualize the patterns of NAs
#install.packages("mice")
library(mice)
aqty2=validate_data
md.pattern(aqty2)
#111 observations with no values

#install.packages("VIM")
library(VIM) #visualize the pattern of NAs
mp <- aggr(aqty2, col=c('navyblue','yellow'),
           numbers=TRUE, sortVars=TRUE,
           labels=names(aqty2), cex.axis=.7,
           gap=3, ylab=c("Missing data","Pattern"))

#checking if any na
summary(validate_data)
summary(test_data)
summary(validate_data)

#creating a vector for character variables
vec= vector()
for(i in 1:ncol(validate_data)){
  if(!is.numeric(validate_data[,i]) & class(validate_data[,i])=="factor" ){
    vec[i]=  colnames(validate_data)[i]
  }else{print("numeric")}}
vec <- vec[!is.na(vec)]
vec= as.vector(vec)
class(vec)

#replacing numerical with median
#method 1 for numeric
impute.med <- function(x) {
  z <- median(x, na.rm = TRUE)
  x[is.na(x)] <- z
  return(x)
}

validate_data_numeric= validate_data[ , !(names(validate_data) %in% vec)]

validate_data_numeric <- sapply(validate_data_numeric, function(x){
  if(is.numeric(x) & any(is.na(x))){
    impute.med(x)
  } else {
    x
  }
}
)

validate_data_numeric= as.data.frame(validate_data_numeric)

#Method 2 for numeric 
validate_data_numeric= validate_data[ , !(names(validate_data) %in% vec)]


summary(validate_data_numeric)
library(plyr)
impute.med <- function(x) replace(x, is.na(x), median(x, na.rm = TRUE))
validate_data_numeric <- sapply(validate_data_numeric, function(x){
  if(is.numeric(x)){
    impute.med(x)
  } else {
    x
  }
}
)

validate_data_numeric= data.frame(validate_data_numeric)

summary(validate_data_numeric)

#replacing categorical with mode
#Method 1
validate_data_factor= validate_data[ , (names(validate_data) %in% vec)]

impute.mode <- function (x, na.rm) { 
  xtab <- table(x) 
  xmode <- names(which(xtab == max(xtab))) 
  if (length(xmode) > 1) 
    xmode <- ">1 mode" 
  return(xmode) }

#class(validate_data_factor$protocol_type)
summary(validate_data_factor)

validate_data_factor <- sapply(validate_data_factor, function(x){
  if(!is.numeric(x) & class(x)=="factor" & any(is.na(x))){
    impute.mode(x, na.rm='TRUE')
  } else {
    x
  }
}
)
summary(validate_data_factor)
validate_data_factor= as.data.frame(validate_data_factor)

validate_data= cbind(validate_data_factor, validate_data_numeric)
summary(validate_data)
#install.packages("dplyr")
library(dplyr)
validate_data= as.data.frame(validate_data)


#treating NA in test

#Method 1
test_data_NA = for (i in length(test_data)){
  ifelse(is.element('TRUE',is.na(test_data[,i]))=='TRUE', print(colnames(test_data)[i]) , NA)}

test_data_NA = for (i in length(test_data)){
  ifelse(is.element('TRUE',is.na(test_data[,i]))=='TRUE', print(colnames(test_data)[i]) , NA)}


test_data_NA= for (i in length(test_data)){
  ifelse(is.element('TRUE',is.na(test_data[,i]))=='TRUE', print(colnames(test_data)[i]) , NA)}

#Method 2
indx_train <- apply(test_data, 2, function(x) any(is.na(x)))
colnames(indx_train)

indx_test <- apply(test_data, 2, function(x) any(is.na(x)))
colnames(indx_test)

indx_validate <- apply(test_data, 2, function(x) any(is.na(x)))
colnames(indx_validate)

#method 3

##visualize the patterns of NAs
#install.packages("mice")
library(mice)
aqty2=test_data
md.pattern(aqty2)
#111 observations with no values

#install.packages("VIM")
library(VIM) #visualize the pattern of NAs
mp <- aggr(aqty2, col=c('navyblue','yellow'),
           numbers=TRUE, sortVars=TRUE,
           labels=names(aqty2), cex.axis=.7,
           gap=3, ylab=c("Missing data","Pattern"))

#checking if any na
summary(test_data)
summary(test_data)
summary(test_data)

#creating a vector for character variables
vec= vector()
for(i in 1:ncol(test_data)){
  if(!is.numeric(test_data[,i]) & class(test_data[,i])=="factor" ){
    vec[i]=  colnames(test_data)[i]
  }else{print("numeric")}}
vec <- vec[!is.na(vec)]
vec= as.vector(vec)
class(vec)

#replacing numerical with median
#method 1 for numeric
impute.med <- function(x) {
  z <- median(x, na.rm = TRUE)
  x[is.na(x)] <- z
  return(x)
}

test_data_numeric= test_data[ , !(names(test_data) %in% vec)]

test_data_numeric <- sapply(test_data_numeric, function(x){
  if(is.numeric(x) & any(is.na(x))){
    impute.med(x)
  } else {
    x
  }
}
)

test_data_numeric= as.data.frame(test_data_numeric)

#Method 2 for numeric 
test_data_numeric= test_data[ , !(names(test_data) %in% vec)]


summary(test_data_numeric)
library(plyr)
impute.med <- function(x) replace(x, is.na(x), median(x, na.rm = TRUE))
test_data_numeric <- sapply(test_data_numeric, function(x){
  if(is.numeric(x)){
    impute.med(x)
  } else {
    x
  }
}
)

test_data_numeric= data.frame(test_data_numeric)

summary(test_data_numeric)

#replacing categorical with mode
#Method 1
test_data_factor= test_data[ , (names(test_data) %in% vec)]

impute.mode <- function (x, na.rm) { 
  xtab <- table(x) 
  xmode <- names(which(xtab == max(xtab))) 
  if (length(xmode) > 1) 
    xmode <- ">1 mode" 
  return(xmode) }

#class(test_data_factor$protocol_type)
summary(test_data_factor)

test_data_factor <- sapply(test_data_factor, function(x){
  if(!is.numeric(x) & class(x)=="factor" & any(is.na(x))){
    impute.mode(x, na.rm='TRUE')
  } else {
    x
  }
}
)
summary(test_data_factor)
test_data_factor= as.data.frame(test_data_factor)

test_data= cbind(test_data_factor, test_data_numeric)
summary(test_data)
#install.packages("dplyr")
library(dplyr)
test_data= as.data.frame(test_data)


#--------------------------------creating flags for numeric variables--------------------------------------------#

#creating flags for numeric variables for teain_data
for(i in 1:length(train_data)){
  if(is.numeric(train_data[,i])==TRUE){
    train_data[,paste(colnames(train_data)[i],"Flag",sep="_")] = ifelse(train_data[,i] > 0,1,0)
  }else{print("NA")}}

#validate_data
#creating flags for numeric variables
for(i in 1:length(validate_data)){
  if(is.numeric(validate_data[,i])==TRUE){
    validate_data[,paste(colnames(validate_data)[i],"Flag",sep="_")] = ifelse(validate_data[,i] > 0,1,0)
  }else{print("NA")}}

#test_data
#creating flags for numeric variables
for(i in 1:length(test_data)){
  if(is.numeric(test_data[,i])==TRUE){
    test_data[,paste(colnames(test_data)[i],"Flag",sep="_")] = ifelse(test_data[,i] > 0,1,0)
  }else{print("NA")}}



#---------------Making data ready to fit model-----------------------------------#
validate_data= subset(validate_data,validate_data$service !="tftp_u" )

validate_data= validate_data[, names(validate_data) %in% names(train_data)]

test_data= subset(test_data,test_data$service !="tftp_u" )
test_data= test_data[, names(test_data) %in% names(test_data)]

# handling dependent var
train_data$class1= ifelse(train_data$class=="anomaly", 1, 0)
train_data$class= NULL
names(train_data)[names(train_data)== "class1"]= "y_var"

validate_data$class1= ifelse(validate_data$class=="anomaly", 1, 0)
validate_data$class= NULL
names(validate_data)[names(validate_data)== "class1"]= "y_var"


#converting categorical into factors

for(i in 1:length(train_data)){
  if(is.numeric(train_data[,i])==FALSE){
    train_data[,i]= as.factor(train_data[,i])
  }else{print("NA")}}

#-------------------------------- feature selection- Recursive Feature Elimination (RFE)----------------------#
#Recursive feature elimnation (rfe) offers traditional feature selection algorithm AND 
#a rigorous way to determine the important variables before you even feed them into a ML algo.
#It can be implemented using the rfe() from caret package.
#The rfe() also takes FOLLOWING important parameters.

#1.sizes- determines the number of most important features the rfe should iterate. 

#2.rfeControl- rfeControl parameter receives the output of the rfeControl(). 
#You can set what type of variable evaluation algorithm must be used.
#Here, I have used random forests based rfFuncs.

#3.The method='repeatedCV' means it will do a repeated k-Fold cross validation with repeats=5.

#Once complete, you get the accuracy and kappa for each model size you provided. 
#The final selected model subset size is marked with a * in the rightmost selected column.
#install.packages("randomForest")
set.seed(100)
# options(warn=-1)

subsets <- c(1:41)
library(caret)
library(randomForest)
ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)
#as service has greater than 32 levels converting into dummy column
# for(level in unique(train_data$service)){
#   train_data[paste("service", level, sep = "_")] <- ifelse(train_data$service == level, 1, 0)
# }
# train_data$service= NULL

train_data$service= as.numeric(train_data$service)

rfe.train <- rfe(x=train_data[,!(names(train_data) %in% c("y_var"))], y=train_data$y_var, size= subsets,
                 rfeControl = ctrl)

plot(rfe.train, type=c("g", "o"), cex = 1.0)
predictors(rfe.train)
rfe.train$optVariables


#subsetting train for only significant variables
fin_var= vector()
fin_var= rownames(imps2)
train_data= train_data[,names(train_data) %in% c(fin_var,"y_var")]
names(train_data)
#---------------------------------------------- model building-------------------------------#
# Iteration 2
#6)	Fit Decision Tree model on training data 

## iteration 2
class(train_data)
train_data= as.data.frame(train_data)
#no need of feature scaling in decision trees as not ecudiliean distance algo
# Fitting Decision Tree Classification to the Training set
#install.packages('rpart')
library(rpart)

classifier = rpart(formula = y_var~.,
                   data = train_data, method="class", control=rpart.control(minsplit=2))

validate_data= subset(validate_data,validate_data$service !="tftp_u" )

validate_data= validate_data[, names(validate_data) %in% names(train_data)]

test_data= subset(test_data,test_data$service !="tftp_u" )
test_data= test_data[, names(test_data) %in% names(test_data)]

# Predicting the Test set results

y_pred = predict(classifier, newdata = validate_data[,!(names(validate_data) %in% c("y_var"))], type = 'class')

# Making the Confusion Matrix to test accuracy
cm = table(validate_data$y_var, y_pred)
accuracy= (cm[1,1] + cm[2,2])/ sum(cm)
print(accuracy)

#0.7966996
plot(classifier)
text(classifier)
# improve the visualization
#Method 1
prp(classifier,box.col=c("Grey", "Orange")[classifier$frame$yval],varlen=0,faclen=0, type=1,extra=4,under=TRUE)
# improve the visualization method 2
#install.packages("rattle")
#install.packages("https://cran.r-project.org/bin/windows/contrib/3.3/RGtk2_2.20.31.zip", repos=NULL)
library(rattle)
rattle()
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(classifier)

# post pruning using cp- complexity parameter and the cross validation error values
printcp(classifier)
plotcp(classifier)
#select the one having the least cross-validated error- xerror and use it to prune the tree
#From the above mentioned list of cp values-
#Prune the tree to create an optimal decision tree :
ptree<- prune(classifier, cp= classifier$cptable[which.min(classifier$cptable[,"xerror"]),"CP"])
fancyRpartPlot(ptree, uniform=TRUE,main="Pruned Classification Tree")
prp(ptree,box.col=c("Grey", "Orange")[ptree$frame$yval],varlen=0,faclen=0, type=1,extra=4,under=TRUE, main="Pruned Classification Tree")

# Predicting the Test set results using pruned tree

y_pred_prune = predict(ptree, newdata = validate_data[,!(names(validate_data) %in% c("y_var"))], type = 'class')

# Making the Confusion Matrix to test accuracy
cm = table(validate_data$y_var, y_pred_prune)
accuracy= (cm[1,1] + cm[2,2])/ sum(cm)
print(accuracy)

#prepruning
# Grow a tree with minsplit of 100 and max depth of 8
classifier <- rpart(formula = y_var~.,
                    data = train_data, method="class", control=rpart.control(cp = 0, maxdepth = 8,minsplit = 100))
prp(classifier,box.col=c("Grey", "Orange")[classifier$frame$yval],varlen=0,faclen=0, type=1,extra=4,under=TRUE, main="Pruned Classification Tree")

# Compute the accuracy of the pruned tree
y_pred_prune_pre = predict(classifier, newdata = validate_data[,!(names(validate_data) %in% c("y_var"))], type = 'class')

cm = table(validate_data$y_var, y_pred_prune_pre)
accuracy= (cm[1,1] + cm[2,2])/ sum(cm)
print(accuracy)
#0.8042408
