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
colnames(train_data)

#---------------------------Feature Creation-------------------------------------------------#

#creating flags for numeric variables
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

colnames(train_data)

#-------------------------------- feature selection----------------------#
# checking for distinct values and the variance of columns with numeric data and frequency distribution for categorical
##examine data distribution 
# removing columns with zero variation
summary(train_data)
i=10
vec1= vector()
for(i in 1:length(train_data)){
  print(paste(i,"column= ",names(train_data)[i],sep="")) 
  if(is.numeric(train_data[,i])){
    print(paste("variance= ", var(train_data[,i]),sep="")) 
    print(unique(train_data[,i]))
    #printing histogram and boxplot for numeric
    hist(get(colnames(train_data)[i],train_data),main=paste("histogram",names(train_data)[i], sep=" ") ) # distribution of a varaibles
    boxplot(train_data[,i],main=paste("boxplot",names(train_data)[i], sep=" "),boxwex=0.9)
    #removing 0 variance columns
    if(var(train_data[,i])==0){
      print(paste("Remove column ", names(train_data)[i], sep=""))
      vec1[i]=  colnames(train_data)[i]
    }else{print("Non zero variance")}   
    
  }else{ print(unique(train_data[,i]))
    print(table(train_data[,i]), sep="")
    #printing barplot for categorical 
    barplot(table(train_data[,i]), main=paste("barplot",names(train_data)[i],sep=" "))
    #removing 0 variance columns
    if(length(unique(train_data[,i]))==1){
      print(paste("Remove column", names(train_data)[i], sep=""))
      vec1[i]=  colnames(train_data)[i]
    }else{print("Non zero variance")}  
  }
}
vec1 <- vec1[!is.na(vec1)]
vec1= as.vector(vec1)
class(vec1)

train_data= train_data[ , !(names(train_data) %in% vec1)]
train_data= as.data.frame(train_data) 
colnames(train_data)
#Checking correlation of class variable(dependent) with individual variables

df <- data.frame(matrix(ncol = 3, nrow = ncol(train_data)))
df$X1= "aa"
df$X2= 0
df$X3= 0
for (i in 1:length(train_data)) {
  df[i,grep("X1", colnames(df))]= colnames(train_data)[i]
  if(is.numeric(train_data[,i])== TRUE){
    train_data$class1= ifelse(train_data$class=="anomaly", 1, 0)
    df[i,grep("X2", colnames(df))]= cor.test(train_data[,i], train_data$class1)$p.value
    df[i,grep("X3", colnames(df))]= cor.test(train_data[,i], train_data$class1)$estimate
  }
  else{
    #chi-square test for categorical variables
    # Create a table with the needed variables
    a = table(train_data[,i], train_data$class) 
    #print(a)
    # Perform the Chi-Square test.
    df[i,grep("X2", colnames(df))]=(chisq.test(a))$p.value
    df[i,grep("X3", colnames(df))]= NA
  }
  df$X4= ifelse(df$X2<0.05,"Significant", "Not Significant") 
}


# removing non significant columns
sig_col= df[df$X4=="Significant",]
sig_col_vec= sig_col$X1
train_data= train_data[ , (names(train_data) %in% sig_col_vec)]

colnames(train_data)

#5)	Different plots to identify relationship between variables for all the variables within independent variables
#PLOTTING ONLY CATEGORICAL VARIABLES AGAINST CLASS
plot(train_data$protocol_type,train_data$class, col=c("red","green"), main=("plot class vs protocol type"), xlab="protocol", ylab="class")
plot(train_data$service,train_data$class, col=c("red","green"), main=("plot class vs service"), xlab="service", ylab="class")
plot(train_data$flag,train_data$class, col=c("red","green"), main=("plot class vs flag"), xlab="flag", ylab="class")

#method2- along with p values of corelation  
#A t test is available to test the null hypothesis that the correlation coefficient is zero
#Note that the P value derived from the test provides no information on how strongly the 2 variables are related
#install.packages("Hmisc")
#the p-value is less than 0.05, there is strong evidence against the null hypothesis. 
#As a result, reject the null hypothesis and accept the alternative hypothesis that there is co relation between  2 variables

vec_num=vector()
vec_char=vector()
for (i in 1:length(train_data)) {
  print(colnames(train_data)[i])
  if(is.numeric(train_data[,i])== TRUE){
    vec_num[i]= colnames(train_data)[i]
  }else{
    vec_char[i]= colnames(train_data)[i]}
  }



library("Hmisc")

# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

res3<-rcorr(as.matrix(train_data[,names(train_data) %in% vec_num]))
res4 =flattenCorrMatrix(res3$r, res3$P)

# variables correlation>0.90 or correlation< (-.0.9) val pvalue <0.05 are having strong coreltion
res5= subset(res4,(res4$cor>0.90 & res4$p<0.05))
names(res5)[names(res5)== "row"]= "col1"
names(res5)[names(res5)== "column"]= "col2"

#checking corelation of both variables with the depentdent variable and  removing one with low correlation value
res6=data.frame()
res6= merge(res5, df, by.x = "col1", by.y = "X1", all.x = T )
names(res6)[names(res6)== "X3"]= "row_cor"
res6= subset(res6, select = c("col1","col2","row_cor" ))
res6= merge(res6, df, by.x = "col2", by.y = "X1", all.x = T )
names(res6)[names(res6)== "X3"]= "column_cor"
res6= subset(res6, select = c("col1","col2","row_cor","column_cor" ))
rem_var=vector()
res6$rem_var= ifelse(res6$row_cor> res6$column_cor, as.character(res6$col2) , as.character(res6$col1) )
vec2=vector()
vec2= unique(res6$rem_var)
train_data= train_data[ , !(names(train_data) %in% vec2)]

colnames(train_data)

#---------------Making data ready to fit model-----------------------------------#
# handling dependent var
train_data$class1= ifelse(train_data$class=="anomaly", 1, 0)
train_data$class= NULL
names(train_data)[names(train_data)== "class1"]= "y_var"

validate_data$class1= ifelse(validate_data$class=="anomaly", 1, 0)
validate_data$class= NULL
names(validate_data)[names(validate_data)== "class1"]= "y_var"

validate_data= subset(validate_data,validate_data$service !="tftp_u" )

validate_data= validate_data[, names(validate_data) %in% names(train_data)]

test_data= subset(test_data,test_data$service !="tftp_u" )
test_data= test_data[, names(test_data) %in% names(test_data)]


# test_data$class1= ifelse(test_data$class=="anomaly", 1, 0)
# test_data$class= NULL
# names(test_data)[names(test_data)== "class1"]= "y_var"

# #install.packages("corrplot")
# #install.packages("ggplot2")


#--------------------------Building Classifier----Decesion Tree------------------------#
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
                   data = train_data, method="class")

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
# improve the visualization method 2
#install.packages("rattle")
#install.packages("https://cran.r-project.org/bin/windows/contrib/3.3/RGtk2_2.20.31.zip", repos=NULL)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(classifier)
#Method 1
prp(classifier,box.col=c("Grey", "Orange")[classifier$frame$yval],varlen=0,faclen=0, type=1,extra=4,under=TRUE)


# post pruning using cp- complexity parameter and the cross validation error values
printcp(classifier)
plotcp(classifier)
#select the one having the least cross-validated error- xerror and use it to prune the tree
#From the above mentioned list of cp values-
#Prune the tree to create an optimal decision tree :
cp_min= min(classifier$cptable[,"xerror"])
cp_min
ptree<- prune(classifier, cp= classifier$cptable[which.min(classifier$cptable[,"xerror"]),"CP"])
fancyRpartPlot(ptree, uniform=TRUE,main="Pruned Classification Tree")
prp(ptree,box.col=c("Grey", "Orange")[ptree$frame$yval],varlen=0,faclen=0, type=1,extra=4,under=TRUE, main="Pruned Classification Tree")

# Predicting the Test set results using pruned tree

y_pred_prune = predict(ptree, newdata = validate_data[,!(names(validate_data) %in% c("y_var"))], type = 'class')

# Making the Confusion Matrix to test accuracy
cm = table(validate_data$y_var, y_pred_prune)
accuracy= (cm[1,1] + cm[2,2])/ sum(cm)
print(accuracy)
#0.7966996

#prepruning
# Grow a tree with minsplit of 100 and max depth of 8
classifier <- rpart(formula = y_var~.,
                    data = train_data, method="class", control=rpart.control(cp = 0.01, maxdepth = 8,minsplit = 100))
prp(classifier,box.col=c("Grey", "Orange")[classifier$frame$yval],varlen=0,faclen=0, type=1,extra=4,under=TRUE, main=" Pre Pruned Classification Tree")

# Compute the accuracy of the pruned tree
y_pred_prune_pre = predict(classifier, newdata = validate_data[,!(names(validate_data) %in% c("y_var"))], type = 'class')

cm = table(validate_data$y_var, y_pred_prune_pre)
accuracy= (cm[1,1] + cm[2,2])/ sum(cm)
print(accuracy)
#0.8042408




#---------------------------RANDOM FOREST------------------------------#
#random forest
# first time H20 instalment
#Remove any previously installed packages for R
#if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
#if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

#download packages that H2O depends on.
#pkgs <- c("RCurl","jsonlite")
#for (pkg in pkgs) {
#  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
#}

#Download and install the latest H2O package for R.
# install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/latest_stable_R")))
# install.packages("rsample")
# install.packages("pROC")

library(rsample)      # data splitting 
library(randomForest) # basic implementation
library(ranger)       # a faster implementation of randomForest
library(caret)        # an aggregator package for performing many machine learning models
library(h2o)          # an extremely fast java-based platform

# Basic Implementation using randomForest package
# for reproduciblity
set.seed(123)

train_data$service= as.numeric(train_data$service)
validate_data$service= as.numeric(validate_data$service)
test_data$service= as.numeric(test_data$service)

train_data$class1= ifelse(train_data$y_var==1,"anomaly", "Normal")
train_data$y_var= NULL
names(train_data)[names(train_data)== "class1"]= "y_var"


validate_data$class1= ifelse(validate_data$y_var==1,"anomaly", "Normal")
validate_data$y_var= NULL
names(validate_data)[names(validate_data)== "class1"]= "y_var"

train_data$y_var= as.factor(train_data$y_var)
validate_data$y_var= as.factor(validate_data$y_var)

class(train_data$y_var)

# default RF model
m1 <- randomForest(
  formula = train_data$y_var ~ .,
  data    = train_data
)

m1

# Call:
#   randomForest(formula = train_data$y_var ~ ., data = train_data) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 6
# 
# OOB estimate of  error rate: 0.39%
# Confusion matrix:
#   anomaly Normal class.error
# anomaly   11683     60 0.005109427
# Normal       38  13411 0.002825489

plot(m1)

# number of trees with lowest errorr rate
min(m1$err.rate)
#0.002751134
which.min(m1$err.rate)
#1398


y_pred = predict(m1, newdata = validate_data[,!(names(validate_data) %in% c("y_var"))],type ="class")

# Making the Confusion Matrix to test accuracy
cm = table(validate_data$y_var, y_pred)
accuracy= (cm[1,1] + cm[2,2])/ sum(cm)
# 0.7605909
print(accuracy)
# 0.7765604 ntree=400

##Tuning parameters
#ntree
#mtry
#sampsize
#nodesize
#maxnodes
# tuning the mtry parameter we can use randomForest::tuneRF for a quick and easy tuning assessment. 
# tuneRf will start at a value of mtry that you supply and increase 
# by a certain step factor until the OOB error stops improving be a specified amount.

# names of features
features <- setdiff(names(train_data), "y_var")

set.seed(123)

m2 <- tuneRF(
  x          = train_data[features],
  y          = train_data$y_var,
  ntreeTry   = 400,
  mtryStart  = 5,
  stepFactor = 2,
  improve    = 0.01,
  trace      = TRUE     # to not show real-time progress 
)

# mtry = 5  OOB error = 0.53% 
# Searching left ...
# mtry = 3 	OOB error = 0.77% 
# -0.4586466 0.01 
# Searching right ...
# mtry = 10 	OOB error = 0.32% 
# 0.3984962 0.01 
# mtry = 20 	OOB error = 0.33% 
# -0.0375 0.01 


m1 <- randomForest(
  formula = train_data$y_var ~ .,
  data    = train_data,
  ntreeTry   = 500,
  mtry =20
)
m1

# Call:
#   randomForest(formula = train_data$y_var ~ ., data = train_data,      ntreeTry = 500, mtry = 20) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 20
# 
# OOB estimate of  error rate: 0.33%
# Confusion matrix:
#   anomaly Normal class.error
# anomaly   11693     50 0.004257856
# Normal       32  13417 0.002379359

y_pred = predict(m1, newdata = validate_data[,!(names(validate_data) %in% c("y_var"))],type ="class")



# Making the Confusion Matrix to test accuracy
cm = table(validate_data$y_var, y_pred)
accuracy= (cm[1,1] + cm[2,2])/ sum(cm)
print(accuracy)

#0.7781129 ntree=500 mtry=20
#0.7644502 ntree=400 mtry=40


#printing important variables
library(caret)
RM_IMP=varImp(m1, scale= F)
RM_IMP
# dst_bytes_Flag	4430.01
# flag	2051.48
# src_bytes_Flag	848.22
# same_srv_rate	636.58
# protocol_type	584.21
# count	519.67
# dst_host_same_src_port_rate	442.18
# service	390.11
# diff_srv_rate	337.67
# dst_host_same_srv_rate	318.51
# diff_srv_rate_Flag	252.14
# dst_host_diff_srv_rate	249.43
# dst_host_srv_count	208.77
# dst_host_srv_diff_host_rate	148.14
# dst_host_count	133.28
# hot	130.60
# hot_Flag	114.42
# logged_in_Flag	97.70
# duration	91.89
# dst_host_rerror_rate_Flag	91.36
# wrong_fragment_Flag	83.23
# dst_host_srv_serror_rate	77.52
# dst_host_diff_srv_rate_Flag	58.00
# dst_host_same_src_port_rate_Flag	44.48
# num_compromised_Flag	37.34
# dst_host_srv_diff_host_rate_Flag	33.76
# num_compromised	33.05
# rerror_rate_Flag	16.14
# srv_diff_host_rate	14.25
# dst_host_srv_rerror_rate_Flag	10.70
# dst_host_serror_rate_Flag	8.83
# duration_Flag	8.20
# srv_diff_host_rate_Flag	7.87
# num_root_Flag	5.49
# is_guest_login_Flag	4.63
# dst_host_same_srv_rate_Flag	3.88
# num_file_creations	1.70
# num_access_files	1.59
# num_access_files_Flag	1.53
# root_shell_Flag	1.40
# num_file_creations_Flag	1.14
# same_srv_rate_Flag	0.92
# su_attempted	0.49
# num_shells_Flag	0.09


#To get the area under the ROC curve for each predictor, the filterVarImp function can be used
roc_imp <- filterVarImp(x = train_data[, -ncol(train_data)], y = train_data$y_var)
roc_imp

# anomaly	Normal
# dst_bytes_Flag	0.9052895	0.9052895
# dst_host_srv_count	0.8908315	0.8908315
# same_srv_rate	0.8723819	0.8723819
# src_bytes_Flag	0.8666435	0.8666435
# dst_host_same_srv_rate	0.8642143	0.8642143
# flag	0.8592038	0.8592038
# diff_srv_rate_Flag	0.8498343	0.8498343
# diff_srv_rate	0.8420721	0.8420721
# logged_in_Flag	0.8371099	0.8371099
# dst_host_diff_srv_rate	0.8248347	0.8248347
# count	0.8207797	0.8207797
# dst_host_srv_serror_rate	0.7838931	0.7838931
# dst_host_serror_rate_Flag	0.783633	0.783633
# dst_host_diff_srv_rate_Flag	0.7363288	0.7363288
# dst_host_count	0.7044021	0.7044021
# dst_host_srv_diff_host_rate_Flag	0.7020398	0.7020398
# dst_host_same_src_port_rate_Flag	0.6813339	0.6813339
# dst_host_srv_diff_host_rate	0.6811461	0.6811461
# srv_diff_host_rate_Flag	0.6470976	0.6470976
# srv_diff_host_rate	0.637861	0.637861
# dst_host_same_src_port_rate	0.630386	0.630386
# service	0.6250169	0.6250169
# protocol_type	0.6107479	0.6107479
# rerror_rate_Flag	0.5871804	0.5871804
# dst_host_rerror_rate_Flag	0.5776429	0.5776429
# dst_host_srv_rerror_rate_Flag	0.5660878	0.5660878
# duration_Flag	0.5413509	0.5413509
# duration	0.5405969	0.5405969
# dst_host_same_srv_rate_Flag	0.5404517	0.5404517
# same_srv_rate_Flag	0.5226416	0.5226416
# wrong_fragment_Flag	0.5095376	0.5095376
# num_root_Flag	0.5046628	0.5046628
# num_compromised_Flag	0.5044033	0.5044033
# num_compromised	0.5043644	0.5043644
# hot_Flag	0.5037172	0.5037172
# is_guest_login_Flag	0.5036857	0.5036857
# hot	0.5036375	0.5036375
# num_access_files_Flag	0.502618	0.502618
# num_access_files	0.5026179	0.5026179
# num_file_creations	0.5016562	0.5016562
# num_file_creations_Flag	0.5016562	0.5016562
# su_attempted	0.5007807	0.5007807
# root_shell_Flag	0.5007321	0.5007321
# num_shells_Flag	0.5002548	0.5002548


varImpPlot(m1,type=2, pretty=T, cex=0.6)

#hyper parameter tuning using H2O package
# start up h2o 
# Activate h2o package for using: 
library(h2o)
h2o.init(nthreads = 20, max_mem_size = "16g")
h2o.no_progress()

# Convert to h2o Frame and identify inputs and output: 
test <- as.h2o(validate_data)
train <- as.h2o(train_data)
y <- "y_var"
x <- setdiff(names(train), y)

#Default Random Forest
# Train default Random Forest: 
default_rf <- h2o.randomForest(x = x, y = y, 
                               training_frame = train, 
                               stopping_rounds = 5, 
                               stopping_tolerance = 0.001, 
                               stopping_metric = "AUC", 
                               seed = 29, 
                               balance_classes = FALSE, 
                               nfolds = 10)

default_rf

# Model Details:
#   ==============
#   
#   H2OBinomialModel: drf
# Model ID:  DRF_model_R_1568884809795_1 
# Model Summary: 
#   number_of_trees number_of_internal_trees model_size_in_bytes min_depth max_depth mean_depth
# 1              24                       24               75047        16        20   18.37500
# min_leaves max_leaves mean_leaves
# 1        210        284   242.66667
# 
# 
# H2OBinomialMetrics: drf
# ** Reported on training data. **
#   ** Metrics reported on Out-Of-Bag training samples **
#   
#   MSE:  0.003671112
# RMSE:  0.0605897
# LogLoss:  0.02283546
# Mean Per-Class Error:  0.004157486
# AUC:  0.9995738
# pr_auc:  0.4148301
# Gini:  0.9991476
# R^2:  0.9852479
# 
# Confusion Matrix (vertical: actual; across: predicted) for F1-optimal threshold:
#   Normal anomaly    Error        Rate
# Normal   13391      58 0.004313   =58/13449
# anomaly     47   11696 0.004002   =47/11743
# Totals   13438   11754 0.004168  =105/25192
# 
# Maximum Metrics: Maximum metrics at their respective thresholds
# metric threshold        value idx
# 1                       max f1  0.424861     0.995531 233
# 2                       max f2  0.350325     0.996341 247
# 3                 max f0point5  0.722771     0.996741 171
# 4                 max accuracy  0.424861     0.995832 233
# 5                max precision  0.970786     0.999910  49
# 6                   max recall  0.000000     1.000000 399
# 7              max specificity  1.000000     0.999926   0
# 8             max absolute_mcc  0.424861     0.991626 233
# 9   max min_per_class_accuracy  0.433838     0.995762 231
# 10 max mean_per_class_accuracy  0.424861     0.995843 233
# 11                     max tns  1.000000 13448.000000   0
# 12                     max fns  1.000000  4875.000000   0
# 13                     max fps  0.000000 13449.000000 399
# 14                     max tps  0.000000 11743.000000 399
# 15                     max tnr  1.000000     0.999926   0
# 16                     max fnr  1.000000     0.415141   0
# 17                     max fpr  0.000000     1.000000 399
# 18                     max tpr  0.000000     1.000000 399
# 
# Gains/Lift Table: Extract with `h2o.gainsLift(<model>, <data>)` or `h2o.gainsLift(<model>, valid=<T/F>, xval=<T/F>)`
# 
# H2OBinomialMetrics: drf
# ** Reported on cross-validation data. **
#   ** 10-fold cross-validation on training data (Metrics computed for combined holdout predictions) **
#   
#   MSE:  0.003375537
# RMSE:  0.05809937
# LogLoss:  0.01857097
# Mean Per-Class Error:  0.003446973
# AUC:  0.9997267
# pr_auc:  0.1994724
# Gini:  0.9994533
# R^2:  0.9864356
# 
# Confusion Matrix (vertical: actual; across: predicted) for F1-optimal threshold:
#   Normal anomaly    Error       Rate
# Normal   13425      24 0.001785  =24/13449
# anomaly     60   11683 0.005109  =60/11743
# Totals   13485   11707 0.003334  =84/25192
# 
# Maximum Metrics: Maximum metrics at their respective thresholds
# metric threshold        value idx
# 1                       max f1  0.558717     0.996418 186
# 2                       max f2  0.358891     0.996426 220
# 3                 max f0point5  0.583333     0.997489 182
# 4                 max accuracy  0.583333     0.996666 182
# 5                max precision  0.999992     1.000000   0
# 6                   max recall  0.000004     1.000000 399
# 7              max specificity  0.999992     1.000000   0
# 8             max absolute_mcc  0.583333     0.993305 182
# 9   max min_per_class_accuracy  0.447539     0.995998 202
# 10 max mean_per_class_accuracy  0.558717     0.996553 186
# 11                     max tns  0.999992 13449.000000   0
# 12                     max fns  0.999992  2345.000000   0
# 13                     max fps  0.000004 13449.000000 399
# 14                     max tps  0.000004 11743.000000 399
# 15                     max tnr  0.999992     1.000000   0
# 16                     max fnr  0.999992     0.199693   0
# 17                     max fpr  0.000004     1.000000 399
# 18                     max tpr  0.000004     1.000000 399
# 
# Gains/Lift Table: Extract with `h2o.gainsLift(<model>, <data>)` or `h2o.gainsLift(<model>, valid=<T/F>, xval=<T/F>)`
# Cross-Validation Metrics Summary: 
#   mean           sd   cv_1_valid   cv_2_valid   cv_3_valid
# accuracy                   0.9970207  0.001013656    0.9967506   0.99796253   0.99763685
# auc                       0.99967295 4.6084155E-4   0.99993825   0.99996996    0.9999689
# err                      0.002979291  0.001013656 0.0032493907 0.0020374898  0.002363135
# err_count                        7.5    2.5495098          8.0          5.0          6.0
# f0point5                   0.9976412  8.908003E-4   0.99647886    0.9975542   0.99850225
# f1                         0.9968032  0.001069798   0.99647886   0.99781567   0.99750626
# f2                         0.9959677 0.0016434814   0.99647886    0.9980773   0.99651223
# lift_top_group             2.1463132  0.053250656    2.1672535     2.145105     2.107054
# logloss                  0.018486135  0.008250675  0.012761446  0.011001714  0.011567972
# max_per_class_error      0.004700774 0.0019333666 0.0035211267 0.0022900763  0.004149378
# mcc                       0.99401736  0.002028737   0.99346226   0.99590683    0.9952657
# mean_per_class_accuracy   0.99692035 0.0010522003   0.99673116   0.99798083    0.9975505
# mean_per_class_error    0.0030796332 0.0010522003  0.003268859 0.0020191642 0.0024495013
# mse                      0.003374505 8.5057126E-4 0.0030054785 0.0024731434 0.0025541056
# precision                 0.99820125 0.0011111415   0.99647886    0.9973799    0.9991674
# r2                         0.9864348  0.003408484   0.98790604   0.99006194    0.9897571
# recall                     0.9954121 0.0020984025   0.99647886   0.99825174    0.9958506
# rmse                       0.0576983 0.0071033044  0.054822244   0.04973071   0.05053816
# specificity               0.99842864 9.7209384E-4    0.9969834   0.99770993   0.99925035
# cv_4_valid   cv_5_valid   cv_6_valid   cv_7_valid   cv_8_valid
# accuracy                   0.9972805   0.99605054    0.9984466   0.99801904   0.99526066
# auc                        0.9984664    0.9999099   0.99942297   0.99962693    0.9996819
# err                     0.0027195027  0.003949447 0.0015533981 0.0019809825 0.0047393367
# err_count                        7.0         10.0          4.0          5.0         12.0
# f0point5                   0.9977185    0.9968062   0.99932075    0.9976604   0.99648887
# f1                         0.9969312   0.99580187   0.99830365   0.99791056   0.99499166
# f2                        0.99614507   0.99479955    0.9972886   0.99816084    0.9934989
# lift_top_group             2.2539403    2.1223805    2.1803555    2.1103678    2.1082432
# logloss                  0.038640372  0.014837253   0.02423852  0.016070016  0.021953512
# max_per_class_error      0.004378284  0.005867561 0.0033869601  0.002259036 0.0074937553
# mcc                       0.99449235    0.9920783    0.9968759   0.99602765   0.99050516
# mean_per_class_accuracy    0.9971125     0.995946    0.9983065   0.99803436    0.9951261
# mean_per_class_error     0.002887466  0.004054019 0.0016934801 0.0019656385   0.00487385
# mse                     0.0034369575 0.0037248004  0.002567595 0.0029053434 0.0051674475
# precision                 0.99824405    0.9974769          1.0   0.99749374    0.9974895
# r2                         0.9860754    0.9850511   0.98965883   0.98834676    0.9792756
# recall                    0.99562174   0.99413246     0.996613   0.99832773   0.99250627
# rmse                      0.05862557   0.06103114  0.050671443  0.053901237   0.07188496
# specificity               0.99860334    0.9977595          1.0     0.997741   0.99774605
# cv_9_valid  cv_10_valid
# accuracy                  0.99637973      0.99642
# auc                       0.99980396    0.9999401
# err                     0.0036202734 0.0035799523
# err_count                        9.0          9.0
# f0point5                   0.9978712   0.99801064
# f1                        0.99601597    0.9962764
# f2                        0.99416757   0.99454814
# lift_top_group             2.1941748    2.0742574
# logloss                  0.017392032  0.016398506
# max_per_class_error     0.0070609003 0.0066006603
# mcc                       0.99271464   0.99284494
# mean_per_class_accuracy       0.9961   0.99631566
# mean_per_class_error    0.0038999992 0.0036843547
# mse                        0.0039168 0.0039933794
# precision                  0.9991119    0.9991701
# r2                         0.9842091     0.984006
# recall                     0.9929391    0.9933993
# rmse                      0.06258434  0.063193195
# specificity                0.9992609   0.99923193

# Function for collecting cross-validation results: 

results_cross_validation <- function(h2o_model) {
  h2o_model@model$cross_validation_metrics_summary %>% 
    as.data.frame() %>% 
    select(-mean, -sd) %>% 
    t() %>% 
    as.data.frame() %>% 
    mutate_all(as.character) %>% 
    mutate_all(as.numeric) %>% 
    select(Accuracy = accuracy, 
           AUC = auc, 
           Precision = precision, 
           Specificity = specificity, 
           Recall = recall, 
           Logloss = logloss) %>% 
    return()
}

# Use function: 
results_cross_validation(default_rf) -> ket_qua_default
ket_qua_default
# #  Accuracy       AUC Precision Specificity    Recall    Logloss
# 1  0.9967506 0.9999383 0.9964789   0.9969834 0.9964789 0.01276145
# 2  0.9979625 0.9999700 0.9973799   0.9977099 0.9982517 0.01100171
# 3  0.9976369 0.9999689 0.9991674   0.9992503 0.9958506 0.01156797
# 4  0.9972805 0.9984664 0.9982441   0.9986033 0.9956217 0.03864037
# 5  0.9960505 0.9999099 0.9974769   0.9977595 0.9941325 0.01483725
# 6  0.9984466 0.9994230 1.0000000   1.0000000 0.9966130 0.02423852
# 7  0.9980190 0.9996269 0.9974937   0.9977410 0.9983277 0.01607002
# 8  0.9952607 0.9996819 0.9974895   0.9977461 0.9925063 0.02195351
# 9  0.9963797 0.9998040 0.9991119   0.9992609 0.9929391 0.01739203
# 10 0.9964200 0.9999401 0.9991701   0.9992319 0.9933993 0.01639851

# Model Performance by Graph: 
theme_set(theme_minimal())

plot_results <- function(df_results) {
  df_results %>% 
    gather(Metrics, Values) %>% 
    ggplot(aes(Metrics, Values, fill = Metrics, color = Metrics)) +
    geom_boxplot(alpha = 0.3, show.legend = FALSE) + 
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +    
    scale_y_continuous(labels = scales::percent) + 
    facet_wrap(~ Metrics, scales = "free") + 
    labs(title = "Model Performance by Some Criteria Selected", y = NULL)
}

plot_results(ket_qua_default) +
  labs(subtitle = "Model: Random Forest (h2o package)")

# Model performance based on test data: 
pred_class <- h2o.predict(default_rf, test) %>% as.data.frame() %>% pull(predict)
library(caret)
confusionMatrix(pred_class, validate_data$y_var) #Accuracy : 0.7849 
# 
# Confusion Matrix and Statistics
# 
# Reference
# Prediction anomaly Normal
# anomaly    8261    277
# Normal     4572   9433
# 
# Accuracy : 0.7849          
# 95% CI : (0.7795, 0.7902)
# No Information Rate : 0.5693          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.5838          
# 
# Mcnemar's Test P-Value : < 2.2e-16       
#                                           
#             Sensitivity : 0.6437          
#             Specificity : 0.9715          
#          Pos Pred Value : 0.9676          
#          Neg Pred Value : 0.6735          
#              Prevalence : 0.5693          
#          Detection Rate : 0.3665          
#    Detection Prevalence : 0.3787          
#       Balanced Accuracy : 0.8076          
#                                           
#        'Positive' Class : anomaly         
                                    

# ROC curve and AUC: 
library(pROC) 

# Function calculates AUC: 
auc_for_test <- function(model_selected) {
  actual <- validate_data$y_var
  pred_prob <- h2o.predict(model_selected, test) %>% as.data.frame() %>% pull(anomaly)
  return(roc(actual, pred_prob))
}

# Use this function: 
my_auc <- auc_for_test(default_rf)
my_auc$auc

#Area under the curve: 0.931

# Graph ROC and AUC: 

sen_spec_df <- data_frame(TPR = my_auc$sensitivities, FPR = 1 - my_auc$specificities)

sen_spec_df %>% 
  ggplot(aes(x = FPR, ymin = 0, ymax = TPR))+
  geom_polygon(aes(y = TPR), fill = "red", alpha = 0.3)+
  geom_path(aes(y = TPR), col = "firebrick", size = 1.2) +
  geom_abline(intercept = 0, slope = 1, color = "gray37", size = 1, linetype = "dashed") + 
  theme_bw() +
  coord_equal() +
  labs(x = "FPR (1 - Specificity)", 
       y = "TPR (Sensitivity)", 
       title = "Model Performance for RF Classifier based on Test Data", 
       subtitle = paste0("AUC Value: ", my_auc$auc %>% round(2)))


# #=================================
#  Full Cartesian Grid Search
#=================================

# Set hyperparameter grid: 

hyper_grid.h2o <- list(ntrees = seq(250, 550, by = 50),
                       mtries = seq(5, 40, by = 5)
                       # max_depth = seq(10, 30, by = 10),
                       # min_rows = seq(1, 3, by = 1),
                       # nbins = seq(20, 30, by = 10),
                       #sample_rate = c(0.55, 0.632, 0.75)
)

# The number of models is 56: 
sapply(hyper_grid.h2o, length) %>% prod()

# Train  Random Forest Models: 
system.time(grid_cartesian <- h2o.grid(algorithm = "randomForest",
                                       grid_id = "rf_grid1",
                                       x = x, 
                                       y = y, 
                                       seed = 29, 
                                       nfolds = 10, 
                                       training_frame = train,
                                       stopping_metric = "AUC", 
                                       hyper_params = hyper_grid.h2o,
                                       search_criteria = list(strategy = "Cartesian")))
# Save an CARTESIAN RF MODEL OUTPUT object to a file

# system.time(grid_cartesian <- h2o.grid(algorithm = "randomForest",
#                                          +                                        grid_id = "rf_grid1",
#                                          +                                        x = x, 
#                                          +                                        y = y, 
#                                          +                                        seed = 29, 
#                                          +                                        nfolds = 10, 
#                                          +                                        training_frame = train,
#                                          +                                        stopping_metric = "AUC", 
#                                          +                                        hyper_params = hyper_grid.h2o,
#                                          +                                        search_criteria = list(strategy = "Cartesian")))
# user   system  elapsed 
# 57.51    10.13 24327.88 

saveRDS(grid_cartesian, file = "RF MODELS USING GRID SEARCH 2_ITERATION.rds")
#h2o.shutdown()

grid_cartesian <- readRDS("RF MODELS USING GRID SEARCH 2_ITERATION.rds")
grid_cartesian

# Collect the results and sort by our model performance metric of choice: 
grid_perf <- h2o.getGrid(grid_id = "rf_grid1", 
                         sort_by = "accuracy", 
                         decreasing = TRUE)
grid_perf
# H2O Grid Details
# ================
#   
#   Grid ID: rf_grid1 
# Used hyper parameters: 
#   -  mtries 
# -  ntrees 
# Number of models: 56 
# Number of failed models: 0 
# 
# Hyper-Parameter Search Summary: ordered by decreasing accuracy
# mtries ntrees         model_ids           accuracy
# 1     20    300 rf_grid1_model_12 0.9971816449666561
# 2     20    350 rf_grid1_model_20 0.9971816449666561
# 3     20    450 rf_grid1_model_36 0.9971419498253413
# 4     20    550 rf_grid1_model_52 0.9971419498253413
# 5     20    500 rf_grid1_model_44 0.9971419498253413
# 
# ---
#   mtries ntrees         model_ids           accuracy
# 51     40    400 rf_grid1_model_32  0.996268656716418
# 52     40    500 rf_grid1_model_48  0.996268656716418
# 53     40    350 rf_grid1_model_24  0.996268656716418
# 54     40    450 rf_grid1_model_40  0.996268656716418
# 55     40    250  rf_grid1_model_8 0.9961892664337885
# 56     40    300 rf_grid1_model_16 0.9961495712924738

# Best model chosen by validation error: 
best_model <- h2o.getModel(grid_perf@model_ids[[1]])

# Model Details:
#   ==============
#   
#   H2OBinomialModel: drf
# Model ID:  rf_grid1_model_12 
# Model Summary: 
#   number_of_trees number_of_internal_trees model_size_in_bytes min_depth max_depth mean_depth
# 1             300                      300              623272        13        20   17.23333
# min_leaves max_leaves mean_leaves
# 1        129        194   159.83667
# 
# 
# H2OBinomialMetrics: drf
# ** Reported on training data. **
#   ** Metrics reported on Out-Of-Bag training samples **
#   
#   MSE:  0.002417252
# RMSE:  0.04916556
# LogLoss:  0.01097108
# Mean Per-Class Error:  0.002721878
# AUC:  0.9998924
# pr_auc:  0.06036745
# Gini:  0.9997848
# R^2:  0.9902864
# 
# Confusion Matrix (vertical: actual; across: predicted) for F1-optimal threshold:
#   Normal anomaly    Error       Rate
# Normal   13409      40 0.002974  =40/13449
# anomaly     29   11714 0.002470  =29/11743
# Totals   13438   11754 0.002739  =69/25192
# 
# Maximum Metrics: Maximum metrics at their respective thresholds
# metric threshold        value idx
# 1                       max f1  0.400000     0.997063 202
# 2                       max f2  0.306874     0.997549 224
# 3                 max f0point5  0.633929     0.997778 156
# 4                 max accuracy  0.400000     0.997261 202
# 5                max precision  1.000000     1.000000   0
# 6                   max recall  0.000001     1.000000 399
# 7              max specificity  1.000000     1.000000   0
# 8             max absolute_mcc  0.400000     0.994498 202
# 9   max min_per_class_accuracy  0.425227     0.997175 196
# 10 max mean_per_class_accuracy  0.400000     0.997278 202
# 11                     max tns  1.000000 13449.000000   0
# 12                     max fns  1.000000   710.000000   0
# 13                     max fps  0.000001 13449.000000 399
# 14                     max tps  0.000001 11743.000000 399
# 15                     max tnr  1.000000     1.000000   0
# 16                     max fnr  1.000000     0.060462   0
# 17                     max fpr  0.000001     1.000000 399
# 18                     max tpr  0.000001     1.000000 399
# 
# Gains/Lift Table: Extract with `h2o.gainsLift(<model>, <data>)` or `h2o.gainsLift(<model>, valid=<T/F>, xval=<T/F>)`
# 
# H2OBinomialMetrics: drf
# ** Reported on cross-validation data. **
#   ** 10-fold cross-validation on training data (Metrics computed for combined holdout predictions) **
#   
#   MSE:  0.002621235
# RMSE:  0.051198
# LogLoss:  0.01459354
# Mean Per-Class Error:  0.00283404
# AUC:  0.9997688
# pr_auc:  0.08778887
# Gini:  0.9995375
# R^2:  0.9894668
# 
# Confusion Matrix (vertical: actual; across: predicted) for F1-optimal threshold:
#   Normal anomaly    Error       Rate
# Normal   13414      35 0.002602  =35/13449
# anomaly     36   11707 0.003066  =36/11743
# Totals   13450   11742 0.002818  =71/25192
# 
# Maximum Metrics: Maximum metrics at their respective thresholds
# metric threshold        value idx
# 1                       max f1  0.450005     0.996977 175
# 2                       max f2  0.342222     0.997106 196
# 3                 max f0point5  0.716667     0.997928 118
# 4                 max accuracy  0.450005     0.997182 175
# 5                max precision  1.000000     1.000000   0
# 6                   max recall  0.000000     1.000000 399
# 7              max specificity  1.000000     1.000000   0
# 8             max absolute_mcc  0.450005     0.994337 175
# 9   max min_per_class_accuracy  0.433889     0.997020 177
# 10 max mean_per_class_accuracy  0.450005     0.997166 175
# 11                     max tns  1.000000 13449.000000   0
# 12                     max fns  1.000000  1033.000000   0
# 13                     max fps  0.000000 13449.000000 399
# 14                     max tps  0.000000 11743.000000 399
# 15                     max tnr  1.000000     1.000000   0
# 16                     max fnr  1.000000     0.087967   0
# 17                     max fpr  0.000000     1.000000 399
# 18                     max tpr  0.000000     1.000000 399
# 
# Gains/Lift Table: Extract with `h2o.gainsLift(<model>, <data>)` or `h2o.gainsLift(<model>, valid=<T/F>, xval=<T/F>)`
# Cross-Validation Metrics Summary: 
#   mean           sd   cv_1_valid   cv_2_valid   cv_3_valid
# accuracy                   0.9976197  7.985348E-4    0.9967506     0.999185   0.99763685
# auc                          0.99977 3.4329726E-4    0.9999187   0.99999535     0.999972
# err                     0.0023803273  7.985348E-4 0.0032493907  8.149959E-4  0.002363135
# err_count                        6.0          2.0          8.0          2.0          6.0
# f0point5                  0.99750704  6.771867E-4   0.99752825   0.99860334   0.99652374
# f1                        0.99744487 8.5620576E-4   0.99647266    0.9991266    0.9975145
# f2                         0.9973837 0.0014661337    0.9954193    0.9996505    0.9985072
# lift_top_group             2.1463132  0.053250656    2.1672535     2.145105     2.107054
# logloss                  0.014586449  0.009609783  0.011049904  0.006853847  0.007787449
# max_per_class_error     0.0035422083 0.0010572101   0.00528169 0.0015267176 0.0037481259
# mcc                        0.9952176 0.0016035072   0.99346596   0.99836403    0.9952672
# mean_per_class_accuracy     0.997588 8.4825105E-4     0.996605   0.99923664     0.997711
# mean_per_class_error    0.0024120486 8.4825105E-4 0.0033949928  7.633588E-4 0.0022890007
# mse                      0.002619952  6.795504E-4 0.0029329236 0.0015272287  0.002041509
# precision                  0.9975491  9.538492E-4    0.9982332    0.9982548   0.99586433
# r2                        0.98946655 0.0027301975   0.98819804     0.993863    0.9918128
# recall                     0.9973435 0.0019309199    0.9947183          1.0    0.9991701
# rmse                     0.050793175 0.0066671083  0.054156475  0.039079774   0.04518306
# specificity                0.9978324  9.212008E-4    0.9984917    0.9984733    0.9962519
# cv_4_valid   cv_5_valid   cv_6_valid   cv_7_valid   cv_8_valid
# accuracy                   0.9980575   0.99723536    0.9976699   0.99762285    0.9964455
# auc                        0.9993258    0.9999377   0.99991554   0.99996066    0.9997285
# err                      0.001942502  0.002764613 0.0023300971  0.002377179 0.0035545023
# err_count                        5.0          7.0          6.0          6.0          9.0
# f0point5                   0.9980722    0.9978162   0.99796504   0.99699396    0.9965006
# f1                         0.9978099    0.9970625    0.9974576   0.99749374    0.9962516
# f2                        0.99754775   0.99630994    0.9969507     0.997994    0.9960027
# lift_top_group             2.2539403    2.1223805    2.1803555    2.1103678    2.1082432
# logloss                  0.025318911 0.0112300785  0.010562367   0.00831061  0.017545445
# max_per_class_error     0.0026269702  0.004191115 0.0033869601  0.003012048 0.0041631972
# mcc                         0.996065    0.9944544   0.99530834   0.99523425   0.99287224
# mean_per_class_accuracy   0.99798816   0.99715763    0.9975892   0.99765784    0.9964158
# mean_per_class_error     0.002011809 0.0028423835 0.0024108402 0.0023421445 0.0035842282
# mse                     0.0025702917 0.0030531236 0.0024512107 0.0020695538  0.003923906
# precision                 0.99824715    0.9983193   0.99830365    0.9966611   0.99666667
# r2                        0.98958665    0.9877468    0.9901276    0.9916991    0.9842629
# recall                    0.99737304    0.9958089     0.996613   0.99832773    0.9958368
# rmse                     0.050698046  0.055255078  0.049509704  0.045492347  0.062641084
# specificity               0.99860334   0.99850637   0.99856526   0.99698794   0.99699473
# cv_9_valid  cv_10_valid
# accuracy                   0.9971842    0.9984089
# auc                       0.99898005    0.9999658
# err                     0.0028157684 0.0015910899
# err_count                        7.0          4.0
# f0point5                  0.99770033   0.99736667
# f1                        0.99690676    0.9983525
# f2                        0.99611443   0.99934036
# lift_top_group             2.1941748    2.0742574
# logloss                  0.037004884  0.010200987
# max_per_class_error     0.0044130627 0.0030721966
# mcc                       0.99432576   0.99681914
# mean_per_class_accuracy    0.9970544    0.9984639
# mean_per_class_error    0.0029456296 0.0015360983
# mse                     0.0031552515 0.0024745204
# precision                  0.9982301   0.99671054
# r2                        0.98727936   0.99008924
# recall                    0.99558693          1.0
# rmse                     0.056171626   0.04974455
# specificity                0.9985218    0.9969278
# Use best model for making predictions: 

confusionMatrix(h2o.predict(best_model, test) %>% as.data.frame() %>% pull(predict), 
                validate_data$y_var)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction anomaly Normal
# anomaly    8701    278
# Normal     4132   9432
# 
# Accuracy : 0.8044          
# 95% CI : (0.7991, 0.8095)
# No Information Rate : 0.5693          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.6195          
# 
# Mcnemar's Test P-Value : < 2.2e-16       
#                                           
#             Sensitivity : 0.6780          
#             Specificity : 0.9714          
#          Pos Pred Value : 0.9690          
#          Neg Pred Value : 0.6954          
#              Prevalence : 0.5693          
#          Detection Rate : 0.3860          
#    Detection Prevalence : 0.3983          
#       Balanced Accuracy : 0.8247          
#                                           
#        'Positive' Class : anomaly         
#                                      
# 

#=================================
#  Random Discrete Grid Search
#=================================

# Set random grid search criteria: 
search_criteria_2 <- list(strategy = "RandomDiscrete",
                          stopping_metric = "AUC",
                          stopping_tolerance = 0.005,
                          stopping_rounds = 20,
                          max_runtime_secs = 30*60)


# Turn parameters for RF: 
system.time(random_grid <- h2o.grid(algorithm = "randomForest",
                                    grid_id = "rf_grid2",
                                    x = x, 
                                    y = y, 
                                    seed = 29, 
                                    nfolds = 10, 
                                    training_frame = train,
                                    hyper_params = hyper_grid.h2o,
                                    search_criteria = search_criteria_2))

# Save an CARTESIAN RF MODEL OUTPUT object to a file
saveRDS(random_grid, file = "RF MODELS USING RANDOM SEARCH 2_ITERATION.rds")
#h2o.shutdown()

random_grid <- readRDS("RF MODELS USING RANDOM SEARCH 2_ITERATION.rds")

# Collect the results and sort by our models: 
grid_perf2 <- h2o.getGrid(grid_id = "rf_grid2", 
                          sort_by = "AUC", 
                          decreasing = FALSE)

# Best RF: 
best_model2 <- h2o.getModel(grid_perf2@model_ids[[1]])

# Use best model for making predictions: 
confusionMatrix(h2o.predict(best_model2, test) %>% as.data.frame() %>% pull(predict), 
                validate_data$y_var)
#Accuracy : 

