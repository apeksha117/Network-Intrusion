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

# #creating flags for numeric variables
# for(i in 1:length(train_data)){
#   if(is.numeric(train_data[,i])==TRUE){
#     train_data[,paste(colnames(train_data)[i],"Flag",sep="_")] = ifelse(train_data[,i] > 0,1,0)
#   }else{print("NA")}}


# #--------------------------Feature selection and EDA---------------------#
# checking for distinct values and the variance of columns with numeric data and frequency distribution for categorical
##examine data distribution 
# removing columns with zero variation
summary(train_data)

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


#outlier treatment 

##Method 1
##using outliers as >1.5 IQR and capping them to 5% and 95% IQR for src_bytes
# #outlier_values_pre <- boxplot.stats(train_data$src_bytes)$out
# x <- train_data$src_bytes
# qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
# caps <- quantile(x, probs=c(.05, .95), na.rm = T)
# H <- 1.5 * IQR(x, na.rm = T)
# boxplot(x)
# x[x < (qnt[1] - H)] <- caps[1]
# x[x > (qnt[2] + H)] <- caps[2]
# boxplot(x)
# boxplot.stats(x)$out
# 
# #Method 2- detecting outliers
# #using package outliers
# #using chisquare, t and z test score along with 95 %ile 
# #This is a better process
# #install.packages("outliers")
# library(outliers)
# 
# outlier(train_data$src_bytes)
# head(sort(outlier_values_pre, decreasing = T))
# 
# a= as.data.frame()
# a= ifelse(scores(train_data$src_bytes, type="chisq", prob=0.95)==TRUE,train_data$src_bytes,0)
# a= as.data.frame(a)
# # beyond 95th %ile based on chisq
# a= subset(a, a>0)
# 
# b= data.frame()
# b= ifelse(scores(train_data$src_bytes, type="z", prob=0.95)==TRUE,train_data$src_bytes,0)
# b= as.data.frame(b)
# # beyond 95th %ile based on z-scores
# b= subset(b, b>0)
# 
# c= data.frame()
# c= ifelse(scores(train_data$src_bytes, type="t", prob=0.95)==TRUE,train_data$src_bytes,0)
# c= as.data.frame(a)
# # beyond 95th %ile based on t-scores 
# c= subset(c, c>0)
# 
# 
# d = data.frame()
# d= ifelse(scores(train_data$src_bytes, type="iqr", lim = 0.99)==TRUE,train_data$src_bytes,0)
# d= as.data.frame(d)
# # beyond 95th %ile based on iqr-scores 
# d= subset(d, d>0)
# colnames(d)[colnames(d) == "d"]= "a"
# 
# 
# Src_bytes= as.data.frame(train_data$src_bytes)
# colnames(Src_bytes)[colnames(Src_bytes) == "train_data$src_bytes"]= "a"
# 
# outlier_values_pre= as.data.frame(outlier_values_pre)
# colnames(outlier_values_pre)[colnames(outlier_values_pre) == "outlier_values_pre"]= "a"
# 
# A=setdiff(outlier_values_pre, a)
# B=setdiff(Src_bytes,outlier_values_pre)
# c=setdiff(Src_bytes,d)

##using chisqure test as it gives most apt results
##Capping to the highest value obtained at 95%iles based on chisquare( code not written)

##Method 3
##Cooks distance computes the influence exerted by each data point (row) on the predicted outcome
##Cook’s distance is a measure computed with respect to a given regression model 
## Fitting Logistic Regression to the Training set
# classifier = glm(formula = class ~ .,
#                  family = binomial,
#                  data = train_data)
# cooksd <- cooks.distance(classifier)
##In general use, those observations that have a cook’s distance greater than 4 times the mean
##may be classified as influential. This is not a hard boundary.

# cookd= as.data.frame(cooksd)
# ifelse(any(is.na(cookd))== "TRUE", 1,0)
#
# plot(cooksd, pch="*", main="Influential Obs by Cooks distance")  # plot cook's distance
# abline(h = 4*mean(cooksd, na.rm=T), col="red", cex=2)  # add cutoff line
# text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="blue")  # add labels

# #Now lets find out the influential rows from the original data
# influential = names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))]  # influential row numbers
# influential_rows=(train_data[influential, ])  # influential observations.
# #ONLY one observation 22991
#  train_data= train_data[-22991, ]
# table(influential_rows$class)
# #rerunning above code for cooks again after removing 22991


#5)	Different plots to identify relationship between variables
#PLOTTING ONLY CATEGORICAL VARIABLES AGAINST CLASS
plot(train_data$protocol_type,train_data$class, col=c("red","green"), main=("plot class vs protocol type"), xlab="protocol", ylab="class")
plot(train_data$service,train_data$class, col=c("red","green"), main=("plot class vs service"), xlab="service", ylab="class")
plot(train_data$flag,train_data$class, col=c("red","green"), main=("plot class vs flag"), xlab="flag", ylab="class")

# Finding correlation matrix
library(dplyr)
train_data1= train_data
train_data=(lapply(train_data,as.numeric))
train_data= as.data.frame(train_data) 
train_data <- subset(train_data, select = -c(class))
train_data= as.data.frame(train_data) 
# Method1
res <- cor(train_data)
round(res, 2)

#method2- along with p values of corelation  
#A t test is available to test the null hypothesis that the correlation coefficient is zero
#Note that the P value derived from the test provides no information on how strongly the 2 variables are related
#install.packages("Hmisc")
#the p-value is less than 0.05, there is strong evidence against the null hypothesis. 
#As a result, reject the null hypothesis and accept the alternative hypothesis that there is co relation between  2 variables


library("Hmisc")
res2 <- rcorr(as.matrix(train_data))

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

res3<-rcorr(as.matrix(train_data))
res4 =flattenCorrMatrix(res2$r, res2$P)
#write.csv(res4, "corrmat_pvalues.csv")

#using visual way for co relation
#install.packages("corrplot")
library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
# variables correlation>0.85 or correlation< (-.0.85) val pvalue <0.05
#1 num_compromised	num_root	0.99895649	0.00000000
#2 serror_rate	srv_serror_rate	0.99329006	0.00000000
#3 rerror_rate	srv_rerror_rate	0.98913369	0.00000000
#4 dst_host_serror_rate	dst_host_srv_serror_rate	0.98470586	0.00000000
#5 srv_serror_rate	dst_host_srv_serror_rate	0.98462120	0.00000000
#6 serror_rate	dst_host_srv_serror_rate	0.97960195	0.00000000
#7 serror_rate	dst_host_serror_rate	0.97802184	0.00000000
#8 srv_serror_rate	dst_host_serror_rate	0.97636331	0.00000000
#9 srv_rerror_rate	dst_host_srv_rerror_rate	0.96984142	0.00000000
#10 rerror_rate	dst_host_srv_rerror_rate	0.96488604	0.00000000
#11 rerror_rate	dst_host_rerror_rate	0.92899197	0.00000000
#12 dst_host_rerror_rate	dst_host_srv_rerror_rate	0.92539043	0.00000000
#13 srv_rerror_rate	dst_host_rerror_rate	0.91953674	0.00000000
#14 dst_host_srv_count	dst_host_same_srv_rate	0.89551156	0.00000000
#install.packages("ggplot2")

# ploting to check relationship between variables having corelation greater than 0.9



library(ggplot2)
qplot(num_compromised,	num_root, data = train_data, 
      xlab = "num_compromised", ylab = "num_root",
      main = "num_compromised vs. num_root in train data")
#1 positive relationship between num_compromised,	num_root

qplot(serror_rate, srv_serror_rate, data = train_data, 
      xlab = "serror_rate", ylab = "srv_serror_rate",
      main = "serror_rate vs. srv_serror_rate in train data")
#2 no non linear relation between serror_rate, srv_serror_rat

qplot(rerror_rate, srv_rerror_rate, data = train_data, 
      xlab = "rerror_rate", ylab = "srv_rerror_rate",
      main = "rerror_rate vs. srv_rerror_rate in train data")
#3 moderate positive co relation seems rerror_rate, srv_rerror_rate

qplot(dst_host_serror_rate, dst_host_srv_serror_rate, data = train_data, 
      xlab = "dst_host_serror_rate", ylab = "dst_host_srv_serror_rate",
      main = "dst_host_serror_rate vs. dst_host_srv_serror_rate in train data")
#4moderate  positive co relation seems dst_host_serror_rate, dst_host_srv_serror_rate

qplot(srv_serror_rate,	dst_host_srv_serror_rate, data = train_data, 
      xlab = "srv_serror_rate", ylab = "dst_host_srv_serror_rate",
      main = "srv_serror_rate vs. dst_host_srv_serror_rate in train data")
#5 no co relation seems srv_serror_rate	dst_host_srv_serror_rate

qplot(serror_rate,	dst_host_srv_serror_rate, data = train_data, 
      xlab = "serror_rate", ylab = "dst_host_srv_serror_rate",
      main = "serror_rate vs. dst_host_srv_serror_rate in train data")
#6   no co relation seems serror_rate	dst_host_srv_serror_rate

qplot(serror_rate,	dst_host_serror_rate, data = train_data, 
      xlab = "serror_rate", ylab = "dst_host_serror_rate",
      main = "serror_rate vs. dst_host_serror_rate in train data")
#7 no co relation seems serror_rate	dst_host_serror_rate

qplot(srv_serror_rate,	dst_host_serror_rate, data = train_data, 
      xlab = "srv_serror_rate", ylab = "dst_host_serror_rate",
      main = "srv_serror_rate vs. dst_host_serror_rate in train data")
#8 no co relation seems srv_serror_rate	dst_host_serror_rate

qplot(srv_rerror_rate,	dst_host_srv_rerror_rate, data = train_data, 
      xlab = "srv_rerror_rate", ylab = "dst_host_srv_rerror_rate",
      main = "srv_rerror_rate vs. dst_host_srv_rerror_rate in train data")
#9 no co relation seems srv_rerror_rate	dst_host_srv_rerror_rate

qplot(rerror_rate, dst_host_srv_rerror_rate, data = train_data, 
      xlab = "rerror_rate", ylab = "dst_host_srv_rerror_rate",
      main = "rerror_rate vs. dst_host_srv_rerror_rate in train data")
#10 no co relation seems rerror_rate	dst_host_srv_rerror_rate

qplot(rerror_rate,	dst_host_rerror_rate, data = train_data, 
      xlab = "rerror_rate", ylab = "dst_host_rerror_rate",
      main = "rerror_rate vs. dst_host_rerror_rate in train data")
#11 moderate  positive co relation seems rerror_rate	dst_host_rerror_rate

qplot(dst_host_rerror_rate,	dst_host_srv_rerror_rate, data = train_data, 
      xlab = "dst_host_rerror_rate", ylab = "dst_host_srv_rerror_rate",
      main = "dst_host_rerror_rate vs. dst_host_srv_rerror_rate in train data")
#12 moderate  positive co relation seems ddst_host_rerror_rate	dst_host_srv_rerror_rate

qplot( srv_rerror_rate,	dst_host_rerror_rate, data = train_data, 
       xlab = "srv_rerror_rate", ylab = "dst_host_rerror_rate",
       main = "srv_rerror_rate vs. dst_host_rerror_rate in train data")
#13 no co relation seems  srv_rerror_rate	dst_host_rerror_rate

qplot(dst_host_srv_count,	dst_host_same_srv_rate, data = train_data, 
      xlab = "dst_host_srv_count", ylab = "dst_host_same_srv_rate",
      main = "dst_host_srv_count vs. dst_host_same_srv_rate in train data")
#14 moderate  positive co relation seems dst_host_srv_count	dst_host_same_srv_rate

# Not Removing any variables as the plots do not show direct relationship
#preparation for tree fitting

# handling dependent var
train_data= cbind(train_data[,-(1:3)], train_data1[,1:4])
class(train_data)
train_data= as.data.frame(train_data)

train_data$class1= ifelse(train_data$class=="anomaly", 1, 0)
train_data$class= NULL
names(train_data)[names(train_data)== "class1"]= "y_var"

validate_data$class1= ifelse(validate_data$class=="anomaly", 1, 0)
validate_data$class= NULL
names(validate_data)[names(validate_data)== "class1"]= "y_var"

# train_data$y_var= as.factor(train_data$y_var)
# validate_data$y_var= as.factor(validate_data$y_var)


validate_data= subset(validate_data,validate_data$service !="tftp_u" )

validate_data= validate_data[, names(validate_data) %in% names(train_data)]

test_data= subset(test_data,test_data$service !="tftp_u" )
test_data= test_data[, names(test_data) %in% names(test_data)]


#6)	Fit Decision Tree model on training data 

## iteration 1
#no need of feature scaling in decision trees
# Fitting Decision Tree Classification to the Training set
#install.packages('rpart')
library(rpart)
#colnames(train_data)[colnames(train_data)== "train_data1$class"]= "class"
classifier = rpart(formula = train_data$y_var~.,
                   data = train_data, method="class")

y_pred = predict(classifier, newdata = validate_data[,!(names(validate_data) %in% c("y_var"))],type ="class")

# Making the Confusion Matrix to test accuracy
cm = table(validate_data$y_var, y_pred)
accuracy= (cm[1,1] + cm[2,2])/ sum(cm)
#0.7583285
print(accuracy)
plot(classifier)
text(classifier, pretty = T, cex=0.5)
#Method 1
library(rattle)
library(rpart.plot)
library(RColorBrewer)
prp(classifier,box.col=c("Grey", "Orange")[classifier$frame$yval],varlen=0,faclen=0, type=1,extra=4,under=TRUE)
# improve the visualization method 2
#install.packages("rattle")
#install.packages("https://cran.r-project.org/bin/windows/contrib/3.3/RGtk2_2.20.31.zip", repos=NULL)

fancyRpartPlot(classifier)

# post pruning using cp- complexity parameter and the cross validation error values
printcp(classifier)
plotcp(classifier)
#select the one having the least cross-validated error- xerror and use it to prune the tree
#From the above mentioned list of cp values-
#Prune the tree to create an optimal decision tree :
cp_prune=min(classifier$cptable[,"xerror"])

ptree<- prune(classifier, cp= classifier$cptable[which.min(classifier$cptable[,"xerror"]),"CP"])
fancyRpartPlot(ptree, uniform=TRUE,main="Pruned Classification Tree")
prp(ptree,box.col=c("Grey", "Orange")[ptree$frame$yval],varlen=0,faclen=0, type=1,extra=4,under=TRUE, main="Pruned Classification Tree")

# Predicting the Test set results using pruned tree

y_pred_prune = predict(ptree, newdata = validate_data[,!(names(validate_data) %in% c("y_var"))], type = 'class')

# Making the Confusion Matrix to test accuracy
cm = table(validate_data$y_var, y_pred_prune)
accuracy= (cm[1,1] + cm[2,2])/ sum(cm)
print(accuracy)
#0.7583285

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
#0.7671117



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
plot(m1)
# number of trees with lowest errorr rate
min(m1$err.rate)
#0.0008179047
which.min(m1$err.rate)
#1159
# randomForest(formula = train_data$y_var ~ ., data = train_data) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 6
# 
# OOB estimate of  error rate: 0.21%
# Confusion matrix:
#   anomaly Normal  class.error
# anomaly   11701     42 0.0035765988
# Normal       11  13438 0.0008179047

y_pred = predict(m1, newdata = validate_data[,!(names(validate_data) %in% c("y_var"))],type ="class")

# Making the Confusion Matrix to test accuracy
cm = table(validate_data$y_var, y_pred)
accuracy= (cm[1,1] + cm[2,2])/ sum(cm)
#0.7818835
print(accuracy)

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
  ntreeTry   = 500,
  mtryStart  = 5,
  stepFactor = 2,
  improve    = 0.01,
  trace      = TRUE     # to not show real-time progress 
)

# mtry = 5  OOB error = 0.23% 
# Searching left ...
# mtry = 3 	OOB error = 0.39% 
# -0.7017544 0.01 
# Searching right ...
# mtry = 10 	OOB error = 0.21% 
# 0.0877193 0.01 
# mtry = 20 	OOB error = 0.21% 
# 0 0.01 

m1 <- randomForest(
  formula = train_data$y_var ~ .,
  data    = train_data,
  ntreeTry   = 500,
  mtry =10
)
# 
# Call:
#   randomForest(formula = train_data$y_var ~ ., data = train_data,      ntreeTry = 500, mtry = 10) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 10
# 
# OOB estimate of  error rate: 0.2%
# Confusion matrix:
#   anomaly Normal  class.error
# anomaly   11704     39 0.0033211275
# Normal       12  13437 0.0008922596
#shows 10 is best value of mtry
y_pred = predict(m1, newdata = validate_data[,!(names(validate_data) %in% c("y_var"))],type ="class")

# Making the Confusion Matrix to test accuracy
cm = table(validate_data$y_var, y_pred)
accuracy= (cm[1,1] + cm[2,2])/ sum(cm)
#0.7803753
print(accuracy)
#printing important variables
library(caret)
RM_IMP=varImp(m1, scale= F)
#                                Overall
# duration                      49.4261576
# src_bytes                   2999.5515525
# dst_bytes                   2074.7768429
# land                           0.2339559
# wrong_fragment                43.6978770
# urgent                         0.5824167
# hot                          142.3554326
# num_failed_logins              1.6534107
# logged_in                    232.8679960
# num_compromised               61.9461862
# root_shell                     1.5790513
# su_attempted                   0.4423577
# num_root                       4.3529941
# num_file_creations             2.6030679
# num_shells                     0.1645801
# num_access_files               1.1341470
# is_guest_login                10.9457314
# count                        357.0761788
# srv_count                    184.6463908
# serror_rate                   36.0885258
# srv_serror_rate               41.4024195
# rerror_rate                   26.6749253
# srv_rerror_rate               28.2522494
# same_srv_rate                897.8535516
# diff_srv_rate                675.8712223
# srv_diff_host_rate            12.3149802
# dst_host_count               140.8915277
# dst_host_srv_count           432.3116743
# dst_host_same_srv_rate       633.7744701
# dst_host_diff_srv_rate       292.3758887
# dst_host_same_src_port_rate  326.2689208
# dst_host_srv_diff_host_rate  180.7509818
# dst_host_serror_rate          95.2793482
# dst_host_srv_serror_rate     157.5951248
# dst_host_rerror_rate          95.5576981
# dst_host_srv_rerror_rate      50.2158056
# protocol_type                509.2779018
# service                      275.4199624
# flag                        1450.2310820

#To get the area under the ROC curve for each predictor, the filterVarImp function can be used
roc_imp <- filterVarImp(x = train_data[, -ncol(train_data)], y = train_data$y_var)
head(roc_imp)
# anomaly    Normal
# duration       0.5405969 0.5405969
# src_bytes      0.8986273 0.8986273
# dst_bytes      0.8997132 0.8997132
# land           0.5000054 0.5000054
# wrong_fragment 0.5095376 0.5095376
# urgent         0.5000426 0.5000426

varImpPlot(m1,type=2)

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
# Model Details:
#   ==============
#   
#   H2OBinomialModel: drf
# Model ID:  DRF_model_R_1568829412999_1 
# Model Summary: 
#   number_of_trees number_of_internal_trees model_size_in_bytes min_depth max_depth mean_depth
# 1              42                       42              132595        18        20   19.85714
# min_leaves max_leaves mean_leaves
# 1        184        301   244.26190
# 
# 
# H2OBinomialMetrics: drf
# ** Reported on training data. **
#   ** Metrics reported on Out-Of-Bag training samples **
#   
#   MSE:  0.00336279
# RMSE:  0.05798957
# LogLoss:  0.01867103
# Mean Per-Class Error:  0.003275399
# AUC:  0.9997497
# pr_auc:  0.4542022
# Gini:  0.9994994
# R^2:  0.9864869
# 
# Confusion Matrix (vertical: actual; across: predicted) for F1-optimal threshold:
#   Normal anomaly    Error       Rate
# Normal   13409      40 0.002974  =40/13449
# anomaly     42   11701 0.003577  =42/11743
# Totals   13451   11741 0.003255  =82/25192
# 
# Maximum Metrics: Maximum metrics at their respective thresholds
# metric threshold        value idx
# 1                       max f1  0.421103     0.996508 206
# 2                       max f2  0.320316     0.996614 229
# 3                 max f0point5  0.579344     0.997400 176
# 4                 max accuracy  0.421103     0.996745 206
# 5                max precision  1.000000     1.000000   0
# 6                   max recall  0.000003     1.000000 399
# 7              max specificity  1.000000     1.000000   0
# 8             max absolute_mcc  0.421103     0.993460 206
# 9   max min_per_class_accuracy  0.414196     0.996509 207
# 10 max mean_per_class_accuracy  0.421103     0.996725 206
# 11                     max tns  1.000000 13449.000000   0
# 12                     max fns  1.000000  5336.000000   0
# 13                     max fps  0.000003 13449.000000 399
# 14                     max tps  0.000003 11743.000000 399
# 15                     max tnr  1.000000     1.000000   0
# 16                     max fnr  1.000000     0.454398   0
# 17                     max fpr  0.000003     1.000000 399
# 18                     max tpr  0.000003     1.000000 399
# 
# Gains/Lift Table: Extract with `h2o.gainsLift(<model>, <data>)` or `h2o.gainsLift(<model>, valid=<T/F>, xval=<T/F>)`
# 
# H2OBinomialMetrics: drf
# ** Reported on cross-validation data. **
#   ** 10-fold cross-validation on training data (Metrics computed for combined holdout predictions) **
#   
#   MSE:  0.003232501
# RMSE:  0.05685509
# LogLoss:  0.01766965
# Mean Per-Class Error:  0.003308435
# AUC:  0.9997742
# pr_auc:  0.5244722
# Gini:  0.9995484
# R^2:  0.9870104
# 
# Confusion Matrix (vertical: actual; across: predicted) for F1-optimal threshold:
#   Normal anomaly    Error       Rate
# Normal   13423      26 0.001933  =26/13449
# anomaly     55   11688 0.004684  =55/11743
# Totals   13478   11714 0.003215  =81/25192
# 
# Maximum Metrics: Maximum metrics at their respective thresholds
# metric threshold        value idx
# 1                       max f1  0.486676     0.996547 187
# 2                       max f2  0.261041     0.996684 237
# 3                 max f0point5  0.651831     0.997738 157
# 4                 max accuracy  0.486676     0.996785 187
# 5                max precision  0.999997     1.000000   0
# 6                   max recall  0.000011     1.000000 399
# 7              max specificity  0.999997     1.000000   0
# 8             max absolute_mcc  0.486676     0.993541 187
# 9   max min_per_class_accuracy  0.400171     0.996253 205
# 10 max mean_per_class_accuracy  0.486676     0.996692 187
# 11                     max tns  0.999997 13449.000000   0
# 12                     max fns  0.999997  6161.000000   0
# 13                     max fps  0.000011 13449.000000 399
# 14                     max tps  0.000011 11743.000000 399
# 15                     max tnr  0.999997     1.000000   0
# 16                     max fnr  0.999997     0.524653   0
# 17                     max fpr  0.000011     1.000000 399
# 18                     max tpr  0.000011     1.000000 399
# 
# Gains/Lift Table: Extract with `h2o.gainsLift(<model>, <data>)` or `h2o.gainsLift(<model>, valid=<T/F>, xval=<T/F>)`
# Cross-Validation Metrics Summary: 
#   mean           sd   cv_1_valid   cv_2_valid   cv_3_valid
# accuracy                   0.9972592 0.0010836659   0.99634445   0.99796253     0.997243
# auc                        0.9997894   3.14818E-4   0.99989444    0.9999733    0.9999549
# err                      0.002740793 0.0010836659 0.0036555645 0.0020374898  0.002756991
# err_count                        6.9     2.726414          9.0          5.0          7.0
# f0point5                   0.9974869 0.0017142587   0.99473685   0.99859846    0.9983347
# f1                        0.99705005 0.0011796576   0.99604744    0.9978118    0.9970894
# f2                         0.9966152 0.0012417237    0.9973615    0.9970264   0.99584717
# lift_top_group             2.1463132  0.053250656    2.1672535     2.145105     2.107054
# logloss                  0.017695302  0.008966543  0.014525801  0.012627885  0.012967503
# max_per_class_error      0.004200554 0.0014696722  0.005279035 0.0034965035 0.0049792533
# mcc                        0.9944928 0.0021809018    0.9926557    0.9959086    0.9944784
# mean_per_class_accuracy   0.99719375 0.0010688769    0.9964802    0.9978701    0.9971356
# mean_per_class_error    0.0028062342 0.0010688769  0.003519799 0.0021299312  0.002864439
# mse                     0.0032328921 7.0491503E-4 0.0032400275 0.0026423738 0.0027382458
# precision                  0.9977792 0.0022171175     0.993865    0.9991236   0.99916667
# r2                         0.9870008  0.002844103   0.98696226    0.9893819    0.9890187
# recall                    0.99632627 0.0016070324   0.99823946    0.9965035   0.99502075
# rmse                      0.05655591 0.0061753355   0.05692124  0.051404025   0.05232825
# specificity               0.99806124   0.00193984   0.99472094   0.99923664   0.99925035
# cv_4_valid   cv_5_valid   cv_6_valid   cv_7_valid   cv_8_valid
# accuracy                   0.9965035    0.9980253    0.9984466   0.99841523   0.99526066
# auc                        0.9996603    0.9999408    0.9999684    0.9999238    0.9996547
# err                     0.0034965035 0.0019747235 0.0015533981 0.0015847861 0.0047393367
# err_count                        9.0          5.0          4.0          4.0         12.0
# f0point5                  0.99631965   0.99865615   0.99932075    0.9988279    0.9945119
# f1                         0.9960578    0.9979018   0.99830365   0.99832636    0.9950083
# f2                         0.9957961   0.99714863    0.9972886    0.9978254    0.9955053
# lift_top_group             2.2539403    2.1223805    2.1803555    2.1103678    2.1082432
# logloss                  0.018466696   0.01642338   0.01219582  0.012899775  0.020221207
# max_per_class_error      0.004378284  0.003352892 0.0033869601 0.0025083611 0.0052592037
# mcc                       0.99291676    0.9960396    0.9968759    0.9968227    0.9904983
# mean_per_class_accuracy    0.9964142   0.99795014    0.9983065   0.99836934    0.9952888
# mean_per_class_error    0.0035857898  0.002049859 0.0016934801 0.0016306866 0.0047112005
# mse                     0.0037453044 0.0034367652 0.0023069514 0.0024649769  0.004412144
# precision                  0.9964943   0.99915963          1.0    0.9991625    0.9941812
# r2                        0.98482615   0.98620707   0.99070865    0.9901131   0.98230475
# recall                    0.99562174    0.9966471     0.996613   0.99749166    0.9958368
# rmse                     0.061198894   0.05862393  0.048030734  0.049648535  0.066423975
# specificity                0.9972067   0.99925315          1.0     0.999247    0.9947408
# cv_9_valid  cv_10_valid
# accuracy                  0.99637973    0.9980111
# auc                        0.9989631   0.99996006
# err                     0.0036202734 0.0019888624
# err_count                        9.0          5.0
# f0point5                   0.9978712    0.9976913
# f1                        0.99601597   0.99793816
# f2                        0.99416757    0.9981851
# lift_top_group             2.1941748    2.0742574
# logloss                   0.04206439  0.014560562
# max_per_class_error     0.0070609003 0.0023041475
# mcc                       0.99271464    0.9960176
# mean_per_class_accuracy       0.9961   0.99802285
# mean_per_class_error    0.0038999992 0.0019771561
# mse                     0.0040911194 0.0032510138
# precision                  0.9991119   0.99752676
# r2                         0.9835064   0.98697925
# recall                     0.9929391   0.99834985
# rmse                      0.06396186   0.05701766
# specificity                0.9992609   0.99769586

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
# Accuracy       AUC Precision Specificity    Recall    Logloss
# 1  0.9963445 0.9998944 0.9938650   0.9947209 0.9982395 0.01452580
# 2  0.9979625 0.9999733 0.9991236   0.9992366 0.9965035 0.01262789
# 3  0.9972430 0.9999549 0.9991667   0.9992503 0.9950208 0.01296750
# 4  0.9965035 0.9996603 0.9964943   0.9972067 0.9956217 0.01846670
# 5  0.9980253 0.9999408 0.9991596   0.9992532 0.9966471 0.01642338
# 6  0.9984466 0.9999684 1.0000000   1.0000000 0.9966130 0.01219582
# 7  0.9984152 0.9999238 0.9991625   0.9992470 0.9974917 0.01289978
# 8  0.9952607 0.9996547 0.9941812   0.9947408 0.9958368 0.02022121
# 9  0.9963797 0.9989631 0.9991119   0.9992609 0.9929391 0.04206439
# 10 0.9980111 0.9999601 0.9975268   0.9976959 0.9983498 0.01456056

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
confusionMatrix(pred_class, validate_data$y_var) #Accuracy : 0.7939
# Confusion Matrix and Statistics
# 
# Reference
# Prediction anomaly Normal
# anomaly    8471    283
# Normal     4362   9427
# 
# Accuracy : 0.7939          
# 95% CI : (0.7886, 0.7992)
# No Information Rate : 0.5693          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.6003          
# 
# Mcnemar's Test P-Value : < 2.2e-16       
#                                           
#             Sensitivity : 0.6601          
#             Specificity : 0.9709          
#          Pos Pred Value : 0.9677          
#          Neg Pred Value : 0.6837          
#              Prevalence : 0.5693          
#          Detection Rate : 0.3758          
#    Detection Prevalence : 0.3883          
#       Balanced Accuracy : 0.8155          
#                                           
#        'Positive' Class : anomaly         
# 
# 


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
#Area under the curve: 0.951

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

hyper_grid.h2o <- list(ntrees = seq(300, 500, by = 50),
                       mtries = seq(6, 12, by = 1)
                       # max_depth = seq(10, 30, by = 10),
                       # min_rows = seq(1, 3, by = 1),
                       # nbins = seq(20, 30, by = 10),
                       #sample_rate = c(0.55, 0.632, 0.75)
                       )

# The number of models is 35: 
sapply(hyper_grid.h2o, length) %>% prod()

# Train  Random Forest Models: 
# system.time(grid_cartesian <- h2o.grid(algorithm = "randomForest",
#                                        grid_id = "rf_grid1",
#                                        x = x, 
#                                        y = y, 
#                                        seed = 29, 
#                                        nfolds = 10, 
#                                        training_frame = train,
#                                        stopping_metric = "AUC", 
#                                        hyper_params = hyper_grid.h2o,
#                                        search_criteria = list(strategy = "Cartesian")))

grid_cartesian <- readRDS("RF MODELS USING GRID SEARCH 1_ITERATION.rds")




# Collect the results and sort by our model performance metric of choice: 
grid_perf <- h2o.getGrid(grid_id = "rf_grid1", 
                         sort_by = "accuracy", 
                        decreasing = TRUE)
# H2O Grid Details
# ================
#   
#   Grid ID: rf_grid1 
# Used hyper parameters: 
#   -  mtries 
# -  ntrees 
# Number of models: 35 
# Number of failed models: 0 
# 
# Hyper-Parameter Search Summary: ordered by decreasing accuracy
# mtries ntrees         model_ids           accuracy
# 1     11    400 rf_grid1_model_20 0.9974992060971737
# 2     11    450 rf_grid1_model_27  0.997459510955859
# 3     11    350 rf_grid1_model_13  0.997459510955859
# 4     11    500 rf_grid1_model_34  0.997459510955859
# 5     12    450 rf_grid1_model_28  0.997459510955859
# 
# ---
#   mtries ntrees         model_ids           accuracy
# 30      7    450 rf_grid1_model_23 0.9971816449666561
# 31      6    500 rf_grid1_model_29 0.9970228644013973
# 32      6    300  rf_grid1_model_1 0.9969831692600826
# 33      6    450 rf_grid1_model_22 0.9969831692600826
# 34      6    400 rf_grid1_model_15 0.9969831692600826
# 35      6    350  rf_grid1_model_8 0.9969434741187678

# Best model chosen by validation error: 
best_model <- h2o.getModel(grid_perf@model_ids[[1]])

# Use best model for making predictions: 
confusionMatrix(h2o.predict(best_model, test) %>% as.data.frame() %>% pull(predict), 
               validate_data$y_var,)
#Accuracy : 0.7832

# Compare: 
confusionMatrix(h2o.predict(default_rf, test) %>% as.data.frame() %>% pull(predict), 
                validate_data$y_var)
# Accuracy : 0.789 


# #=================================
# #  Random Discrete Grid Search
# #=================================
# 
# # Set random grid search criteria: 
# search_criteria_2 <- list(strategy = "RandomDiscrete",
#                           stopping_metric = "AUC",
#                           stopping_tolerance = 0.005,
#                           stopping_rounds = 20,
#                           max_runtime_secs = 30*60)
# 
# 
# # Turn parameters for RF: 
# system.time(random_grid <- h2o.grid(algorithm = "randomForest",
#                                     grid_id = "rf_grid2",
#                                     x = x, 
#                                     y = y, 
#                                     seed = 29, 
#                                     nfolds = 10, 
#                                     training_frame = train,
#                                     hyper_params = hyper_grid.h2o,
#                                     search_criteria = search_criteria_2))
# 
# # Collect the results and sort by our models: 
# grid_perf2 <- h2o.getGrid(grid_id = "rf_grid2", 
#                           sort_by = "AUC", 
#                           decreasing = FALSE)
# 
# # Best RF: 
# best_model2 <- h2o.getModel(grid_perf2@model_ids[[1]])
# 
# # Use best model for making predictions: 
# confusionMatrix(h2o.predict(best_model2, test) %>% as.data.frame() %>% pull(predict), 
#                 validate_data$y_var)
# #Accuracy : 0.7832
# 
# # Compare: 
# confusionMatrix(h2o.predict(default_rf, test) %>% as.data.frame() %>% pull(predict), 
#                 validate_data$y_var)
# # Accuracy : 0.789 
# 
# # Save an CARTESIAN RF MODEL OUTPUT object to a file
# saveRDS(grid_cartesian, file = "RF MODELS USING RANDOM SEARCH 1_ITERATION.rds")
# #h2o.shutdown()
# 
# #AAy_data <- readRDS("RF MODELS USING GRID SEARCH 1_ITERATION.rds")
# 
# 
