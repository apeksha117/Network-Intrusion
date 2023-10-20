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

#-------------------------------- feature selection-- Boruta----------------------#
#wrapper methods is to use Boruta package that finds the importance of a feature 
#by creating shadow features
# Firstly, it adds randomness to the given data set by creating shuffled copies of all features (which are called shadow features).
# Then, it trains a random forest classifier on the extended data set and applies a feature importance measure (the default is Mean Decrease Accuracy) to evaluate the importance of each feature where higher means more important.
# At every iteration, it checks whether a real feature has a higher importance than the best of its shadow features (i.e. whether the feature has a higher Z-score than the maximum Z-score of its shadow features) and constantly removes features which are deemed highly unimportant.
# Finally, the algorithm stops either when all features get confirmed or rejected or it reaches a specified limit of random forest runs.

#install.packages("Boruta")
library(Boruta)

#note- It’s important to treat missing values prior to implementing boruta package

#Boruta is a feature ranking and selection algorithm based on random forests algorithm.
#The advantage with Boruta is that it clearly decides if a variable is important or not 
#and helps to select variables that are statistically significant. 
#Besides, you can adjust the strictness of the algorithm by adjusting the p values that defaults to 0.01 and the maxRuns.

#maxRuns is the number of times the algorithm is run. 
#The higher the maxRuns the more selective you get in picking the variables.
#The default value is 100.

#In the process of deciding if a feature is important or not, some features may be marked by Boruta as 'Tentative'. 
#Sometimes increasing the maxRuns can help resolve the 'Tentativeness' of the feature.

#The doTrace argument controls the amount of output printed to the console. 
#Higher the value, more the log details you get. 

# Decide if a variable is important or not using Boruta


set.seed(123)
#boruta_output <- Boruta(y_var ~ ., data=na.omit(train_data), doTrace=2)  # perform Boruta search

#saveRDS(boruta_output, file = "boruta feature selection 3_ITERATION.rds")

boruta_output <- readRDS("boruta feature selection 3_ITERATION.rds")



names(boruta_output)
boruta_output


# oruta performed 99 iterations in 1.465729 hours.
# 62 attributes confirmed important: count, diff_srv_rate, diff_srv_rate_Flag,
# dst_bytes, dst_bytes_Flag and 57 more;
# 14 attributes confirmed unimportant: count_Flag, dst_host_count_Flag,
# dst_host_srv_count_Flag, is_host_login, is_host_login_Flag and 9 more;
# 3 tentative attributes left: num_access_files, num_access_files_Flag, su_attempted;


# Get significant variables including tentatives
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif)  

# Do a tentative rough fix
roughFixMod <- TentativeRoughFix(boruta_output)
print(roughFixMod)
boruta_signif <- getSelectedAttributes(roughFixMod)
print(boruta_signif)

# Variable Importance Scores
imps <- attStats(roughFixMod)
class(imps)
print(imps)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2[order(-imps2$meanImp), ])  # descending sort

# Plot variable importance
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  
# 
# #Here I’m adding the attributes to the x-axis vertically.
# plot(boruta_output, xlab = "", xaxt = "n")
# lz<-lapply(1:ncol(boruta_output$ImpHistory),function(i)
# boruta_output$ImpHistory[is.finite(boruta_output$ImpHistory[,i]),i])
# names(lz) <- colnames(boruta_output$ImpHistory)
# Labels <- sort(sapply(lz,median))
# axis(side = 1,las=2,labels = names(Labels),
#        at = 1:ncol(boruta_output$ImpHistory), cex.axis = 0.7)

#subsetting train for only significant variables
fin_var= vector()
fin_var= rownames(imps2)
train_data= train_data[,names(train_data) %in% c(fin_var,"y_var")]
names(train_data)

#---------------Making data ready to fit model-----------------------------------#
validate_data= subset(validate_data,validate_data$service !="tftp_u" )

validate_data= validate_data[, names(validate_data) %in% names(train_data)]

test_data= subset(test_data,test_data$service !="tftp_u" )
test_data= test_data[, names(test_data) %in% names(test_data)]

#---------------------------------------------- model building-------------------------------#
# Iteration 3
#6)	Fit Decision Tree model on training data 

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

#0.7583285
plot(classifier)
text(classifier)
# improve the visualization
#Method 1
prp(classifier,box.col=c("Grey", "Orange")[classifier$frame$yval],varlen=0,faclen=0, type=1,extra=4,under=TRUE)
# improve the visualization method 2
#install.packages("rattle")
#install.packages("https://cran.r-project.org/bin/windows/contrib/3.3/RGtk2_2.20.31.zip", repos=NULL)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(classifier)

#----------------------post pruning using cp-------------------#
#method 1

# post pruning using cp- complexity parameter and the cross validation error values
printcp(classifier)
plotcp(classifier)
#select the one having the least cross-validated error- xerror and use it to prune the tree
#From the above mentioned list of cp values-
#Prune the tree to create an optimal decision tree :
cp_value= min(classifier$cptable[,"xerror"])
cp_value
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

#method 2

#Hyper parameter tuning only gives cp
# Applying Grid Search to find the best parameters

#using grid search cv to hypertune param in DT

#install.packages('caret')
train_data5=train_data
train_data$class1= as.factor(ifelse(train_data$y_var== 1, "anomaly", "normal"))

train_data$y_var= NULL
names(train_data)[names(train_data)== "class1"]= "y_var"

library(caret)
classifier = train(form = y_var ~ ., data = train_data, method = 'rpart')
classifier
classifier$bestTune

# Predicting the Test set results

y_pred = predict.train(classifier, newdata = validate_data[,!(names(validate_data) %in% c("y_var"))])

# Making the Confusion Matrix to test accuracy
cm = table(validate_data$y_var, y_pred)
accuracy= (cm[1,1] + cm[2,2])/ sum(cm)
print(accuracy)
#0.1711396

#----------------------------------pre pruning-----------------------#

#prepruning
#Minimum samples for a node split- minsplit
#Defines the minimum number of samples (or observations) required in a node to be considered for splitting.
#Used to control over-fitting BUT HIGH VALUE CAN LEAD TO UNDER FITTING

#minsplit	
#the minimum number of observations that must exist in a node in order for a split to be attempted.

#Minimum samples for a terminal node (leaf)-minbucket	
#the minimum number of observations in any terminal <leaf> node. 
#If only one of minbucket or minsplit is specified, the code either sets minsplit to minbucket*3 or minbucket to minsplit/3, as appropriate.
#Generally lower values should be chosen for imbalanced class problems because the regions in which the minority class will be in majority will be very small.

#cp	
#complexity parameter. Any split that does not decrease the overall lack of fit by a factor of cp is not attempted. 
#For instance, with anova splitting, this means that the overall R-squared must increase by cp at each step. 
#The main role of this parameter is to save computing time by pruning off splits that are obviously not worthwhile.
#Essentially,the user informs the program that any split which does not improve the fit by cp will likely be pruned off by cross-validation,
#and that hence the program need not pursue it.

#Maximum depth of tree (vertical depth)- maxdepth	
#Set the maximum depth of any node of the final tree, with the root node counted as depth 0. 
#Values greater than 30 rpart will give nonsense results on 32-bit machines.#Should be tuned using CV.

#xval	
#number of cross-validations.
# 
# Maximum number of terminal nodes
# The maximum number of terminal nodes or leaves in a tree.
# Can be defined in place of max_depth. Since binary trees are created, a depth of ‘n’ would produce a maximum of 2^n leaves.

# Maximum features to consider for split
# The number of features to consider while searching for a best split. These will be randomly selected.
# As a thumb-rule, square root of the total number of features works great but we should check upto 30-40% of the total number of features.
# Higher values can lead to over-fitting but depends on case to case.

# Grow a tree with minsplit of 100 and max depth of 8
#rpart.control(minsplit = 20, minbucket = round(minsplit/3), cp = 0.01, 
#              maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10,
#              surrogatestyle = 0, maxdepth = 30, ...)


classifier <- rpart(formula = y_var~.,
                    data = train_data, method="class", control=rpart.control(cp = 0, maxdepth = 6, minsplit = 60))
prp(classifier,box.col=c("Grey", "Orange")[classifier$frame$yval],varlen=0,faclen=0, type=1,extra=4,under=TRUE, main="Pruned Classification Tree")

# Compute the accuracy of the pruned tree
y_pred_prune_pre = predict(classifier, newdata = validate_data[,!(names(validate_data) %in% c("y_var"))], type = 'class')

cm = table(validate_data$y_var, y_pred_prune_pre)
accuracy= (cm[1,1] + cm[2,2])/ sum(cm)
print(accuracy)
#0.7821053

library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(classifier)
prp(classifier,box.col=c("Grey", "Orange")[classifier$frame$yval],varlen=0,faclen=0, type=1,extra=4,under=TRUE)
# improve the visualization method 2
#install.packages("rattle")
#install.packages("https://cran.r-project.org/bin/windows/contrib/3.3/RGtk2_2.20.31.zip", repos=NULL)


#0.7816174 control(cp = 0, maxdepth = 6, minsplit = 100))
# 0.7821053 control(cp = 0, maxdepth = 6, minsplit = 50))
# 0.7736326 control(cp = 0, maxdepth = 6, minsplit = 30))
#0.7821053 control(cp = 0, maxdepth = 6, minsplit = 60))

#best accuracy- 0.7821053 control(cp = 0, maxdepth = 6, minsplit = 60)) using pre pruning



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
# No. of variables tried at each split: 7
# 
# OOB estimate of  error rate: 0.26%
# Confusion matrix:
#   anomaly Normal class.error
# anomaly   11695     48 0.004087542
# Normal       17  13432 0.001264035

plot(m1)

# number of trees with lowest errorr rate
min(m1$err.rate)
#0.0008922596
which.min(m1$err.rate)
#1247

y_pred = predict(m1, newdata = validate_data[,!(names(validate_data) %in% c("y_var"))],type ="class")

# Making the Confusion Matrix to test accuracy
cm = table(validate_data$y_var, y_pred)
accuracy= (cm[1,1] + cm[2,2])/ sum(cm)
print(accuracy)
# 0.7761611 

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


#mtry= 40 ntree= 400
#mtry=40 ntree=500

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
# OOB estimate of  error rate: 0.19%
# Confusion matrix:
#   anomaly Normal class.error
# anomaly   11708     35 0.002980499
# Normal       14  13435 0.001040970

y_pred = predict(m1, newdata = validate_data[,!(names(validate_data) %in% c("y_var"))],type ="class")

# Making the Confusion Matrix to test accuracy
cm = table(validate_data$y_var, y_pred)
accuracy= (cm[1,1] + cm[2,2])/ sum(cm)
#0.7835248 ntree=500 mtry=20
#0.7644502 ntree=400 mtry=40

print(accuracy)
#printing important variables
library(caret)
RM_IMP=varImp(m1, scale= F)
RM_IMP

# src_bytes	3705.812374
# dst_bytes	1985.704726
# dst_bytes_Flag	1397.110701
# flag	857.0234282
# src_bytes_Flag	481.3259335
# count	395.9811237
# protocol_type	391.6863848
# same_srv_rate	379.4461011
# diff_srv_rate	333.134693
# dst_host_srv_count	290.4195463
# dst_host_same_srv_rate	273.5052894
# dst_host_same_src_port_rate	267.6261254
# service	242.3903227
# srv_count	174.2853163
# diff_srv_rate_Flag	153.7289246
# dst_host_diff_srv_rate	131.1396283
# dst_host_srv_diff_host_rate	117.2649684
# hot	107.9218572
# dst_host_count	82.8235247
# hot_Flag	76.4280492
# logged_in	70.2559366
# dst_host_srv_serror_rate	61.9133756
# logged_in_Flag	61.0332634
# duration	56.9738541
# dst_host_diff_srv_rate_Flag	52.3549431
# dst_host_rerror_rate	50.9036968
# dst_host_serror_rate	36.6928003
# dst_host_srv_diff_host_rate_Flag	31.2237696
# num_compromised_Flag	25.9838351
# num_compromised	24.9759607
# dst_host_same_src_port_rate_Flag	24.5906115
# wrong_fragment_Flag	23.7993664
# dst_host_srv_rerror_rate	22.649772
# serror_rate	17.4743537
# wrong_fragment	17.4121342
# srv_serror_rate	16.2714891
# dst_host_rerror_rate_Flag	14.9443071
# dst_host_srv_rerror_rate_Flag	10.8395502
# rerror_rate	8.2684779
# srv_rerror_rate	7.4427139
# duration_Flag	5.8562988
# rerror_rate_Flag	5.641251
# srv_diff_host_rate	5.0262677
# is_guest_login	4.1680872
# num_root	3.4894377
# is_guest_login_Flag	3.2529841
# num_root_Flag	3.0261746
# srv_diff_host_rate_Flag	2.8377901
# srv_rerror_rate_Flag	2.6001329
# dst_host_same_srv_rate_Flag	2.5433988
# dst_host_srv_serror_rate_Flag	2.4831171
# srv_serror_rate_Flag	2.3774015
# dst_host_serror_rate_Flag	2.1349888
# serror_rate_Flag	2.0028174
# num_file_creations	1.6117256
# num_file_creations_Flag	1.2513967
# root_shell	1.2324092
# root_shell_Flag	1.1246623
# num_failed_logins	0.8322559
# num_failed_logins_Flag	0.8145786
# su_attempted_Flag	0.493785
# same_srv_rate_Flag	0.3387323

#To get the area under the ROC curve for each predictor, the filterVarImp function can be used
roc_imp <- filterVarImp(x = train_data[, -ncol(train_data)], y = train_data$y_var)
roc_imp

# protocol_type                    0.6107479 0.6107479
# service                          0.6250169 0.6250169
# flag                             0.8592038 0.8592038
# duration                         0.5405969 0.5405969
# src_bytes                        0.8986273 0.8986273
# dst_bytes                        0.8997132 0.8997132
# wrong_fragment                   0.5095376 0.5095376
# hot                              0.5036375 0.5036375
# num_failed_logins                0.5000222 0.5000222
# logged_in                        0.8371099 0.8371099
# num_compromised                  0.5043644 0.5043644

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

default_rf

# Model Details:
#   ==============
#   
#   H2OBinomialModel: drf
# Model ID:  DRF_model_R_1568884809795_295 
# Model Summary: 
#   number_of_trees number_of_internal_trees model_size_in_bytes min_depth max_depth mean_depth
# 1              11                       11               34063        17        20   19.09091
# min_leaves max_leaves mean_leaves
# 1        200        284   239.90909
# 
# 
# H2OBinomialMetrics: drf
# ** Reported on training data. **
#   ** Metrics reported on Out-Of-Bag training samples **
#   
#   MSE:  0.004640996
# RMSE:  0.06812485
# LogLoss:  0.04972445
# Mean Per-Class Error:  0.005287199
# AUC:  0.9987565
# pr_auc:  0.06684808
# Gini:  0.9975129
# R^2:  0.9813507
# 
# Confusion Matrix (vertical: actual; across: predicted) for F1-optimal threshold:
#   Normal anomaly    Error        Rate
# Normal   13307      45 0.003370   =45/13352
# anomaly     84   11576 0.007204   =84/11660
# Totals   13391   11621 0.005158  =129/25012
# 
# Maximum Metrics: Maximum metrics at their respective thresholds
# metric threshold        value idx
# 1                       max f1  0.560909     0.994459 195
# 2                       max f2  0.375000     0.994980 229
# 3                 max f0point5  0.650794     0.996121 171
# 4                 max accuracy  0.560909     0.994842 195
# 5                max precision  0.923962     0.999029  64
# 6                   max recall  0.000000     1.000000 399
# 7              max specificity  1.000000     0.999176   0
# 8             max absolute_mcc  0.560909     0.989640 195
# 9   max min_per_class_accuracy  0.500000     0.993933 205
# 10 max mean_per_class_accuracy  0.560909     0.994713 195
# 11                     max tns  1.000000 13341.000000   0
# 12                     max fns  1.000000   786.000000   0
# 13                     max fps  0.000000 13352.000000 399
# 14                     max tps  0.000000 11660.000000 399
# 15                     max tnr  1.000000     0.999176   0
# 16                     max fnr  1.000000     0.067410   0
# 17                     max fpr  0.000000     1.000000 399
# 18                     max tpr  0.000000     1.000000 399
# 
# Gains/Lift Table: Extract with `h2o.gainsLift(<model>, <data>)` or `h2o.gainsLift(<model>, valid=<T/F>, xval=<T/F>)`
# 
# H2OBinomialMetrics: drf
# ** Reported on cross-validation data. **
#   ** 10-fold cross-validation on training data (Metrics computed for combined holdout predictions) **
#   
#   MSE:  0.003442448
# RMSE:  0.05867238
# LogLoss:  0.0182009
# Mean Per-Class Error:  0.003584881
# AUC:  0.9997809
# pr_auc:  0.1664075
# Gini:  0.9995618
# R^2:  0.9861668
# 
# Confusion Matrix (vertical: actual; across: predicted) for F1-optimal threshold:
#   Normal anomaly    Error       Rate
# Normal   13419      30 0.002231  =30/13449
# anomaly     58   11685 0.004939  =58/11743
# Totals   13477   11715 0.003493  =88/25192
# 
# Maximum Metrics: Maximum metrics at their respective thresholds
# metric threshold        value idx
# 1                       max f1  0.506859     0.996249 200
# 2                       max f2  0.305670     0.996632 240
# 3                 max f0point5  0.607692     0.997775 176
# 4                 max accuracy  0.513936     0.996507 198
# 5                max precision  0.979801     0.999908  29
# 6                   max recall  0.000001     1.000000 399
# 7              max specificity  0.999998     0.999926   0
# 8             max absolute_mcc  0.513936     0.992983 198
# 9   max min_per_class_accuracy  0.420067     0.995836 215
# 10 max mean_per_class_accuracy  0.506859     0.996415 200
# 11                     max tns  0.999998 13448.000000   0
# 12                     max fns  0.999998  1956.000000   0
# 13                     max fps  0.000001 13449.000000 399
# 14                     max tps  0.000001 11743.000000 399
# 15                     max tnr  0.999998     0.999926   0
# 16                     max fnr  0.999998     0.166567   0
# 17                     max fpr  0.000001     1.000000 399
# 18                     max tpr  0.000001     1.000000 399
# 
# Gains/Lift Table: Extract with `h2o.gainsLift(<model>, <data>)` or `h2o.gainsLift(<model>, valid=<T/F>, xval=<T/F>)`
# Cross-Validation Metrics Summary: 
#   mean           sd   cv_1_valid   cv_2_valid   cv_3_valid
# accuracy                  0.99698806 0.0011443751   0.99756294    0.9987775   0.99763685
# auc                       0.99979365 3.1977327E-4    0.9999545     0.999991    0.9999446
# err                     0.0030119692 0.0011443751  0.002437043  0.001222494  0.002363135
# err_count                        7.6    2.9135697          6.0          3.0          6.0
# f0point5                   0.9968905 0.0019348506   0.99788433    0.9989503    0.9980056
# f1                         0.9967695 0.0012182381   0.99735683    0.9986882    0.9975083
# f2                         0.9966508 0.0013167618   0.99682987    0.9984263    0.9970115
# lift_top_group              2.146086  0.052742828    2.1672535     2.145105     2.107054
# logloss                   0.01813176  0.010112106  0.014831754 0.0109137455  0.012096747
# max_per_class_error      0.004455152 0.0019407028 0.0035211267 0.0017482517  0.003319502
# mcc                        0.9939507 0.0022932745    0.9950974     0.997544    0.9952624
# mean_per_class_accuracy    0.9969423 0.0011215919    0.9974853    0.9987442    0.9975906
# mean_per_class_error    0.0030577153 0.0011215919  0.002514711 0.0012558053 0.0024093762
# mse                      0.003439434  7.655733E-4 0.0035131045 0.0022892586 0.0027504996
# precision                 0.99697244 0.0025767053   0.99823636    0.9991251    0.9983375
# r2                        0.98616916 0.0030936569    0.9858634   0.99080086    0.9889695
# recall                      0.996573 0.0018163684   0.99647886   0.99825174    0.9966805
# rmse                     0.058310747 0.0066072936  0.059271447  0.047846198  0.052445207
# specificity                0.9973116 0.0023611637    0.9984917   0.99923664   0.99850076
# cv_4_valid   cv_5_valid   cv_6_valid   cv_7_valid   cv_8_valid
# accuracy                  0.99611497   0.99723536   0.99805826    0.9972266    0.9948657
# auc                        0.9990302    0.9999105    0.9999323   0.99997795    0.9993973
# err                      0.003885004  0.002764613 0.0019417476 0.0027733755 0.0051342812
# err_count                       10.0          7.0          5.0          7.0         13.0
# f0point5                  0.99614173    0.9973159   0.99864244   0.99583334    0.9923854
# f1                        0.99561787     0.997065   0.99788046    0.9970797   0.99460804
# f2                         0.9950946    0.9968142    0.9971196   0.99832916    0.9968407
# lift_top_group             2.2516685    2.1223805    2.1803555    2.1103678    2.1082432
# logloss                  0.041765977  0.014767257  0.013360116  0.010852733  0.030978857
# max_per_class_error     0.0052539404  0.003352892 0.0033869601  0.004518072  0.008264462
# mcc                        0.9921299    0.9944524    0.9960917   0.99444705   0.98973316
# mean_per_class_accuracy    0.9959764    0.9972033    0.9979478    0.9973229    0.9950351
# mean_per_class_error     0.004023618 0.0027966849 0.0020521602 0.0026770963  0.004964871
# mse                      0.004276732 0.0035549586 0.0030987097  0.002558961 0.0046489583
# precision                 0.99649125    0.9974832    0.9991511    0.9950042    0.9909091
# r2                         0.9826731   0.98573273   0.98751974    0.9897361     0.981355
# recall                     0.9947461    0.9966471     0.996613   0.99916387    0.9983347
# rmse                     0.065396726  0.059623472  0.055666056  0.050586175   0.06818327
# specificity                0.9972067    0.9977595   0.99928266    0.9954819    0.9917355
# cv_9_valid  cv_10_valid
# accuracy                  0.99637973    0.9960223
# auc                        0.9998568    0.9999414
# err                     0.0036202734  0.003977725
# err_count                        9.0         10.0
# f0point5                   0.9978712    0.9958746
# f1                        0.99601597    0.9958746
# f2                        0.99416757    0.9958746
# lift_top_group             2.1941748    2.0742574
# logloss                  0.015881078  0.015869344
# max_per_class_error     0.0070609003 0.0041254126
# mcc                       0.99271464    0.9920343
# mean_per_class_accuracy       0.9961   0.99601716
# mean_per_class_error    0.0038999992  0.003982829
# mse                      0.004029828 0.0036733295
# precision                  0.9991119    0.9958746
# r2                        0.98375344   0.98528785
# recall                     0.9929391    0.9958746
# rmse                      0.06348093     0.060608
# specificity                0.9992609   0.99615973
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
# Accuracy       AUC Precision Specificity    Recall    Logloss
# 1  0.9975629 0.9999545 0.9982364   0.9984917 0.9964789 0.01483175
# 2  0.9987775 0.9999910 0.9991251   0.9992366 0.9982517 0.01091375
# 3  0.9976369 0.9999446 0.9983375   0.9985008 0.9966805 0.01209675
# 4  0.9961150 0.9990302 0.9964913   0.9972067 0.9947461 0.04176598
# 5  0.9972354 0.9999105 0.9974832   0.9977595 0.9966471 0.01476726
# 6  0.9980583 0.9999323 0.9991511   0.9992827 0.9966130 0.01336012
# 7  0.9972266 0.9999780 0.9950042   0.9954819 0.9991639 0.01085273
# 8  0.9948657 0.9993973 0.9909091   0.9917355 0.9983347 0.03097886
# 9  0.9963797 0.9998568 0.9991119   0.9992609 0.9929391 0.01588108
# 10 0.9960223 0.9999414 0.9958746   0.9961597 0.9958746 0.01586934

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
# anomaly    7305    240
# Normal     5528   9470
# 
# Accuracy : 0.7441          
# 95% CI : (0.7384, 0.7498)
# No Information Rate : 0.5693          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.5107          
# 
# Mcnemar's Test P-Value : < 2.2e-16       
#                                           
#             Sensitivity : 0.5692          
#             Specificity : 0.9753          
#          Pos Pred Value : 0.9682          
#          Neg Pred Value : 0.6314          
#              Prevalence : 0.5693          
#          Detection Rate : 0.3240          
#    Detection Prevalence : 0.3347          
#       Balanced Accuracy : 0.7723          
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

hyper_grid.h2o <- list(ntrees = seq(250, 550, by = 50),
                       mtries = seq(5, 40, by = 5)
                       # max_depth = seq(10, 30, by = 10),
                       # min_rows = seq(1, 3, by = 1),
                       # nbins = seq(20, 30, by = 10),
                       #sample_rate = c(0.55, 0.632, 0.75)
)

# The number of models is 35: 
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
                validate_data$y_var)
#Accuracy : 0.7832

# Compare: 
confusionMatrix(h2o.predict(default_rf, test) %>% as.data.frame() %>% pull(predict), 
                validate_data$y_var)
# Accuracy : 0.789 

# Save an CARTESIAN RF MODEL OUTPUT object to a file
saveRDS(grid_cartesian, file = "RF MODELS USING GRID SEARCH 2_ITERATION.rds")
#h2o.shutdown()

#AAy_data <- readRDS("RF MODELS USING GRID SEARCH 3_ITERATION.rds")


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

# Collect the results and sort by our models: 
grid_perf2 <- h2o.getGrid(grid_id = "rf_grid2", 
                          sort_by = "AUC", 
                          decreasing = FALSE)

# Best RF: 
best_model2 <- h2o.getModel(grid_perf2@model_ids[[1]])

# Use best model for making predictions: 
confusionMatrix(h2o.predict(best_model2, test) %>% as.data.frame() %>% pull(predict), 
                validate_data$y_var)
#Accuracy : 0.7832

# Compare: 
confusionMatrix(h2o.predict(default_rf, test) %>% as.data.frame() %>% pull(predict), 
                validate_data$y_var)
# Accuracy : 0.789 

# Save an CARTESIAN RF MODEL OUTPUT object to a file
saveRDS(grid_cartesian, file = "RF MODELS USING RANDOM SEARCH 3_ITERATION.rds")
#h2o.shutdown()

#AAy_data <- readRDS("RF MODELS USING RANDOM SEARCH 2_ITERATION.rds")

