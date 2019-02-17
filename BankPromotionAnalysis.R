#install.packages("readxl")
#install.packages("caret")
#install.packages("faraway")
#install.packages("tidyverse")
#install.packages("pROC")
#install.packages("ROCR")
library(ggplot2)
library(readxl)
library(caret)
library(dplyr)
library(faraway)
library(tidyverse)
library(pROC)
library(ROCR)
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")


#read data in as data frame
bankdata <- read.csv("bank-full.csv", sep = ';')
colnames(bankdata)[colnames(bankdata)=='y'] <- 'Subscribed'

#Adding offset column
#bankdata <- bankdata %>%
#  mutate(offset = case_when(bankdata$Subscribed == "no" ~ 0,
#                            bankdata$Subscribed == "yes" ~ 1))

#### Function that list column names and na values
propmiss <- function(dataframe) {
  m <- sapply(dataframe, function(x) {
    data.frame(
      na_count=sum(is.na(x)),
      Obs=length(x), 
      perc_missing=sum(is.na(x))/length(x)*100
    )
  })
  d <- data.frame(t(m))
  d <- sapply(d, unlist)
  d <- as.data.frame(d)
  d$col_name <- row.names(d)
  row.names(d) <- NULL
  d <- cbind(d[ncol(d)],d[-ncol(d)])
  return(d[order(d$na_count, decreasing=TRUE), ])
}

#Basic info on df
dim(bankdata)

#Check for missing data
propmiss(bankdata)

#List column names vertically
df_ColNames <- matrix(colnames(bankdata))
colnames(df_ColNames) <- c("col_name")
df_ColNames

#structure of data
str(bankdata)

#find levels of categorical columns and counts
table(bankdata$job)
table(bankdata$marital)
table(bankdata$education)
table(bankdata$default)
table(bankdata$housing)
table(bankdata$loan)
table(bankdata$contact)
table(bankdata$month)
table(bankdata$poutcome)
table(bankdata$Subscribed)

#Quick Summary of attributes
summary(bankdata)

#Cross tables
crosstab(bankdata, row.vars = c("education","marital"), col.vars = "Subscribed", type = "r")
crosstab(bankdata, row.vars = c("poutcome","job"), col.vars = "Subscribed", type = "r")

#change to factor (Numeric responses)
bankdata$job <- factor(bankdata$job)
bankdata$marital <- factor(bankdata$marital)
bankdata$education <- factor(bankdata$education)
bankdata$default <- factor(bankdata$default)
bankdata$housing <- factor(bankdata$housing)
bankdata$loan <- factor(bankdata$loan)
bankdata$contact <- factor(bankdata$contact)
bankdata$month <- factor(bankdata$month)
bankdata$poutcome <- factor(bankdata$poutcome)
bankdata$Subscribed <- factor(bankdata$Subscribed)

#Set seed
set.seed(10)

#split data into train/test 80/20 split
train_idx <- createDataPartition(bankdata$Subscribed,
                    p = 0.8,
                    list = FALSE)

#Create training data set
train_data <- bankdata[train_idx,]

#Create testing data set, use - to remove entries from train
test_data <- bankdata[-train_idx,]

#find porpotion of subscribed training data set
table(train_data$Subscribed)
prop.table(table(train_data$Subscribed))

#find porpotion of subscribed testing data set
table(test_data$Subscribed)
prop.table(table(test_data$Subscribed))

#creating succes and failure sets
train_success <- dplyr::filter(train_data,Subscribed=='yes') 
train_failure <- dplyr::filter(train_data,Subscribed=='no') 

#create oversampling training data set
success_idx <- createDataPartition(1:nrow(train_success),
                                 p = 7/8,
                                 list = FALSE)

failure_idx <- createDataPartition(1:nrow(train_failure),
                                    p = 1/8,
                                    list = FALSE)

#appending data sets to each other
df <- rbind(train_success[success_idx,], train_failure[failure_idx,])

#find porption of responses
table(df$offset)
prop.table(table(df$Subscribed))

p1 <- prop.table(table(train_data$Subscribed))[2]
r1 <- prop.table(table(df$Subscribed))[2]

#define offset to be used training data set
#to adjust by using an offset, add a variable to your data set defined 
#as log[(r1*(1-p1)) / ((1-r1)*p1)], where log represents the natural logarithm
df$offset <- log((r1*(1-p1)) / ((1-r1)*p1))
str(df)

#building model, family=binomial tells R it is logistic
fit1 <- glm(Subscribed ~ education + job + poutcome + housing + duration + campaign, 
            offset = offset,
            family = binomial, 
            data = df)

fit2 <- glm(Subscribed ~ education + job + poutcome + housing, 
            offset = offset,
            family = binomial, 
            data = df)

fit3 <- glm(Subscribed ~., 
            offset = offset,
            family = binomial, 
            data = df)

summary(fit1)
summary(fit2)
summary(fit3)

#Comparing models (compare only 2 models, nested)
anova(fit1, fit2, test = "Chisq")
anova(fit2, fit3, test = "Chisq")
anova(fit1, fit3, test = "Chisq")

#I like fit1
#Std.Error
#     This represents the accuracy of the coefficients. 
#     The larger the standard error, the less confident we are about the estimate.
summary(fit1)$coeff
summary(fit3)$coeff



##########################################
#
#Prediction on testing data set the 20%
#
##########################################

####Adding offset column
test_data$offset <- log((r1*(1-p1)) / ((1-r1)*p1))

#Calculating lphat
lphat <- predict(fit1, newdata = test_data) - test_data$offset

#adding prediction to data frame
test_data$phat <- ilogit(lphat)
histogram(test_data$phat)
test_data$pred_class <- test_data$phat > 0.5 
test_data$pred_class <- factor(test_data$pred_class, labels = c("no", "yes"))

dim(test_data)
#Caclualte AUC
roccalc <- roc(test_data$Subscribed, test_data$phat)
auc(roccalc)
levels(test_data$pred_class)
str(test_data)


predResults <- table(test_data$pred_class, test_data$Subscribed, dnn=list("PredSubscribed","Subscribed"))
predResults2 <- predResults[2:1,2:1]
sum(predResults)
sum(diag(predResults))

#Classification Accuracy actual and pred match
100 * sum(diag(predResults))/ sum(predResults) 

#Sensitivity & RECALL breaking down by the subscribed of yes
100*prop.table(predResults, margin = 2)

str(test_data)

head(test_data)

##########################################
#
#Stack overflow
#https://stackoverflow.com/questions/8499361/easy-way-of-counting-precision-recall-and-f1-score-in-r
##########################################
?posPredValue
posPredValue(test_data$pred_class, test_data$Subscribed)
sensitivity(test_data$pred_class, test_data$Subscribed)

lvs <- c("normal", "abnormal")
truth <- factor(rep(lvs, times = c(86, 258)),
                levels = rev(lvs))
pred <- factor(
  c(
    rep(lvs, times = c(54, 32)),
    rep(lvs, times = c(27, 231))),               
  levels = rev(lvs))

xtab <- table(pred, truth)
sensitivity(predResults2)
posPredValue(predResults2)
predResults2

y <- ... # factor of positive / negative cases
predictions <- ... # factor of predictions

precision <- posPredValue(predictions, y, positive="1")
recall <- sensitivity(predictions, y, positive="1")

F1 <- (2 * precision * recall) / (precision + recall)
?confusionMatrix

