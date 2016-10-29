# PatientReadmission.R
# Copyright Ernst Henle 2016

# Previously:
# QuizOnClassification.R

# Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

url <- "PatientReadmission.csv"
url <- "https://www.dropbox.com/s/72sldg5t0jssprx/PatientReadmission.csv?dl=1"
Patients <- read.csv(url, header=TRUE, stringsAsFactors=TRUE)
ReadmitFx <- Readmitted ~ .

# Partition the data the exact way
fractionOfTest=0.25 # do not change
set.seed(2) # do not change
randoms <- runif(nrow(Patients))
cutoff <- quantile(randoms, fractionOfTest)
testFlag <- randoms <= cutoff
testData <- Patients[testFlag, ]
trainData <- Patients[!testFlag, ]

# Get Package
reposURL <- "http://cran.rstudio.com/"
if (!require("randomForest")) {install.packages("randomForest", dep=TRUE, repos=reposURL)} else {" randomForest is already installed "}
# Now that the package is installed, we want to load the package so that we can use its functions
library(randomForest)
if (!require("nnet")) {install.packages("nnet", dep=TRUE, repos=reposURL)} else {" nnet is already installed "}
# Now that the package is installed, we want to load the package so that we can use its functions
library(nnet)
if (!require("rpart")) {install.packages("rpart", dep=TRUE, repos=reposURL)} else {" rpart is already installed "}
# Now that the package is installed, we want to load the package so that we can use its functions
library(rpart)

# Q4:  What is d2? Look ahead a few line to see how it is used
d2 <- trainData # Change this line
#d2 seems to be input training data


# The following lines of code create classifications.
# The classifications are a neural net, a random forest, and a decision tree.
set.seed(4)  # do not change
Model.NN <- nnet(formula=ReadmitFx, data=d2, size=10, maxit=200)
Model.RF <- randomForest(formula=ReadmitFx, data=d2)
Model.DT <- rpart(formula=ReadmitFx, data=d2)

# Q5:  What is d1? Look ahead a few line to see how it is used
d1 <- testData # Change this line
#<ANS> This seems to be test data

# The following lines of code calculate probabilities.
# These probabilities will be used to test the accuracy of their respectives models. 
prob.NN <- predict(Model.NN, newdata=d1, type="raw")
prob.RF <- predict(Model.RF, newdata=d1, type="prob")[,2]
prob.DT <- predict(Model.DT, newdata=d1, type="prob")[,2]

# Q7 and Q8
# Threshold probabilities to get predictions (see homework solution for examples)


# Create a table to compare predicted values to actual values (see homework solution for examples)

threshold = 0.2
predicted.NN <- ifelse(prob.NN > threshold, "Yes", "No")
predicted.RF <- ifelse(prob.RF > threshold, "Yes", "No")
predicted.DT <- ifelse(prob.DT > threshold, "Yes", "No")

print(table(predicted.NN, testData$Readmitted, dnn = c("Predicted","Actual")))
print(table(predicted.RF, testData$Readmitted, dnn = c("Predicted","Actual")))
print(table(predicted.DT, testData$Readmitted, dnn = c("Predicted","Actual")))




# Calculate Accuracy:  Correct/Total (see homework solution for examples)

