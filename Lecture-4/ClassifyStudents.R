# ClassifyStudents.R
# Copyright 2016 by Ernst Henle

# Clear objects from Memory
rm(list=ls())
# Clear Console:
cat("\014")

source("CollegeStudentsDataset.R")

# Set repeatable random seed
set.seed(4)

###################################################

# Partition data between training and testing sets

# Replace the following line with a function that partitions the data correctly
StudentsSplit <- PartitionExact(Students, fractionOfTest=0.4) # ********** Change here
TestStudents <- StudentsSplit$testingData
TrainStudents <-StudentsSplit$trainingData

###################################################

# Logistic Regression (glm, binomial)

# http://data.princeton.edu/R/glms.html
# http://www.statmethods.net/advstats/glm.html
# http://stat.ethz.ch/R-manual/R-patched/library/stats/html/glm.html
# http://www.stat.umn.edu/geyer/5931/mle/glm.pdf

# Create logistic regression
glmModel <- glm(formula, data=TrainStudents, family="binomial") # ********** add code here
# Predict the outcomes for the test data. (predict type="response")
predictedProbabilities.GLM <- predict(glmModel, newdata=TestStudents, type="response") # ********** add code here
###################################################

# Naive Bayes
# http://cran.r-project.org/web/packages/e1071/index.html
# http://cran.r-project.org/web/packages/e1071/e1071.pdf

# Get the algorithm
reposURL <- "http://cran.rstudio.com/"
# install package with naive bayes if not alreay installed
if (!require("e1071")) {install.packages("e1071", dep=TRUE, repos=reposURL)} else {" e1071 is already installed "}
# Now that the package is installed, we want to load the package so that we can use its functions
library(e1071)

# Create Naive Bayes model
naiveBayesModel <- naiveBayes(formula, data=TrainStudents) # ********** add code here
# Predict the outcomes for the test data. (predict type="raw")
predictedProbabilities.NB <- predict(naiveBayesModel, newdata=TestStudents, type="raw") # ********** add code here
###################################################

# Confusion Matrices

actual <- ifelse(TestStudents$CollegePlans, "Attend", "NotAttend")
threshold <- 0.7

#Confusion Matrix for Logistic Regression
# convert the predicted probabilities to predictions using a threshold
predicted.GLM <- ifelse(predictedProbabilities.GLM > threshold, "Attend", "NotAttend") # ********** add code here
print(" ")
print(" -------------------------------- ")
print("Confusion Matrix for Logistic Regression")
# create a table to compare predicted values to actual values
print(table(predicted.GLM, actual, dnn = c("Predicted","Actual"))) # ********** add code here

#Confusion Matrix for Naive Bayes
# convert the predicted probabilities to predictions using a threshold
predicted.NB <- ifelse(predictedProbabilities.NB[,2] > threshold, "Attend", "NotAttend") # ********** add code here
print(" ")
print(" -------------------------------- ")
print("Confusion Matrix Naive Bayes")
# create a table to compare predicted values to actual values
print(table(predicted.NB, actual, dnn = c("Predicted","Actual"))) # ********** add code here
###################################################

# Bad Partition; threshold = 0.5
# 
# --------------------------------
# "Confusion Matrix for Logistic Regression"
#              Actual
# Predicted    Attend  NotAttend
# Attend        934        116
# NotAttend     759       1071
# Accuracy defined as fraction of predictions that are correct
# Accuracy:  (934 + 1071)/(934 + 759 + 116 + 1071) = 70%
# 
# --------------------------------
# "Confusion Matrix Naive Bayes"
#            Actual
# Predicted   Attend NotAttend
# Attend       325        84
# NotAttend   1368      1103
# Accuracy defined as fraction of predictions that are correct
# Accuracy:  (325 + 1103)/(325 + 1368 + 84 + 1103) = 50%

# Fill in the rest:

# Exact Partition; threshold = 0.5
#
# --------------------------------
# "Confusion Matrix for Logistic Regression"
#            Actual
# Predicted   Attend NotAttend
# Attend       687       227
# NotAttend    260      1706
# Accuracy defined as fraction of predictions that are correct
# Accuracy:  (687 + 1706)/(687 + 260 + 227 + 1706) = 83%
#
# --------------------------------
# "Confusion Matrix Naive Bayes"
#            Actual
# Predicted   Attend NotAttend
# Attend       570       209
# NotAttend    377      1724
# Accuracy defined as fraction of predictions that are correct
# Accuracy:  (570 + 1724)/(570 + 377 + 209 + 1724) = 80%


# Fast Partition; threshold = 0.5
#
# --------------------------------
# "Confusion Matrix for Logistic Regression"
#            Actual
# Predicted   Attend NotAttend
# Attend       691       227
# NotAttend    262      1715
# Accuracy defined as fraction of predictions that are correct
# Accuracy:  (691 + 1715)/(691 + 262 + 227 + 1715) = 83%
#
# --------------------------------
# "Confusion Matrix Naive Bayes"
#            Actual
# Predicted   Attend NotAttend
# Attend       572       211
# NotAttend    381      1731
# Accuracy defined as fraction of predictions that are correct
# Accuracy:  (572 + 1731)/(570 + 381 + 211 + 1731) = 80%

# Exact Partition;  threshold = 0.7
#
# --------------------------------
# "Confusion Matrix for Logistic Regression"
#            Actual
# Predicted   Attend NotAttend
# Attend       498        81
# NotAttend    449      1852
# Accuracy defined as fraction of predictions that are correct
# Accuracy:  (498 + 1852)/(498 + 449 + 81 + 1852) = 82%
#
# --------------------------------
# "Confusion Matrix Naive Bayes"
#            Actual
# Predicted   Attend NotAttend
# Attend       425       100
# NotAttend    522      1833
# Accuracy defined as fraction of predictions that are correct
# Accuracy:  (425 + 1833)/(425 + 522 + 100 + 1833) = 78%

###################################################
