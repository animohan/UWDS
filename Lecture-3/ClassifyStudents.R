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
glmmodel = glm(formula = formula, family = "binomial", data = TrainStudents)
# Predict the outcomes for the test data. (predict type="response")
predictedProbabilities.glm  = predict(glmmodel, newdata = TestStudents, type = "response")
###################################################
# Confusion Matrices for Logistic Regression
actual <- ifelse(TestStudents$CollegePlans, "Attend", "NotAttend")
threshold <- 0.7

#Confusion Matrix for Logistic Regression
# convert the predicted probabilities to predictions using a threshold
predicted.GLM = ifelse(predictedProbabilities.glm > threshold, "Attend","NotAttend")

print(" ")
print(" -------------------------------- ")
print("Confusion Matrix for Logistic Regression")
# create a table to compare predicted values to actual values
print(table(predicted.GLM, actual, dnn = c("Predicted","Actual")))

####################################################
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
# ********** add code here
bayesModel = naiveBayes(formula = formula, data = TrainStudents)

# Predict the outcomes for the test data. (predict type="raw")
# ********** add code here
predictedProbabilities.bayes = predict(bayesModel, newdata = TestStudents,  type = "raw")
###################################################
#Confusion Matrix for Naive Bayes
# convert the predicted probabilities to predictions using a threshold
actual <- ifelse(TestStudents$CollegePlans, "Attend", "NotAttend")
threshold <- 0.7
predicted.Bayes = ifelse(predictedProbabilities.bayes[,2]>threshold, "Attend","NotAttend")

print(" ")
print(" -------------------------------- ")
print("Confusion Matrix Naive Bayes")
# create a table to compare predicted values to actual values
print(table(predicted.Bayes, actual, dnn = c("Predicted","Actual")))

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



#################################################################################
# HOMEWORK ANSWERS
#################################################################################

# 1. Completed in Class (Added code to ClassifyStudents for LogisticRegression)
# 2. Completed in Class (Used predict to predict outcome using Logistic Regression model)
# 3. Completed in Class (Create confusion Matrix)
# 4. Adding code for Naive Bayes: Completed and added code at the appropriate place.

# 5. 
  # Naive Bayes Results
    # Number of rows:2880
    # Number of columns:2

    # Naive Bayes gives posterior probabilities for both classes.

  # Logistic Regression Results
    # Number of rows:2880
    # Number of columns: 1

# 6: Added code for Naive Bayes. 

# 7
  #7a: Partition Wrong; fractionOfTest=0.4, Threshold 0.5
    
    #  Confusion Matrix Naive Bayes
        #            Actual
        # Predicted   Attend NotAttend
        # Attend       325        84
        # NotAttend   1368      1103
        # Accuracy defined as fraction of predictions that are correct
        # Accuracy:  (325 + 1103)/(325 + 1368 + 84 + 1103) = 49.58%

    # Confusion Matrix for Logistic Regression
        #              Actual
        # Predicted    Attend  NotAttend
        # Attend        934        116
        # NotAttend     759       1071
        # Accuracy defined as fraction of predictions that are correct
        # Accuracy:  (934 + 1071)/(934 + 759 + 116 + 1071) = 79.61%
  
  #7b. Added code to PartitionFast() in class
  #7c. Added following code to PartitionExact()
        #PartitionExact <- function(dataSet, fractionOfTest = 0.3)
        #{
        #  numberOfRows = nrow(dataSet)
        #  numberOfTestRows = fractionOfTest*numberOfRows
        #  testFlag = sample(numberOfRows, numberOfTestRows)
        #  testingData = dataSet[testFlag]
        #  trainingData = dataSet[!testFlag]
        #  dataSetSplit <- list(trainingData=trainingData, testingData=testingData)
        #  return(dataSetSplit)
        #}

#8
  #8a.
    # Fast Partition; threshold = 0.5, fractionOfTest=0.4
    #
    # --------------------------------
    # Confusion Matrix for Logistic Regression
        #            Actual
        # Predicted   Attend NotAttend
        # Attend      691    227    add results here
        # NotAttend   262    1715   add results here
        # Accuracy defined as fraction of predictions that are correct
        # Accuracy:   (691+1715)/(691+227+262+1715) = 83.11%   add results here

    # Confusion Matrix for Naive Bayes
        #            Actual
        # Predicted   Attend NotAttend
        # Attend      572    211    add results here
        # NotAttend   381    1731   add results here
        # Accuracy defined as fraction of predictions that are correct
        # Accuracy:   (572+1731)/(572+1731+381+211) = 79.55%   add results here

  #8b.
    # Exact Partition; threshold = 0.5, fractionOfTest=0.4
    #
    # --------------------------------
     # Confusion Matrix for Logistic Regression
        #            Actual
        # Predicted   Attend NotAttend
        # Attend      679    212    add results here
        # NotAttend   273    1716   add results here
        # Accuracy defined as fraction of predictions that are correct
        # Accuracy:   (679+1716)/(679+1716+212+273) = 83.16%   add results here
    
    # Confusion Matrix for Naive Bayes
      #            Actual
      # Predicted   Attend NotAttend
      # Attend      551    208    add results here
      # NotAttend   401    1720   add results here
      # Accuracy defined as fraction of predictions that are correct
      # Accuracy:   (551+1720)/(551+1720+401+208) = 78.86%   add results here


#8c.
  # Exact Partition; threshold = 0.5, fractionOfTest=0.4
  #
  # --------------------------------
    # Confusion Matrix for Logistic Regression
        #            Actual
        # Predicted   Attend NotAttend
        # Attend      463    88    add results here
        # NotAttend   489    1840   add results here
        # Accuracy defined as fraction of predictions that are correct
        # Accuracy:   (463+1840)/(463+1840+88+489) = 79.97%   add results here

  # Confusion Matrix for Naive Bayes
        #            Actual
        # Predicted   Attend NotAttend
        # Attend      397    89    add results here
        # NotAttend   555    1839   add results here
        # Accuracy defined as fraction of predictions that are correct
        # Accuracy:   (397+1839)/(397+1839+89+555) = 77.63%   add results here

  # By increasing the Threshold to 0.7, we specify that the posterior probability should be
  # greater than 0.7 for predicting a student will attend college. With this higher, threshold
  # we can see in cases where the method predicts that a student will go to college
  # and the student does not; the error rate for prediction decreases. E.g. In Logistic 
  # regression, with threshold 0.5, 208 students were predicted to attend college, but they 
  # did not. With threhsold = 0.7, only 88 students who were predicted to attend college 
  # did not end up attending.
  # However, by increasing the threshold, the error rate increases in the case where the
  # the method predicted a Student will not attend college, whereas they did attend college.
