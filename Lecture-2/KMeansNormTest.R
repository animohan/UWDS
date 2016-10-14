# KMeansNormTest.R

# Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

TestObservations <- as.matrix(read.csv("TestObservations.csv"))
TestCenters <- matrix(c(1, 1, -2, -2, 2, -2), nrow=3)

source("KMeans.R")
source("KMeansHelper.R")
source("KMeansNorm.R")

# TestObservations Distribution in second dimension
hist(TestObservations[,2], col=rgb(1,1,0,1))

# TestObservations Distribution in first dimension
hist(TestObservations[,1], col=rgb(0,0,1,0.25), add=T)

# What is the single most obvious difference between these two distributions?

# Test 1
KMeansNorm(clusterCenters = TestCenters, observations = TestObservations, normD1=F, normD2=F)
# Does clustering occur along one or two dimensions?  Which dimensions?  Why?

# Test 2
KMeansNorm(clusterCenters = TestCenters, observations = TestObservations, normD1=T, normD2=F)
# Does clustering occur along one or two dimensions?  Which dimensions?  Why?

# Test 3
KMeansNorm(clusterCenters = TestCenters, observations = TestObservations, normD1=F, normD2=T)
# Does clustering occur along one or two dimensions?  Which dimensions?  Why?

# Test 4
KMeansNorm(clusterCenters = TestCenters, observations = TestObservations, normD1=T, normD2=T)
# Does clustering occur along one or two dimensions?  Which dimensions?  Why?

# Put answers to assignment questions here:
