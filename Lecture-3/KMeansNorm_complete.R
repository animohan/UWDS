# KMeansNorm.R
# Copyright 2016 by Ernst Henle

KMeansNorm <- function(observations = sampleObservations, clusterCenters = centersGuess, normD1 = F, normD2 = F)
{
  if (normD1)
  {
    # Determine mean and standard deviation of 1st dimension in observations
    mu1 <- mean(observations[,1])
    sigma1 <- sd(observations[,1])
    # normalize 1st dimension of observations
    observations[,1] <- (observations[,1] - mu1)/sigma1
    # normalize 1st dimension of clusterCenters
    clusterCenters[,1] <- (clusterCenters[,1] - mu1)/sigma1
  }
  if (normD2)
  {
    # Determine mean and standard deviation of 2nd dimension in observations
    mu2 <- mean(observations[,2])
    sigma2 <- sd(observations[,2])
    # normalize 2nd dimension of observations
    observations[,2] <- (observations[,2] - mu2)/sigma2
    # normalize 2nd dimension of clusterCenters
    clusterCenters[,2] <- (clusterCenters[,2] - mu2)/sigma2
  }
  clusterCenters <- KMeans(observations, clusterCenters)
  if (normD1)
  {
    # denormalize in first dimension
    clusterCenters[,1] <- clusterCenters[,1] * sigma1 + mu1
  } 
  if (normD2)
  {
    # denormalize in second dimension
    clusterCenters[,2] <- clusterCenters[,2] * sigma2 + mu2
  } 
  return(clusterCenters)
}

# Q4a
# What is the single most obvious difference between these two distributions?
# The 2nd dimension is 10X (500:50) larger than the first dimension

# Q4b
# Does clustering occur along one or two dimensions?  Which dimensions?  Why?
# One Dimension.  The 2nd Dimension.
# The 2nd Dimension is 10X larger (500:50) than the first dimension.

# Q4c
# Does clustering occur along one or two dimensions?  Which dimensions?  Why?
# One Dimension.  The 2nd Dimension.
# The 2nd Dimension is 100X larger (500:5) than the first dimension.

# Q4d
# Does clustering occur along one or two dimensions?  Which dimensions?  Why?
# One Dimension.  The 1st Dimension.
# The 2nd Dimension is 10X smaller (4:40) than the first dimension.

# Q4e
# Does clustering occur along one or two dimensions?  Which dimensions?  Why?
# Two Dimensions. 
# The 2nd Dimension is about the same size as (2:2) as the first dimension.

# Q5
# Normalization makes the dimensions similar in range.  K-means clustering
# is sensitive to differences in range among its dimensions

# Q6
# Change the categorical variables to dummy variables (binarization).
# K-means requires numerical data.

# Q7
# Unsupervised:
# The (training) data are evaluated without input from an expert.
# The algorithm and the (training) data determine the outcomes.
#
# Supervised:
# The training data are evaluated together with an expert label.
# The algorithm tries to finds a pattern that predicts the expert label from the training data.
#