# KMeansNorm.R
source("KMeans.R")

KMeansNorm <- function(observations = sampleObservations, clusterCenters = centersGuess, normD1 = F, normD2 = F)
{
  if (normD1)
  {
    # Determine mean and standard deviation of 1st dimension in observations
     meanval = mean(observations[,1])
     sdval = sd(observations[,1])
    # normalize 1st dimension of observations
     observations[,1] = (observations[,1]-meanval)/sdval
    # Put code in place of this line
    # normalize 1st dimension of clusterCenters
    # Put code in place of this line
  }
  if (normD2)
  {
    # Determine mean and standard deviation of 2nd dimension in observations
    # Put code in place of this line
    # normalize 2nd dimension of observations
    # Put code in place of this line
    # normalize 2nd dimension of clusterCenters
    # Put code in place of this line
  }
  clusterCenters <- KMeans(observations, clusterCenters)
  if (normD1)
  {
    # denormalize in first dimension
    # Put code in place of this line
  } 
  if (normD2)
  {
    # denormalize in second dimension
    # Put code in place of this line
  } 
  return(clusterCenters)
}

