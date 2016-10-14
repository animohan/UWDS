# DataScience01c
# Data Preparation

# Start fresh
rm(list=ls())

# Outlier Removal
# This is a vector
c(1, -1, -5, -1, -1, -19, 3, -1, -1, -5) 
# assign the vector
Vector <- c(1, -1, -5, -1, -1, -19, 3, -1, -1, -5)
# Guestimate:  -19 is the outlier
# Anything less than -6 can be removed
# The following indicates whether we want to keep the values:
Vector > -6
Vector <- Vector[Vector > -6]
Vector

# Start fresh
rm(list=ls())
Vector <- c(1, -1, -5, -1, -1, -19, 3, -1, -1, -5)
# Goal:  Remove anything beyond 2 standard deviations from the mean
VectorMean <- mean(Vector)
VectorSd <- sd(Vector)
lowBoundary <- VectorMean - 2*VectorSd
HighBoundary <- VectorMean + 2*VectorSd
goodFlag <- (Vector > lowBoundary) & (Vector < HighBoundary)
Vector <- Vector[goodFlag]
Vector

# Outlier Removal
Vector <- c('a', 'a', 'a', 'a', 'b', 'b', 'b', 'b', 'b', 'c', 'c', 'c', 'c', 'c', 'c', 'c', 'd', 'a', 'a', 'a', 'a')
# Category Outlier is 'd' because it occurs less than 5% of the time
Vector
Vector.modified <- Vector[Vector != 'd']
setdiff(Vector, Vector_mod)

# Relabel
c('BS', 'MS', 'PhD', 'HS', 'BSc', 'Masters', 'High School', 'Masters', 'Masters', 'BA', 'Bachelors', 'MS', 'MS')
Vector <- c('BS', 'MS', 'PhD', 'HS', 'BSc', 'Masters', 'High School', 'Masters', 'Masters', 'BA', 'Bachelors', 'MS', 'MS')
unique(Vector)
length(unique(Vector))
Vector[Vector == 'Bachelors'] <- 'BS'
length(unique(Vector))
Vector[Vector == 'BSc'] <- 'BS'
length(unique(Vector))
Vector[Vector == 'BA'] <- 'BS'
length(unique(Vector))
Vector[Vector == 'Masters'] <- 'MS'
length(unique(Vector))
Vector[Vector == 'High School'] <- 'HS'
length(unique(Vector))
Vector

# Turn codes into years of college
# Exercise

#Normalization
# Start fresh
rm(list=ls())
Vector <- c(1, -1, -5, -1, -1, -19, 3, -1, -1, -5)
# Linear normalization maps data in a linear fashion:  normalizedVector <- Vector*slope + offset

# Min Max normalization maps from 0 to 1.
# y = a + bx
# OR:
# y = (x - c)/d; Where:  a = -c/d; b = 1/d

# "c" adjusts min value to zero:
minValue <- min(Vector)
# range adjusts max value to 1
#  range is the min subtracted from the max
range <- max(Vector) - minValue
Vector<- (Vector - minValue)/range
min(Vector)
max(Vector)
Vector


rm(list=ls())
# Relabel and cast this vector into a number:
c('one', 'two', 3, 4, 5, 6, 7, 8, 9, 0, 1)


rm(list=ls())
# Binarization:
#  Binarization turns columns of categories into a columns of binaries:
#  You start out with a vector called vehicle that can contain three categories:  car, truck, bicycle
# Vehicle vector looks like the following:
#  c(car, bicycle, bicycle, bicycle, car, car, truck, bicycle, truck, bicycle)
# You create three columns called car, truck, and bicycle:
#  car <- c(1,0,0,0,1,1,0,0,0,0)
#  truck <- c(0,0,0,0,0,0,1,0,1,0)
#  bicycle <- c(0,1,1,1,0,0,0,1,0,1)

# Binning
Vector<- c(1, 1:5, 1:10, 1:20, 1:40, 100) # Vector<- c(runif(30))
Vector
hist(Vector)
numberOfBins <- 7

# Discretization into 4 bins
range <- max(Vector) - min(Vector)
binWidth <- range / numberOfBins
bin1Min <- -Inf
bin1Max <- min(Vector) + 1*binWidth
bin2Min <- bin1Max
bin2Max <- min(Vector) + 2*binWidth
bin3Min <- bin1Max
bin3Max <- min(Vector) + 3*binWidth
bin4Min <- bin3Max
bin4Max <- min(Vector) + 4*binWidth
bin5Min <- bin4Max
bin5Max <- min(Vector) + 5*binWidth
bin6Min <- bin5Max
bin6Max <- min(Vector) + 6*binWidth
bin7Min <- bin6Max
bin7Max <- Inf
xDiscretized <- rep(NA, length(Vector))
xDiscretized
xDiscretized[bin1Min < Vector & Vector <= bin1Max] <- "L1"
xDiscretized
xDiscretized[bin2Min < Vector & Vector <= bin2Max] <- "L2"
xDiscretized
xDiscretized[bin3Min < Vector & Vector <= bin3Max] <- "L3"
xDiscretized[bin4Min < Vector & Vector <= bin4Max] <- "L4"
xDiscretized[bin5Min < Vector & Vector <= bin5Max] <- "L5"
xDiscretized[bin6Min < Vector & Vector <= bin6Max] <- "L6"
xDiscretized[bin7Min < Vector & Vector <= bin7Max] <- "L7"
xDiscretized





quantileBinMax <- function(Vector=c(1,1,1,2,2,2,10), numberOfBins=2)
{
  binMax <- NA
  for (i in 1:(numberOfBins-1))
  {
    binMax[i] <- quantile(Vector, i/numberOfBins)
  }
  c(-Inf, binMax,+Inf)
}
binLimits <- quantileBinMax(Vector, numberOfBins)

cut(Vector, binLimits, right=T)
?hist
hist(Vector, c(min(Vector), binLimits[2:(length(binLimits)-1)], max(Vector)))
quantile(1:11, .49)
