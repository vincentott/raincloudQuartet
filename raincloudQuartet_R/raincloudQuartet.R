#
#
# Creating the Raincloud Quartet via Simulated Annealing
#
# Written by Vincent L. Ott, University of Amsterdam, The Netherlands
# 2024
#
#



# Libraries ----
library(ggplot2)
library(ggrain)
source("helperFunctions.R")



# Hyperparameters ----
sampleSize   <- 111  # Hommage to Anscombe (1973)
targetPValue <- 0.049



# rnorm() ----
set.seed(1)
normalStart <- rnorm(sampleSize, 0, 1)
makeCloud(normalStart)
inspectStats(normalStart)
getP(normalStart)



# runif() ----
set.seed(1)
unifStart <- runif(sampleSize)
makeCloud(unifStart)
inspectStats(unifStart)

# Center
unifCenter <- unifStart - mean(unifStart)
makeCloud(unifCenter)
inspectStats(unifCenter)

# Identify H0 with p = 0.49
currentH0     <- -0.01  # Informed start based on raincloud plot & manually obtained pValue
getP(unifCenter, mu = currentH0)
currentPValue <- round(t.test(unifCenter, alternative = "greater", mu = currentH0)[["p.value"]], 3)
print(currentPValue)
while (currentPValue != targetPValue) {  # Reduce H0 until current matches target
  currentH0     <- currentH0 - 0.00001
  currentPValue <- getP(unifCenter, mu = currentH0)
}
print(currentH0)
print(currentPValue)

# Move unifCenter up for H0 = 0 with p = .049
seedData <- unifCenter + abs(currentH0)
inspectStats(seedData)
targetPValue == getP(seedData)
makeCloud(seedData)

# What if we round seedData?
roundSeedData <- round(seedData, 2)
inspectStats(roundSeedData)
makeCloud(roundSeedData)
getP(roundSeedData)  # p = .050, so we are very, very close!

# Let´s sort roundSeedData in ascending order, then increase some of the highest points to drop pValue
sortedSeedData <- roundSeedData[order(roundSeedData)]
currentPValue <- getP(sortedSeedData)
while(currentPValue != 0.049) {
  sortedSeedData[length(sortedSeedData)] <- sortedSeedData[length(sortedSeedData)] + 0.01
  currentPValue <- getP(sortedSeedData)
}
# print(sortedSeedData[length(sortedSeedData)])
inspectStats(sortedSeedData)
makeCloud(sortedSeedData)
getP(sortedSeedData)

# # Inspect cumulative density function #test
# seedCdf <- ecdf(sortedSeedData)
# normCdf <- ecdf(rnorm(sampleSize, 0.04, 0.27))  # Mean and SD from sortedSeedData
# plot(seedCdf)
# lines(normCdf, col='red')
# makeCloud(rnorm(sampleSize, 0.04, 0.27))
# inspectStats(rnorm(sampleSize, 0.04, 0.27))
# makeCloud(sortedSeedData)
# # Extract ECDF values for a specific set of input values
# x_values <- seq(min(data, data2), max(data, data2), length.out = 100)  # Generating a sequence of x values
#
# # Extract ECDF values for the first data
# ecdf_values_data <- p(x_values)
#
# # Extract ECDF values for the second data
# ecdf_values_data2 <- p2(x_values)



# Storage ----
# set.seed(1)
# oneNormal  <- rnorm(1000, 5,1)
# twoBimodal <- c(rnorm(500, 3.5, 0.5), rnorm(500, 6.5, 0.5))
# threeSkewed <- c(rnorm(500, 3, 1), rnorm(500, 6.5, 3))
# fourOutliers <- c(rnorm(900, 4.5, 1), runif(100, 7, 14))
# par(mfrow=c(2,2))
# hist(oneNormal)
# hist(twoBimodal)
# hist(threeSkewed)
# hist(fourOutliers)
# mean(oneNormal)
# mean(twoBimodal)
# mean(threeSkewed)
# mean(fourOutliers)
#
# # for (i in 1:lenght(list(oneNormal, twoBimodal, threeSkewed, fourOutliers))) {
# #   print(inspect(i))
# # }

