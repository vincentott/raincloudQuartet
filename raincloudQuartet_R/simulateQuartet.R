#
#
# Creating the Raincloud Quartet via Simulated Annealing
#
# Written by Vincent L. Ott, University of Amsterdam, The Netherlands
# 2024
#
#



# Libraries ----
source("helperFunctions.R")



# Load seedData & define hyperparameters ----
seedData <- read.csv("./quartetData/seedData.csv")
seedData <- as.vector(seedData[ , 2])
makeCloud(seedData)
inspectStats(seedData)
getP(seedData)

sampleSize   <- 111
targetMean   <- round(mean(seedData), 2)
targetSd     <- round(sd(seedData), 2)
targetPValue <- getP(seedData)



# I: Normal distribution ----
set.seed(4)  # With 4 instead of 1, clearer bell shape
iteration = 1
normCurrent <- rnorm(sampleSize, targetMean, targetSd)
while (
  isErrorOk(normCurrent, targetMean, targetSd, targetPValue) == FALSE
) {
  normCurrent <- rnorm(sampleSize, targetMean, targetSd)
  iteration = iteration + 1
}
makeCloud(normCurrent)
inspectStats(normCurrent)
getP(normCurrent)
print(iteration)

write.csv(normCurrent, "./quartetData/normalData.csv")  # Commented out just for safety



# II: Right skewed distribution ----






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

