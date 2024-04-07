#
#
# Creating the Raincloud Quartet via Simulated Annealing
#
# Written by Vincent L. Ott, University of Amsterdam, The Netherlands
# 2024
#
#



# Source ----
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

# write.csv(normCurrent, "./quartetData/normalData.csv")  # Commented out just for safety
normalData <- read.csv("./quartetData/normalData.csv")
normalData <- as.vector(normalData[ , 2])
makeCloud(normalData)
inspectStats(normalData)
getP(normalData)



# II: Right skewed distribution ----
# To mimic typical response time data, we want positively skewed data.
# First attempt aims for skewness =  3; see blanca2013 in methodology journal

# iterations 200k, shake 0.10
set.seed(1)
skewedI200S10 <- simulateAnnealing(
  startData    = seedData,
  fitFunction  = skewness,
  fitTarget    = 3,
  maxIter      = 200000,
  shake        = 0.10,
  targetMean   = targetMean,
  targetSd     = targetSd,
  targetPValue = targetPValue
)
makeCloud(normalData)
makeCloud(skewedI200S10)
skewness(skewedI200S10)

# iterations 200k, shake 0.50
set.seed(1)
skewedI200S50 <- simulateAnnealing(
  startData    = seedData,
  fitFunction  = skewness,
  fitTarget    = 3,
  maxIter      = 200000,
  shake        = 0.50,
  targetMean   = targetMean,
  targetSd     = targetSd,
  targetPValue = targetPValue
)
makeCloud(normalData)
makeCloud(skewedI200S50)
skewness(skewedI200S50)

# iterations 200k, shake 0.30
set.seed(1)
skewedI200S30 <- simulateAnnealing(
  startData    = seedData,
  fitFunction  = skewness,
  fitTarget    = 3,
  maxIter      = 200000,
  shake        = 0.30,
  targetMean   = targetMean,
  targetSd     = targetSd,
  targetPValue = targetPValue
)
makeCloud(normalData)
makeCloud(skewedI200S30)
skewness(skewedI200S30)

# Conclusion:
# 200k with 0.50 shake seems to have worked best.
# Now, lets try 500k 0.10
set.seed(1)
skewedI500S10 <- simulateAnnealing(
  startData    = seedData,
  fitFunction  = skewness,
  fitTarget    = 3,
  maxIter      = 500000,
  shake        = 0.10,
  targetMean   = targetMean,
  targetSd     = targetSd,
  targetPValue = targetPValue
)
makeCloud(normalData)
makeCloud(skewedI500S10)
skewness(skewedI500S10)
# That is also not as good.

# # Lets keep I200S50 for now.
# write.csv(skewedI200S50, "./quartetData/v1skewedData.csv")  # Commented out just for safety
v1skewedData <- read.csv("./quartetData/v1skewedData.csv")
v1skewedData <- as.vector(v1skewedData[ , 2])



# III: Bimodal distribution ----
# freeman2015: bimodality coefficient and hartigans dip statistic generally convergent
# further: bimodality coeffient makes an easy fitTarget in simulateAnnealing(), so lets go with that first

makeCloud(seedData)
bimodality_coefficient(seedData)  # 0.507
bimodality_coefficient(normalData)  # 0.27
# pfister2013: > .555 indicates bimodality


# Lets try startData = normalData; Note: seedData was not as good when I tried out various settings
# bimodal_normStart_i250_s05_t700
set.seed(1)
bimodal_normStart_i250_s05_t700 <- simulateAnnealing(
  startData    = normalData,
  fitFunction  = bimodality_coefficient,
  fitTarget    = .700,
  maxIter      = 250000,
  shake        = 0.05,
  targetMean   = targetMean,
  targetSd     = targetSd,
  targetPValue = targetPValue
)
makeCloud(bimodal_normStart_i250_s05_t700, -0.8, 1.25)  # Not bad at all!
bimodality_coefficient(bimodal_normStart_i250_s05_t700)
skewness(bimodal_normStart_i250_s05_t700)  # It would be great to have this, but with negative skewness!


# Increase iterations
# bimodal_normStart_i255_s05_t700
set.seed(1)
bimodal_normStart_i255_s05_t700 <- simulateAnnealing(
  startData    = normalData,
  fitFunction  = bimodality_coefficient,
  fitTarget    = .700,
  maxIter      = 255000,
  shake        = 0.05,
  targetMean   = targetMean,
  targetSd     = targetSd,
  targetPValue = targetPValue
)
makeCloud(bimodal_normStart_i255_s05_t700, -0.8, 1.25)
bimodality_coefficient(bimodal_normStart_i255_s05_t700)
skewness(bimodal_normStart_i255_s05_t700)


# Interim summary:
# bimodal_normStart_i250_s05_t700 gives a good looking bimodal, but with positive skewness
# I will now use it as a starter and try to get negative skew


# bimodal_custom_i050_s05_t650
bimodal_custom_i050_s05_t650 <- simulateAnnealing(
  startData    = bimodal_normStart_i250_s05_t700,
  fitFunction  = bimodality_coefficient,
  fitTarget    = .650,
  maxIter      = 50000,
  shake        = 0.05,
  targetMean   = targetMean,
  targetSd     = targetSd,
  targetPValue = targetPValue
)
makeCloud(bimodal_custom_i050_s05_t650, -0.8, 1.25)
bimodality_coefficient(bimodal_custom_i050_s05_t650)
skewness(bimodal_custom_i050_s05_t650)

# Lets compare current two contenders with rest seedData and normalData
makeCloud(normalData, -0.8, 1.25)
makeCloud(v1skewedData, -0.8, 1.25)
makeCloud(bimodal_normStart_i250_s05_t700, -0.8, 1.25)
makeCloud(bimodal_custom_i050_s05_t650, -0.8, 1.25)


# # Lets keep the second contender with negative skew for now.
# write.csv(bimodal_custom_i050_s05_t650, "./quartetData/v1bimodalData.csv")  # Commented out just for safety
v1bimodalData <- read.csv("./quartetData/v1bimodalData.csv")
v1bimodalData <- as.vector(v1bimodalData[ , 2])


# III: Outliers ----

# Lets look at the current quartet
makeCloud(normalData, -1, 1.075)
makeCloud(v1skewedData, -1, 1.075)
makeCloud(v1bimodalData, -1.25, 1.075)


# Outliers start after about 2 standard deviations.
# Lets do 11 on each side.
outlierLoss <- function(inputData) {
  loss <- 0
  sortedData <- inputData[order(inputData)]

  # First 11 values should be lower than lowerTarget
  lowerTarget <- -1.04  # targetMean - 4 * targetSd
  lowerLoss   <- sum(inputData[1:11] - lowerTarget)   # Upward deviation increases loss, downward reduces!

  # In analogy for last 11 values
  upperTarget <- 1.12  # targetMean + 4 * targetSd
  upperDelta  <- sum(inputData[101:111] - upperTarget)
  upperLoss   <- sum(upperDelta * -1)  # Downward deviations (which are negative), should increase loss, thus * -1

  loss <- lowerLoss + upperLoss
  return(loss)
}


sortedNormalData <- normalData[order(normalData)]  # Also order it, to get correct loss in very first iteration.
outlierLoss(sortedNormalData)


v1Outlier <- simulateAnnealing(
  startData    = sortedNormalData,
  fitFunction  = outlierLoss,
  fitTarget    = 0,
  maxIter      = 200000,
  shake        = 0.05,
  targetMean   = targetMean,
  targetSd     = targetSd,
  targetPValue = targetPValue
)
makeCloud(v1Outlier, -1, 1.075)  # That looks good!
outlierLoss(v1Outlier)

# write.csv(v1Outlier, "./quartetData/v1outlierData.csv")  # Commented out just for safety
v1outlierData <- read.csv("./quartetData/v1outlierData.csv")
v1outlierData <- as.vector(v1outlierData[ , 2])



# The Raincloud Quartet ----
axisMin = -1
axisMax = 1.075
makeCloud(normalData, axisMin, axisMax)
makeCloud(v1skewedData, axisMin, axisMax)
makeCloud(v1bimodalData, axisMin, axisMax)
makeCloud(v1outlierData, axisMin, axisMax)

v1RaincloudQuartet <- data.frame(

  distribution = c(
    rep("normal",  111),
    rep("skewed",  111),
    rep("bimodal", 111),
    rep("outlier", 111)
  ),

  data <- c(
    normalData,
    v1skewedData,
    v1bimodalData,
    v1outlierData
  )

)
colnames(v1RaincloudQuartet)[2] <- "data"
v1RaincloudQuartet$distribution <- as.factor(v1RaincloudQuartet$distribution)

# write.csv(v1RaincloudQuartet, "./quartetData/v1RaincloudQuartet.csv")

