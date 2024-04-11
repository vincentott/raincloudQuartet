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
# Aim for skewness =  3; see blanca2013 in methodology journal

# iterations 200k, shake 0.50
set.seed(1)
v1SkewedData <- simulateAnnealing(
  startData    = seedData,
  fitFunction  = skewness,
  fitTarget    = 3,
  maxIter      = 200000,
  shake        = 0.50,
  targetMean   = targetMean,
  targetSd     = targetSd,
  targetPValue = targetPValue
)
makeCloud(normalData, -1.1, 1.1)
makeCloud(v1SkewedData, -1.1, 1.1)  # Looks good!
skewness(v1SkewedData, -1.1, 1.1)

# However, the left density is a bit cut-off.
# Thus, some manual tuning.
# First we order the data:
wipSkewedData <- v1SkewedData[order(v1SkewedData)]
isErrorOk(wipSkewedData, targetMean, targetSd, targetPValue)

wipSkewedData[1] <- wipSkewedData[1] - 0.05
makeCloud(v1SkewedData, -1.1, 1.1)
makeCloud(wipSkewedData, -1.1, 1.1)
isErrorOk(wipSkewedData, targetMean, targetSd, targetPValue)
inspectStats(wipSkewedData)
getP(wipSkewedData)  # P is too high now.

# So, we increase another low value.
wipSkewedData[1:10]
# Maybe take
wipSkewedData[4]

fine <- isErrorOk(wipSkewedData, targetMean, targetSd, targetPValue)
while (fine == FALSE) {
  wipSkewedData[4] <- wipSkewedData[4] + 0.0001
  fine <- isErrorOk(wipSkewedData, targetMean, targetSd, targetPValue)
}
print(fine)


# Final Check:
makeCloud(v1SkewedData, -1.1, 1.1)
makeCloud(wipSkewedData, -1.1, 1.1)
isErrorOk(wipSkewedData, targetMean, targetSd, targetPValue)
inspectStats(wipSkewedData)
getP(wipSkewedData)


# write.csv(wipSkewedData, "./quartetData/skewedData.csv")  # Commented out just for safety
skewedData <- read.csv("./quartetData/skewedData.csv")
skewedData <- as.vector(skewedData[ , 2])
makeCloud(skewedData)



# III: Bimodal distribution ----
# freeman2015: bimodality coefficient and hartigans dip statistic generally convergent
# further: bimodality coeffient makes an easy fitTarget in simulateAnnealing(), so lets go with that first

makeCloud(seedData)
bimodality_coefficient(seedData)  # 0.507
bimodality_coefficient(normalData)  # 0.27
# pfister2013: > .555 indicates bimodality

# startData = normalData // Note: seedData was not as good when I tried out various settings.
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
makeCloud(bimodal_normStart_i250_s05_t700, -1.1, 1.1)  # Not bad at all!
bimodality_coefficient(bimodal_normStart_i250_s05_t700)
skewness(bimodal_normStart_i250_s05_t700)  # It would be great to have this, but with negative skewness!

# Interim summary:
# bimodal_normStart_i250_s05_t700 gives a good looking bimodal, but with positive skewness
# I will now use it as a starter and try to get negative skew.

# bimodal_custom_i050_s05_t650
set.seed(1)
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
makeCloud(bimodal_custom_i050_s05_t650, -1.1, 1.1)
bimodality_coefficient(bimodal_custom_i050_s05_t650)
skewness(bimodal_custom_i050_s05_t650)

# This looks great!
# However, maybe we can run it for a few more iterations, to get a slightly negative skew?

# bimodal_custom_i055_s05_t650
set.seed(1)
bimodal_custom_i055_s05_t650 <- simulateAnnealing(
  startData    = bimodal_normStart_i250_s05_t700,
  fitFunction  = bimodality_coefficient,
  fitTarget    = .650,
  maxIter      = 55000,
  shake        = 0.05,
  targetMean   = targetMean,
  targetSd     = targetSd,
  targetPValue = targetPValue
)
makeCloud(bimodal_custom_i055_s05_t650, -1.1, 1.1)
bimodality_coefficient(bimodal_custom_i055_s05_t650)
skewness(bimodal_custom_i055_s05_t650)


# However, the densities are a bit cut-off.
# Thus, some manual fine-tuning just like for skewedData
wipBimodalData <- bimodal_custom_i055_s05_t650[order(bimodal_custom_i055_s05_t650)]
makeCloud(wipBimodalData, -1.1, 1.1)
isErrorOk(wipBimodalData, targetMean, targetSd, targetPValue)

# First lower tail
wipBimodalData[1] <- wipBimodalData[1] - 0.08
makeCloud(wipBimodalData, -1.1, 1.1)
isErrorOk(wipBimodalData, targetMean, targetSd, targetPValue)
inspectStats(wipBimodalData)
getP(wipBimodalData)  # P is too high now.

# Increase another low value.
wipBimodalData[1:10]
# Maybe take
wipBimodalData[6]

fine <- isErrorOk(wipBimodalData, targetMean, targetSd, targetPValue)
while (fine == FALSE) {
  wipBimodalData[6] <- wipBimodalData[6] + 0.0001
  fine <- isErrorOk(wipBimodalData, targetMean, targetSd, targetPValue)
}
print(fine)

makeCloud(wipBimodalData, -1.1, 1.1)


# Now upper tail
upperWipBimodalData <- wipBimodalData[order(wipBimodalData)]  # Assign to new object and restore order.
tail(upperWipBimodalData, 10)
upperWipBimodalData[111] <- upperWipBimodalData[111] + 0.15
makeCloud(upperWipBimodalData, -1.1, 1.1)
inspectStats(upperWipBimodalData)
getP(upperWipBimodalData)  # P is now too low.

# Interim summary: Right tails is fading out more, but the one point also looks a bit lonely.
# I will increase another value, and then tune down the p value.
upperWipBimodalData[109:111]
upperWipBimodalData[107] <- 0.5
makeCloud(upperWipBimodalData, -1.1, 1.1)

isErrorOk(upperWipBimodalData, targetMean, targetSd, targetPValue)
inspectStats(upperWipBimodalData)
getP(upperWipBimodalData)

# Decrease another high value to lower the target statistics.
tail(upperWipBimodalData, 10)
# Maybe take
upperWipBimodalData[104]

fine <- isErrorOk(upperWipBimodalData, targetMean, targetSd, targetPValue)
while (fine == FALSE) {
  upperWipBimodalData[104] <- upperWipBimodalData[104] - 0.0001
  fine <- isErrorOk(upperWipBimodalData, targetMean, targetSd, targetPValue)
}
print(fine)


makeCloud(upperWipBimodalData, -1.1, 1.1)
inspectStats(upperWipBimodalData)
getP(upperWipBimodalData)


# write.csv(upperWipBimodalData, "./quartetData/bimodalData.csv")  # Commented out just for safety
bimodalData <- read.csv("./quartetData/bimodalData.csv")
bimodalData <- as.vector(bimodalData[ , 2])
makeCloud(bimodalData)




# IV: Outliers ----

# Lets look at the current quartet
makeCloud(normalData)
makeCloud(skewedData)
makeCloud(bimodalData)


# Define outlier loss
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

# Execute all of the following code until STOP STOP STOP
set.seed(1)
v1OutlierData <- simulateAnnealing(
  startData         = sortedNormalData,
  fitFunction       = outlierLoss,
  fitTarget         = 0,
  maxIter           = 200000,
  shake             = 0.05,
  targetMean        = targetMean,
  targetSd          = targetSd,
  targetPValue      = targetPValue,
  orderAfterPerturb = TRUE
)
makeCloud(v1OutlierData)
outlierLoss(v1OutlierData)
v2OutlierData <- simulateAnnealing(      # I forgot to set.seed() - and found that this looks nicer.
  startData         = sortedNormalData,  # Thus, requires v1 to be run first (incl. set.seed(1) before v1)
  fitFunction       = outlierLoss,
  fitTarget         = 0,
  maxIter           = 200000,
  shake             = 0.05,
  targetMean        = targetMean,
  targetSd          = targetSd,
  targetPValue      = targetPValue,
  orderAfterPerturb = TRUE
)
# STOP STOP STOP
makeCloud(v2OutlierData)
outlierLoss(v2OutlierData)


# write.csv(v2OutlierData, "./quartetData/outlierData.csv")  # Commented out just for safety
outlierData <- read.csv("./quartetData/outlierData.csv")
outlierData <- as.vector(outlierData[ , 2])
makeCloud(outlierData)



# The Raincloud Quartet ----
makeCloud(normalData)
makeCloud(skewedData)
makeCloud(bimodalData)
makeCloud(outlierData)

raincloudQuartet <- data.frame(

  distribution = c(
    rep("normal",  111),
    rep("bimodal", 111),
    rep("skewed",  111),
    rep("outlier", 111)
  ),

  data <- c(
    normalData,
    bimodalData,
    skewedData,
    outlierData
  )

)
colnames(raincloudQuartet)[2] <- "data"
raincloudQuartet$distribution <- as.factor(raincloudQuartet$distribution)

# write.csv(raincloudQuartet, "./quartetData/raincloudQuartet.csv")  # Commented out just for safety

