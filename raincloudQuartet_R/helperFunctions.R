
# helperFunctions


# Libraries ----
library(moments)
library(ggplot2)
library(ggrain)



# makeCloud() ----
makeCloud <- function(inputVector) {
  title     <- deparse(substitute(inputVector))
  df        <- as.data.frame(inputVector)
  meanValue <- mean(inputVector)
  sdValue   <- sd(inputVector)
  plot <- ggplot(df, aes(1, .data$inputVector)) +
    geom_rain(point.args = list(alpha = 0.5)) +
    theme_classic() +
    coord_flip() +
    annotate("point", x = 1, y = meanValue, color = "blue", size = 5, shape = 18) +  # Add mean point
    geom_errorbar(aes(ymin = meanValue - sdValue, ymax = meanValue + sdValue), width = 0.05, color = "blue", lwd = 1) +
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    labs(title = title) +
    ylim(-2, 2)
    ylab(title)  # Because of coord_flip()
  return(plot)
}  # End makeCloud()



# inspectStats() ----
inspectStats <- function(inputVector) {

  stats <- data.frame(
    name = deparse(substitute(inputVector)),
    mean = round(mean(inputVector), 2),
    sd   = round(sd(inputVector), 2),
    skew = round(skewness(inputVector), 2),
    kurt = round(kurtosis(inputVector), 2)
  )

  return(stats)
}  # End inspectStats



# getP() ----
getP <- function(inputVector, mu = 0) {
  return(round(t.test(inputVector, alternative = "greater", mu = mu)[["p.value"]], 3))
}  # End getP()



# simulateAnnealing(); see Matejka & Fitzmaurice (2017) ----
simulateAnnealing <- function(
    startData, fitFunction, fitTarget, maxIter = 20000, targetMean, targetSd, targetPValue
) {

  currentData <- startData

  for (i in 1:maxIter) {
    currentTemperature <- calculateTemperature(i, maxIter)

    if (i %% 50000 == 0) {
      print(gettextf("Iteration %s with temperature %s", i, currentTemperature))
    }
    testData           <- perturb(currentData, fitFunction, fitTarget, currentTemperature)
    if (isErrorOk(testData, targetMean, targetSd, targetPValue)) {
      currentData <- testData
    }
  }
  return(currentData)
}  # End simulateAnnealing()



# calculateTemperature(); see Matejka & Fitzmaurice (2017) ----
# Based on their original python code:
# https://github.com/jmatejka/same-stats-different-graphs/blob/master/samestats/samestats.py
# as well as ChatGPT
calculateTemperature <- function(currentIter, maxIter, maxTemp = 0.4, minTemp = 0.01) {
  outScurve   <- sCurve((maxIter - currentIter) / maxIter)
  temperature <- (maxTemp - minTemp) * outScurve + minTemp
  return(temperature)
}  # End calculateTemperature()



# sCurve() ----
# Based on Matejka & Fitzmaurice (2017)´s original python code:
# https://github.com/jmatejka/same-stats-different-graphs/blob/master/samestats/samestats.py
# as well as ChatGPT
sCurve <- function(x) {
  numerator   <- x^2
  denominator <- x^2 + (1 - x)^2
  return(numerator / denominator)
}  # End sCurve()



# perturb(); see Matejka & Fitzmaurice (2017) ----
perturb <- function(currentData, fitFunction, fitTarget, currentTemperature) {

  currentLoss  <- abs(fitTarget - fitFunction(currentData))
  # print(gettextf(" Current Loss:   %s", currentLoss))

  perturbedData <- currentData
  while (TRUE) {
    # perturbedData <- moveRandomPoint(currentData)  # FIX  THIS LINE OVER HERE!

    randomIndices <- sample(1:length(perturbedData), 6)
    perturbedData[randomIndices] <- perturbedData[randomIndices] + rnorm(6) * 0.50
    perturbedData <- round(perturbedData, 4)
    perturbedLoss <- abs(fitTarget - fitFunction(perturbedData))
    # print(gettextf(" Perturbed Loss: %s", perturbedLoss))

    betterFit       <- perturbedLoss < currentLoss
    acceptWorseFit  <- runif(1) < currentTemperature  # As temperature cools down, less likely to accept perturbedData with worse fit

    if (betterFit || acceptWorseFit) break
  }

  return(perturbedData)
}  # End perturb()


# moveRandomPoint(); see Matejka & Fitzmaurice (2017) ----
moveRandomPoint <- function(inputData) {
  randomIndices          <- sample(1:length(inputData), 6)
  change                 <- rnorm(6) * 0.10
  inputData[randomIndices] <- inputData[randomIndices] + change
  return(inputData)
}  # End moveRandomPoint()



# isErrorOk(); see Matejka & Fitzmaurice (2017) ----
isErrorOk <- function(inputVector, targetMean, targetSd, targetPValue) {

  inputMean   <- round(mean(inputVector), 2)
  inputSd     <- round(sd(inputVector), 2)
  inputPValue <- getP(inputVector)

  matchMean   <- inputMean   == targetMean
  matchSd     <- inputSd     == targetSd
  matchPValue <- inputPValue == targetPValue

  sameStats <- matchMean && matchSd && matchPValue

  if (sameStats) return(TRUE) else return(FALSE)
}  # End isErrorOk()
