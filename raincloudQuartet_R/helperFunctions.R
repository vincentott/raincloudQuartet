
# helperFunctions


# Libraries ----
library(moments)
library("mousetrap")  # bimodality_coefficient()
library(ggplot2)
library(ggrain)



# makeCloud() ----
makeCloud <- function(inputVector, axisMin = -2, axisMax = 2) {
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
    ylim(axisMin, axisMax)
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
    startData, fitFunction, fitTarget, maxIter = 20000, shake = 0.50, targetMean, targetSd, targetPValue,
    orderAfterPerturb = FALSE
) {

  startTime <- Sys.time()

  currentData <- startData

  for (i in 1:maxIter) {
    currentTemperature <- calculateTemperature(i, maxIter)

    if (i %% 50000 == 0) {
      print(gettextf("Iteration %s with temperature %s", i, currentTemperature))
    }
    testData           <- perturb(currentData, fitFunction, fitTarget, currentTemperature, shake, orderAfterPerturb)
    if (isErrorOk(testData, targetMean, targetSd, targetPValue)) {
      currentData <- testData
    }
  }

  endTime <- Sys.time()
  timeTaken <- round(difftime(endTime, startTime, units = "mins"), 2)
  print(gettextf("Simulation ran for %s mins.", timeTaken))
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
perturb <- function(currentData, fitFunction, fitTarget, currentTemperature, shake, orderAfterPerturb) {

  currentLoss  <- abs(fitTarget - fitFunction(currentData))

  perturbedData <- currentData
  while (TRUE) {

    randomIndices <- sample(1:length(perturbedData), 6)
    perturbedData[randomIndices] <- perturbedData[randomIndices] + rnorm(6) * shake
    if (orderAfterPerturb) perturbedData <- perturbedData[order(perturbedData)]
    perturbedData <- round(perturbedData, 4)
    perturbedLoss <- abs(fitTarget - fitFunction(perturbedData))

    betterFit       <- perturbedLoss < currentLoss
    acceptWorseFit  <- runif(1) < currentTemperature  # As temperature cools down, less likely to accept perturbedData with worse fit
    if (betterFit || acceptWorseFit) break
  }

  return(perturbedData)
}  # End perturb()



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
