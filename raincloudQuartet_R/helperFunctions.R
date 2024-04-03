
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



# Matejka & Fitzmaurice (2017) ----

isErrorOk <- function(inputVector, targetMean, targetSd, targetPValue) {

  inputMean   <- round(mean(inputVector), 2)
  inputSd     <- round(sd(inputVector), 2)
  inputPValue <- getP(inputVector)

  matchMean   <- inputMean   == targetMean
  matchSd     <- inputSd     == targetSd
  matchPValue <- inputPValue == targetPValue

  sameStats <- matchMean && matchSd && matchPValue

  if (sameStats) return(TRUE) else return(FALSE)
}



