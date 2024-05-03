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



# Hyperparameters ----
sampleSize   <- 111  # Hommage to Anscombe (1973)
targetPValue <- 0.049



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
inspectStats(sortedSeedData)
makeCloud(sortedSeedData)
getP(sortedSeedData)

# write.csv(sortedSeedData, "./quartetData/seedData.csv")  # Commented out just for safety
