#
#
# Create Data to Showcase different Types of Raincloud Plots in JASP
#
# Written by Vincent L. Ott, University of Amsterdam, The Netherlands
# 2024
#
#



# Libraries ----
library("tidyverse")
library("ggplot2")
library("ggrain")



# 3x1 Design ----
sampleSizePerGroup <- 50

set.seed(1)
design3x1 <- data.frame(

  id = rep(1:sampleSizePerGroup, 3),  # 3, because factorA has three levels

  factorA = c(
    rep("levelA", sampleSizePerGroup),
    rep("levelB", sampleSizePerGroup),
    rep("levelC", sampleSizePerGroup)
  ),

  covariate = c(  # Higher in the first two levels than in levelC
    round(rnorm(mean = 34, sd = 6, n = sampleSizePerGroup)),
    round(rnorm(mean = 34, sd = 6, n = sampleSizePerGroup)),
    round(rnorm(mean = 15, sd = 6, n = sampleSizePerGroup))
  )

)

# Check that covariate is above 0
design3x1 %>% filter(covariate < 0) %>% nrow()

# Add dependentVariable
set.seed(1)
dvLevelA <- rnorm(mean = 0.50,  sd = 0.10, n = sampleSizePerGroup)
dvLevelA <- sort(dvLevelA, decreasing = TRUE)

design3x1$dependentVariable <- rep(dvLevelA, 3)

# Visualize interim result
ggplot(design3x1, aes(factorA, dependentVariable, fill = factorA)) +
  geom_rain(alpha = .5, cov = "covariate", id.long.var = "id") +
  theme_classic() +
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = 'none') +
  scale_color_viridis_c(option =  "A")


# IDs 6 to 20 wont change across the three levels, but the rest will
set.seed(1)
dvLevelB <- dvLevelA + c(
  rnorm(mean = -0.01, sd = 0.05, 5),
  rep(0, 15),
  rnorm(mean = -0.01, sd = 0.05, 30)
)

set.seed(1)
dvLevelC <- dvLevelA + c(
  rnorm(mean = 0.10, sd = 0.05, 5),
  rep(0, 15),
  rnorm(mean = 0.11, sd = 0.05, 30)
)

design3x1$dependentVariable <- c(dvLevelA, dvLevelB, dvLevelC)

design3x1$dependentVariable <- round(design3x1$dependentVariable, 2)  # Round to percent (2 decimal places)

ggplot(design3x1, aes(factorA, dependentVariable, fill = factorA)) +
  geom_rain(
    alpha = .5,
    id.long.var = "id",
    cov = "covariate"
  ) +
  theme_classic() +
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = 'none') +
  scale_color_viridis_c(option =  "A")



# write.csv(design3x1, "./showcaseData/design3x1.csv")  # Commented out just for safety



# 2x2 Design ----

# factorT is within subjects:  levels 1 & 2
# factorG is between subjects: levels X & Y

sampleSizePerGroup <- 65  # how many subjects are in each of the two levels of factorT?

dfT1 <- data.frame(
  id = c(1:130),
  factorT = c(rep("1", sampleSizePerGroup), rep("1", sampleSizePerGroup)),
  factorG = c(rep("X", sampleSizePerGroup), rep("Y", sampleSizePerGroup))
)

set.seed(1)
dependentVariableT1 <- c(
  rnorm(mean = 580, sd = 75, n = sampleSizePerGroup),
  rnorm(mean = 215, sd = 75, n = sampleSizePerGroup)
)

dfT1$dependentVariable <- round(dependentVariableT1 / 10) * 10  # Round to nearest 10

set.seed(2)
dependentVariableT2 <- dependentVariableT1 + c(
  rnorm(mean = -170, sd = 55, n = sampleSizePerGroup),
  rnorm(mean =  260, sd = 55, n = sampleSizePerGroup)
)

dfT2 <- data.frame(
  id = c(1:130),
  factorT = c(rep("2", sampleSizePerGroup), rep("2", sampleSizePerGroup)),
  factorG = c(rep("X", sampleSizePerGroup), rep("Y", sampleSizePerGroup))
)

dfT2$dependentVariable <- round(dependentVariableT2 / 10) * 10


# Combine T1 and T2 measurements
design2x2 <- rbind(dfT1, dfT2)

# Visualize
ggplot(design2x2, aes(factorT, dependentVariable, fill = factorG)) +
  geom_rain(
    alpha = .5,
    rain.side = "f2x2",
    id.long.var = "id"
  ) +
  theme_classic() +
  scale_fill_brewer(palette = 'Dark2')


# write.csv(design2x2, "./showcaseData/design2x2.csv")  # Commented out just for safety



# 3x2 Design ----

# fully between design

# factorM is for the three manipulations
# factorG are the two groups

sampleSizePerGroup <- 40

set.seed(1)
design3x2 <- data.frame(

  factorM = c(
    rep(rep("A", sampleSizePerGroup), 2),  # There are two groups (factorG), so outer rep() is 2
    rep(rep("B", sampleSizePerGroup), 2),
    rep(rep("C", sampleSizePerGroup), 2)
  ),

  factorG = c(
    rep(
      c(rep("experimental", sampleSizePerGroup), rep("control", sampleSizePerGroup)),
      3
    )
  ),

  dependentVariable = c(

    rnorm(sampleSizePerGroup, .70, .10),  # Both groups are equal in first manipulation
    rnorm(sampleSizePerGroup, .70, .10),

    rnorm(sampleSizePerGroup, .45, .10),  # Both groups are equal, but lower in second
    rnorm(sampleSizePerGroup, .45, .10),

    rnorm(sampleSizePerGroup, .70, .10),  # Groups differ
    rnorm(sampleSizePerGroup, .45, .10)

  )
)

# Visualize
ggplot(design3x2, aes(factorM, dependentVariable, fill = factorG)) +
  geom_rain(
    alpha = .5,
    rain.side = "f",
  ) +
  theme_classic() +
  scale_fill_brewer(palette = 'Dark2')


# write.csv(design3x2, "./showcaseData/design3x2.csv")  # Commented out just for safety



