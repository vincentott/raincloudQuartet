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



