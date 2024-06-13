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



# 2x2 Design: irisFertilizer ----

# For this we can use the irisFertilizer data set from the ggrain vignette: https://www.njudd.com/raincloud-ggrain/
set.seed(42)  # The magic number
iris_subset <- iris[iris$Species %in% c('versicolor', 'virginica'),]

iris.long <- cbind(
  rbind(iris_subset, iris_subset, iris_subset),
  data.frame(
    time = c(rep("t1", dim(iris_subset)[1]), rep("t2", dim(iris_subset)[1]), rep("t3", dim(iris_subset)[1])),
    id = c(rep(1:dim(iris_subset)[1]), rep(1:dim(iris_subset)[1]), rep(1:dim(iris_subset)[1]))
  )
)

# adding .5 and some noise to the versicolor species in t2
iris.long$Sepal.Width[iris.long$Species == 'versicolor' & iris.long$time == "t2"] <-
  iris.long$Sepal.Width[iris.long$Species == 'versicolor' & iris.long$time == "t2"] +
  .5 +
  rnorm(length(iris.long$Sepal.Width[iris.long$Species == 'versicolor' & iris.long$time == "t2"]), sd = .2)

# adding .8 and some noise to the versicolor species in t3
iris.long$Sepal.Width[iris.long$Species == 'versicolor' & iris.long$time == "t3"] <-
  iris.long$Sepal.Width[iris.long$Species == 'versicolor' & iris.long$time == "t3"] +
  .8 +
  rnorm(length(iris.long$Sepal.Width[iris.long$Species == 'versicolor' & iris.long$time == "t3"]), sd = .2)

# now we subtract -.2 and some noise to the virginica species
iris.long$Sepal.Width[iris.long$Species == 'virginica' & iris.long$time == "t2"] <-
  iris.long$Sepal.Width[iris.long$Species == 'virginica' & iris.long$time == "t2"] -
  .2 +
  rnorm(length(iris.long$Sepal.Width[iris.long$Species == 'virginica' & iris.long$time == "t2"]), sd = .2)

# now we subtract -.4 and some noise to the virginica species
iris.long$Sepal.Width[iris.long$Species == 'virginica' & iris.long$time == "t3"] <-
  iris.long$Sepal.Width[iris.long$Species == 'virginica' & iris.long$time == "t3"] -
  .4 +
  rnorm(length(iris.long$Sepal.Width[iris.long$Species == 'virginica' & iris.long$time == "t3"]), sd = .2)

iris.long$Sepal.Width <- round(iris.long$Sepal.Width, 1) # rounding Sepal.Width so t2 data is on the same resolution
iris.long$time <- factor(iris.long$time, levels = c('t1', 't2', 't3'))
# End of ggrain vignette code


# Clean up for showcase
irisFertilizer <- iris.long

irisFertilizer$sepalWidth <- irisFertilizer$Sepal.Width  # camelCase to avoid bugs due to "." character

irisFertilizer <- irisFertilizer %>% select(id, time, Species, sepalWidth)

irisFertilizer <- irisFertilizer %>% filter(time != "t2")  # Simplify to 2x2


# Change one slope to stand out idiosyncratically:
# Third highest virginica at t3 should be a bit higher
# Lets find it first in the dataframe
irisFertilizer %>%
  filter(time == "t3", Species == "virginica") %>%
  select(id, sepalWidth) %>%
  arrange(desc(sepalWidth)) %>%
  head(3)

# id == 99; Should go from current 3.4 to 3.6
irisFertilizer[irisFertilizer$id == 99 & irisFertilizer$time == "t3", "sepalWidth"] <- 3.6

nrow(irisFertilizer)


# Visualize
ggplot(irisFertilizer, aes(time, sepalWidth, fill = Species)) +
  geom_rain(
    alpha = .5,
    rain.side = "f2x2",
    id.long.var = "id"
  ) +
  theme_classic() +
  scale_fill_brewer(palette = 'Dark2')


# write.csv(irisFertilizer, "./showcaseData/design2x2.csv")  # Commented out just for safety



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



