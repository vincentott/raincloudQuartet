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

  dependentVariable = c(
    rnorm(mean = 0.49,  sd = 0.09, n = sampleSizePerGroup),
    rnorm(mean = 0.46, sd = 0.10, n = sampleSizePerGroup),
    rnorm(mean = 0.67,  sd = 0.11, n = sampleSizePerGroup)
  ),

  covariate = c(  # Higher in the first two levels than in levelC
    round(rnorm(mean = 34, sd = 6, n = sampleSizePerGroup)),
    round(rnorm(mean = 34, sd = 6, n = sampleSizePerGroup)),
    round(rnorm(mean = 15, sd = 6, n = sampleSizePerGroup))
  )

)

# Check that covariate is above 0
design3x1 %>% filter(covariate < 0) %>% nrow()


# Visualize
ggplot(design3x1, aes(factorA, dependentVariable, fill = factorA)) +
  geom_rain(alpha = .5, id.long.var = "id") +
  theme_classic() +
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = 'none') +
  scale_color_viridis_c(option =  "A")


# Create some odd observations, that can be detected with subjectLines


