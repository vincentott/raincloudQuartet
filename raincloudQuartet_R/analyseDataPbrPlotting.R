#
#
# Data Analysis: Types of Plots published in Psychonomic Bulletin & Review in 2023
#
# Written by Vincent L. Ott, University of Amsterdam, The Netherlands
# 2024
#
#



# Libraries ----
library("tidyverse")
library("readxl")
library("ggplot2")



# Load data ----
dataPbr <- read_excel("dataPbrPlotting.xlsx")


# Reports ----

# How many were published?
dataPbr %>% group_by(year) %>% summarize(amount = n())
# 121 in 2013
# 133 in 2023


# How many are applicable?
# Such reports for example explored correlations or used machine learning methods.
# This meant that plotting data with discrete predictors and a continuous target variable did not apply.
# "dna" means "does not apply"
dataApply <- dataPbr %>% filter(didPlot != "dna")
dataApply %>% group_by(year) %>% summarize(amount = n())
# 99 in 2013
99 / 121  # 81%

# 105 in 2023
105 / 133  # 79%


# How many used a tTest or ANOVA?
dataApply %>% group_by(year, tTestANOVA) %>% summarize(amount = n())
# In 2013, 5 did not, the other 94 did.
94 / 99  # 95%

# In 2023, 24 did not, the other 81 did.
81 / 105  # 77%


# tTestANOVA are the scenarios where raincloud plots *can* be very useful.
# Where they were not used, those brief reports had still group comparisons (e.g. fixed effects in mixed model),
# and the authors did plot or could have plotted the data accordingly (e.g. barPlot, similarToRaincloud, or raincloud).


# Plot Types ----

# How many of the applicable reports plotted the data?
dataApply %>% group_by(year, didPlot) %>% summarize(amount = n())
# In 2013, 81 did plot their data.
81 / 99  # 81%

# In 2023, 94 did plot their data.
94 / 105  # 90%


# What were the plot types?
dataTypes2013 <- dataApply %>%
  filter(year == 2013, didPlot == "yes") %>%
  group_by(year, plotType) %>%
  summarize(amount = n())
dataTypes2013$percent <- round( ( dataTypes2013$amount / sum(dataTypes2013$amount) ) * 100 )
dataTypes2013
sum(dataTypes2013$percent)  # Only 99% due to rounding


dataTypes2023 <- dataApply %>%
  filter(year == 2023, didPlot == "yes") %>%
  group_by(year, plotType) %>%
  summarize(amount = n())
dataTypes2023$percent <- round( ( dataTypes2023$amount / sum(dataTypes2023$amount) ) * 100 )
dataTypes2023
sum(dataTypes2023$percent)


