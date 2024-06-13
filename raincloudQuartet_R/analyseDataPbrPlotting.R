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
dataPlotting <- read_excel("dataPbrPlotting.xlsx")



# Relevant Reports ----
( nTotal <- nrow(dataPlotting) )
# There where 133 brief reports published in 2023.

dataPlotting %>% group_by(didPlot, tTestANOVA) %>% summarize(amount = n())
# First row shows that 28 reports did not use a tTest or ANOVA but were also not applicable otherwise.
# dna: "did no apply"
# Such reports for example explored correlations or used machine learning methods.
# This meant that plotting data with discrete predictors and a continuous target variable did not apply.




dataApply <- dataPlotting %>% filter(didPlot != "dna")
( nApply <- nrow(dataApply) )
# Thus, 105 reports that are relevant here.

dataApply %>% group_by(tTestANOVA) %>% summarize(amount = n())
# 81 reported a tTest or an ANOVA; these are the scenarios where raincloud plots *can* be very useful.
# 24 did not report a tTest or an ANOVA; mostly these studies used mixed effects regressions.
# However, in all these 24 cases, there were still group comparisons (fixed effects like in tTest and ANOVA),
# and the authors did plot or could have plotted the data accordingly (e.g. barPlot, similarToRaincloud, or raincloud).



# Reports with tTest or ANOVA: N = 81 ----
# First, lets look at the 81 reports that had a tTest or ANOVA.
dataTTestAnova <- dataApply %>% filter(tTestANOVA == "yes")

dataTTestAnova %>% group_by(didPlot) %>% summarize(amount = n())
# 72 reports also plotted their data.
# 9 did not.

dataTTestAnova %>% filter(didPlot == "no") %>% group_by(ifNotPlotTable) %>% summarize(amount = n())
# Of those 9 reports who did not plot, 4 reported the results in a table and 5 did not.

dataTTestAnovaDidPlot <- dataTTestAnova %>% filter(didPlot == "yes") %>% group_by(plotType) %>% summarize(amount = n())
dataTTestAnovaDidPlot

dataTTestAnovaDidPlot$plotType <- factor(
  dataTTestAnovaDidPlot$plotType,
  levels = c("raincloud", "similarToRaincloud", "linePlot", "pointShowsMean", "barPlot")
)

# Visualize
ggplot(dataTTestAnovaDidPlot, aes(x = 1, y = amount, fill = plotType)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("forestgreen", "lightgreen", "purple", "#F9DDB1", "orange"))




# Special Cases: N = 24 ----
dataSpecial <- dataApply %>% filter(tTestANOVA == "no")
dataSpecial %>% group_by(didPlot) %>% summarize(amount = n())
# 22 of the special cases plotted their data.
# 2 did not.

dataSpecial %>% filter(didPlot == "no") %>% group_by(ifNotPlotTable) %>% summarize(amount = n())
# Both of these reports that did not plot, still reported the results in a table.


dataSpecialDidPlot <- dataSpecial %>% filter(didPlot == "yes") %>% group_by(plotType) %>% summarize(amount = n())
dataSpecialDidPlot

dataSpecialDidPlot$plotType <- factor(
  dataSpecialDidPlot$plotType,
  levels = c("raincloud", "similarToRaincloud", "boxPlot", "barPlot")
)

# Visualize
ggplot(dataSpecialDidPlot, aes(x = 1, y = amount, fill = plotType)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("forestgreen", "lightgreen", "lightblue", "orange"))



# Visualize tTestANOVA and Special Cases separately ----
dataVisualize <- dataApply %>% filter(didPlot == "yes") %>% group_by(tTestANOVA, plotType) %>% summarize(amount = n())
dataVisualize

dataVisualize$tTestANOVA <- factor(dataVisualize$tTestANOVA, levels = c("yes", "no"))

dataVisualize$plotType <- factor(
  dataVisualize$plotType,
  levels = c("raincloud", "similarToRaincloud", "linePlot", "pointShowsMean", "boxPlot", "barPlot")
)

ggplot(dataVisualize, aes(x = tTestANOVA, y = amount, fill = plotType)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_classic() +
  scale_fill_manual(
    values = c(
      "#53c12c", "#94e2d9",
      "#9692ff", "#4a6ac3",
      "#ff9fb1", "#7c1158"
    )
  ) +
  scale_y_continuous(limits = c(0, 75), breaks = seq(0, 75, by = 10))



# Visualize collapsed ----
dataCollapsed <- dataApply %>% filter(didPlot == "yes") %>% group_by(plotType) %>% summarize(amount = n())
dataCollapsed
dataCollapsed$plotType <- factor(
  dataCollapsed$plotType,
  levels = c("barPlot", "pointShowsMean", "boxPlot", "linePlot", "similarToRaincloud", "raincloud")
)

# Visualize Proportions
# Exported as pdf with size: 4.38 x 4.86 inches
ggplot(dataCollapsed, aes(x = 1, y = amount, fill = plotType)) +
  geom_bar(position = "fill", stat = "identity") +
  theme_classic() +
  scale_fill_manual(
    values = c(
      "#BA0057", "#FB8B00",
      "#424342", "#C2AFF0",
      "#00A9E6", "#00BA63"
    )
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.20),
    labels = scales::percent_format(accuracy = 1)
  ) +
  ylab("Percent")

# Calculate proportions
nPlotted <- sum(dataCollapsed$amount)
dataCollapsed$perc <- round(dataCollapsed$amount / nPlotted, 2)
dataCollapsed
sum(dataCollapsed$perc)  # Rounding successful



# Percentages that we report ----

round(105 / 133, 2) * 100  # 79% applicable cases out of 133 published brief reports

round(81 / 105, 2) * 100   # 77% of the applicable cases reported tTest or ANOVA
round(24 / 105, 2) * 100   # 23% reported more complex models (such as linear mixed effects)

round(94 / 105, 2) * 100   # 90 % of the applicable cases plotted their data
