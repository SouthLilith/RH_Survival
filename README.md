---
title: "Statistical Analysis"
Author: "Lilith South"
date: "March 28th 2019"
output:
  word_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
##Survival Analysis

###Kaplain-Meir Curves & Associated Plots

###Step 1: Importing data & formating data for survival analysis

#First: dowload packages

```{r}
#install tidyverse for ggplot2
install.packages("tidyverse")
install.packages("ggplot2")

#I want to make a survival curve. So I'll install ggfortify which lets ggplot2 know how to draw survival curves

install.packages('ggfortify')
install.packages("survival")

#ggsurvplot not recognized, install survminer
install.packages("survminer")

```

```{r}
library(tidyverse)
library(ggplot2)
library(ggfortify)
library(survival)
library(survminer)

```


```{r}

#load my survivorship data
SurvivalData <- read.csv("file:///C:/Users/Lilith South/Documents/spring_2019/research/survival_data.csv")

#rename the first column to "replicate"
names(SurvivalData) <- c("Day", "Replicate", "RH", "H2O", "ID", "Event", "drown")

#inspect
SurvivalData

#Set RH, H2O as factors
SurvivalData$RH <- as.factor(SurvivalData$RH)
SurvivalData$H2O <- as.factor(SurvivalData$H2O)

```
#For SurvivalData
Day: Number of days after initial blood feed
Replicate: 1=A, 2=B 
Relative humidity(RH): 1= 50 "low", 2=70 "intermediate", 3=90 "high" 
water stress(H2O): 1=8 hours with water per day,2 =16 hours, 3=24 
Event: 0=alive, no event, 1= dead, event

###Step 2: Generating the Kaplain-Meir Curve & Log-rank Test
NOTE: FIND OUT WHAT GENERATED THE CSV KAPLAN MEIR CURVE ESTIMATES
```{r}
#To plot the survival probabilities over time

surv_RH <- survfit(Surv(Day,Event) ~ RH, data = SurvivalData)
surv_H2O <- survfit(Surv(Day,Event) ~ H2O, data = SurvivalData)
surv_H2ORH <- survfit(Surv(Day,Event) ~ H2O + strata(RH), data = SurvivalData)

# Kaplan-Meier tests for H2O, RH, H2O stratified by RH

surv.diff.H2O <- survdiff(Surv(Day,Event) ~ H2O, data = SurvivalData)
surv.diff.H2O

surv.diff.RH <- survdiff(Surv(Day, Event) ~ H2O + strata(RH), data=SurvivalData)
surv.diff.RH

surv.diff.RH.solo <- survdiff(Surv(Day,Event) ~ RH, data = SurvivalData)
surv.diff.RH.solo


surv_H2ORH
```

###Step 3: Plotting the Kaplain-Meir Curve 
```{r echo= FALSE}

#KM curve for each rRH seperately
surv <- ggsurvplot(surv_RH, data = SurvivalData, conf.int = T,
           size = 1.2,
           linetype = "solid",
           xlab = "Day",
           break.x.by = 5,
           break.y.by = .5,
           xlim = c(0,45,2),
           legend.title = "RH",
           font.tickslab = c (12, "plain", "black"),
           ggtheme= theme_bw(base_size = 16))
surv
```

```{r}
x <- ggsurvplot_facet(surv_H2O, data = SurvivalData, conf.int= T, facet.by = "RH")
x

y<-ggsurvplot_facet(surv_RH, data = SurvivalData, conf.int= T, facet.by = "H2O")
y
```

I might have to take out the first replicate of 24 hour access to water due to drowning deaths
#gompertz curve & area under the curve to find average lifespan
```{r}
library(dplyr)
filter <- SurvivalData %>%
  filter(drown != "1")
filter
```
#I want to filter our mosquitoes that drowned
```{r}
#I want to change the RH labels
labels <- c( "50%", "70%", "90%")


x.filter <- ggsurvplot_facet(surv_H2O, data = filter, conf.int= T, facet.by = "RH", xlab = "Day", legend = c("8", "16", "24"), legend.title = "Water Stress")
x.filter

```

#Find lifetime egg production
#Find proper distribution for lifespand curve
#second blood feeding 
