# RH_Survival
Looking at survivalship curves and other traits of Anopheles at different relative humidities
Directly copied from RStudio

---
title: "Relative Humidity"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


#Data analysis for relative humidity experiments with Anopheles
##I want survivorship, eggs laid for both blood feeds individually, propensity to blood feed
###Maybe include lifespan, vectoral capacity
```{r}
#install tidyverse for ggplot2
install.packages("tidyverse")
library(tidyverse)
```
I want to make a survival curve. So I'll use ggfortify which lets ggplot2 know how to draw survival curves
```{r}
#install ggfortify
install.packages('ggfortify')
library(ggfortify)
library(survival)
```
```{r}
#load my survivorship data
surv <- read.csv("file:///C:/Users/Lilith South/Documents/spring_2019/research/survival_data.csv")

#rename the first column to "replicate"
names(surv) <- c("Replicate", "RH", "H20", "ID", "Age")

#inspect
surv
```
#For surv 
Replicate: 1=A, 2=B
Relative humidity(RH): 1= 50 "low", 2=70 "intermediate", 3=90 "high"
water stress(H20): 1=8 hours with water per day,2 =16 hours, 3=24
Age = days after initial blood feed that mosquito died. 

```{r}
#lets separate the different RH so we can have 3 different plots. Low= 50% RH, Int= 70% RH, High= 90%RH
Low <- surv %>%
  filter(RH=="1")

Int <- surv %>%
  filter(RH=="2")

High <- surv %>%
  filter(RH=="3")

Low
Int
High

```


```{r}
#lets plot survival curves
##will this function work with how I have my data laid out?
Low_Surv <- survfit(Surv(Age) ~ H20, data = Low)
autoplot(Low_Surv)

Int_Surv <- survfit(Surv(Age) ~ H20, data = Int)
autoplot(Int_Surv)

High_Surv <- survfit(Surv(Age) ~ H20, data = High)
autoplot(High_Surv)

```
