---
title: "Final Assessment Report"
author: "Adrian Minnig"
toc: true
toc-depth: 4
toc-location: left
format: html
code-fold: true
code-summary: "Show the code"
editor: source
theme:
  light: flatly
  dark: darkly
---

## Introduction (Dataset-Info)

The dataset "ToothGrowth" already built into R was used for this assessment.

> The response is the length of odontoblasts (cells responsible for tooth growth) in 60 guinea pigs. Each animal received one of three dose levels of vitamin C (0.5, 1, and 2 mg/day) by one of two delivery methods, orange juice or ascorbic acid (a form of vitamin C and coded as VC). [C. I. Bliss (1952). The Statistics of Bioassay](https://doi.org/10.1016/C2013-0-12584-6)

From this description, one possible hypothesis could be:

\
*For equivalent doses, there is a difference in the length of odontoblasts depending on which supplement the guinea pigs receive.*

With the corresponding Null-Hypothesis that there is no difference in length of odontoblasts\

## Materials and Methods (Requirements in R(Studio)

The following packages were installed and loaded to work with the data.\
(Detailed information on used versions of R(Studio) and packages installed to reproduce the data are summarized in the "sessionInfo" at the end of this report.)

```{r}
#| warning: false
#| message: false
#| code-fold: false
library(tidyverse)
library(unibeCols)
library(usethis)
library(remotes)
library(gitcreds)
library(here)
library(medicaldata)
library(cowplot)
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(gtsummary)
library(rstatix)
```

-   Coding:
    -   len = Length of Odontoblasts
    -   supp = Supplement Type
        -   OJ = Orange Juice
        -   VC = Vitamin C
    -   dose = Dose used for application of supplement in mg/d

## Descriptive Statistics

The dataset "ToothGrowth" was examined using the functions "view" and "str"

```{r}
#| warning: false
#| message: false
data("ToothGrowth")
view(ToothGrowth)
str(ToothGrowth)
```

We can have a first look at means in different supplement groups and by different dosages using the function "summarize":

```{r}
#| warning: false
#| message: false
ToothGrowth %>%
  group_by(supp, dose) %>%
  summarize(mean(len))
```

### Summary Table

@fig-Tooth-Length shows means and standard deviations (SD) for the three doses of the supplement Orange Juice.

```{r}
#| label: fig-Tooth-Length
#| fig-cap: Mean Tooth-Length for supplement orange juice for doses 0.5, 1 and 2 mg/d

data_OJ <- ToothGrowth %>%
  filter(supp == "OJ")
  data_OJ %>%
  select(len, dose) %>%
  tbl_summary(
    by = dose, 
    label = list(len ~ "Tooth Length [mm]"),
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    digits = list(len ~ c(1, 1))) %>%
  add_overall()
```

### Boxplot

@fig-Boxplot shows the distribution of obtained values for Tooth-Length for the supplement orange juice. While there is a big difference in the median values for doses 0.5 and 1, median tooth-length for dose 2 is only slightly higher than for dose 1. One outlier with a length of over 30mm stands out for dose 3.

```{r}
#| label: fig-Boxplot
#| fig-cap: Distribution of Tooth-Length for supplement orange juice for doses 0.5, 1 and 2 mg/d

data_OJ <- data_OJ %>%
  mutate(dose_f = as.factor(as.character(dose)))

data_OJ %>%
  ggplot(aes(dose_f, len)) +
  geom_boxplot() +
  xlab(label = "Dose of supplement [mg/d]") +
  ylab(label = "Tooth-Length [mm]") +
  theme_bw() + theme(legend.position="bottom") 
```

## Analysis and Discussion

### Normality Testing

#### QQ-Plot

@fig-QQ-Plot shows a Quantile-Quantile (QQ)-Plot. Data appears to follow a normal distribution.

```{r}
#| label: fig-QQ-Plot
#| fig-cap: QQ-Plot for visual inspection of (Non)-Normality
ToothGrowth %>%
  ggplot(aes(sample = len)) + 
  geom_qq_line(distribution = stats::qnorm) +
  geom_qq(color = "steelblue", distribution = stats::qnorm) + 
  xlab("Theoretical Quantiles") +
  ylab("Tooth Length Quantiles") +
  theme_bw() +
  ggtitle(label = "QQ-Plot of Tooth Length")
```

#### Shapiro-Wilk Test

As sample size for this dataset is rather small (\< 50), a Shapiro-Wilk Test was performed in order to test\
if the data follows a normal distribution. Testing subsets of data individually and all tooth-length data, resulting p-values indicate that the Null-Hypothesis of Normality cannot be rejected. In other words, the data is normally distributed.

```{r}
#| warning: false
#| message: false

data_OJ_0.5 <- data_OJ %>%
  filter(dose == 0.5)
data_OJ_1 <- data_OJ %>%          
  filter(dose == 1)
data_OJ_2 <- data_OJ %>%          
  filter(dose == 2)
data_VC_0.5 <- data_OJ %>%
  filter(dose == 0.5)
data_VC_1 <- data_OJ %>%          
  filter(dose == 1)
data_VC_2 <- data_OJ %>%          
  filter(dose == 2)

shapiro.test(data_OJ_0.5$len)
shapiro.test(data_OJ_1$len)
shapiro.test(data_OJ_2$len)
shapiro.test(data_VC_0.5$len)
shapiro.test(data_VC_1$len)
shapiro.test(data_VC_2$len)

shapiro.test(ToothGrowth$len)
```

### Comparison of supplements (2-sample t-test)

As the two different supplement groups are independent and data is normally distributed, a 2-sample t-test was chosen to compare means of these groups. There is strong evidence for a difference in tooth length for doses 0.5 and 1. For these doses and most likely also for doses in between the two, tooth length was greater in groups supplemented with orange juice when compared to vitamin c. As for dose 2, there is no evidence of a difference in tooth length.

```{r}
#| warning: false
#| message: false

dose_0.5 <- ToothGrowth %>%
  filter(dose == 0.5)
dose_1 <- ToothGrowth %>%
  filter(dose == 1)
dose_2 <- ToothGrowth %>%
  filter(dose == 2)

dose_0.5 %>%
  t_test(len ~ supp)

dose_1 %>%
  t_test(len ~ supp)

dose_2 %>%
  t_test(len ~ supp) 
```

## Appendix - Session Info

```{r}
#| warning: false
#| message: false
sessionInfo()
```
