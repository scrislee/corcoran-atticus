#### Answers and code for ABD problems (chapter-problem number):12-18,
# 12-20, 12-33, 13-21, 13-30

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

# To perform sign tests, install and load the package DescTools
# install.packages("DescTools")
library("DescTools")

# To simplify summary statistics, install and load the package summarytools
install.packages("summarytools")
library("summarytools")

# For later plotting
install.packages("Hmisc")
library(Hmisc)

### Problem 12-18 ######
eyespan <- read_csv("datasets/abd/chapter12/chap12q18StalkieEyespan.csv")

# a.
# Do the data meet the assumption of normality?  Check with plots.
ggplot(eyespan) +
  geom_histogram(aes(eyeSpan), binwidth = .1)+
  facet_wrap(~food)
ggplot(eyespan) +
  geom_boxplot(aes(x = food, y = eyeSpan))
ggplot(eyespan)+
  geom_qq(aes(sample = eyeSpan, color = food))
# Yes, the data are symmetrical (top & bottom whisker equal for each box) 
# and mostly linear q-q plots.

# Do the data meet the assumption of homogeneous variance?  Check ratio.
(eyespan_ratio <- 0.0812/0.00558) 
# No, the groups do not have equal variance.  Use Welch's t-test.

# b.
t.test(eyeSpan ~ food, data = eyespan, alternative = "two.sided",
       conf.level = 0.95)
# Eye spans in male stalk-eyed flies raised on a corn diet were larger 
# than eye spans of flies raised on a cotton diet (Welch's t-test,
# two-sided: t = 8.35, df = 26.57, P < 0.0001).

### Problem 12-20 ######
# Data are paired, n = 12
# Is assumption of normality met?
fish <- read_csv("datasets/abd/chapter12/chap12q20ElectricFish.csv")
summ_fish<-descr(fish)
fish <- mutate(fish, diff = speciesUpstream - speciesDownstream)
# Because n = 12, just do boxplot and q-q
ggplot(fish) +
  geom_boxplot(aes(x = "", y = diff))
ggplot(fish)+
  geom_qq(aes(sample = diff))
# Maybe a bit of right skew when you look at location of median line 
# within the box, but not terrible skew when you consider the length of
# top and bottom whiskers. Proceed with parametric  paired t-test

# Research question was "whether presence of tributaries affected local
# number fish species" so do a two-sided test.
t.test(fish$speciesUpstream, fish$speciesDownstream, 
       alternative = "two.sided", paired = TRUE, conf.level = 0.95)

# a.
# Mean difference: Upstream of tributaries had on average 1.83 fewer 
# fish species than downstream of tributaries (95% CI of difference:
# -3.95<mu<-0.280).

# b.
# There was no effect of tributary presence on the species richness of 
# electric fish (t = -1.910, df = 11, p = 0.083).

# c.
# Assumed rivers sampled were randomly selected and independent
# from each other, and assumed differences were normally 
# distributed.

### Problem 12-33 ######
mice <- read_csv("datasets/abd/chapter12/chap12q33SpinocerebellarAtaxia.csv")
summ_mice <- stby(data = mice, INDICES = mice$treatment, FUN = descr)
# Want 95% CI
# Start by setting alpha
alpha <-0.05

# Convert summ_mice to a tibble and calculate CI using mutate
summ_mice <- summ_mice %>% 
  tb() %>%
  mutate(se = sd / sqrt(n.valid),
         lower_ci = mean - qnorm(1-alpha/2)*se,
         upper_ci = mean + qnorm(1-alpha/2)*se)

# a. 
# Answers will vary: dotplot, means +/- 95% CI, strip chart (what ABD calls it)

# b. 
#Nope.  Overlapping CIs do not necessarily mean no significant difference

# c.
# Start by checking assumptions
# Equal variances
mice_ratio <- summ_mice %>%
  summarise(max(sd)/min(sd))
# 1.97 is less than 3 so we are OK

# Normally distributed
ggplot(mice) +
  geom_boxplot(aes(x = treatment, y = lifespan))
ggplot(mice)+
  geom_qq(aes(sample = lifespan, color = treatment))
# Not terrific, but it is a small sample.  Given equal sample sizes, it is
# probably OK.  Proceed with two-sample, two-sided t-test with pooled variance.

t.test(lifespan ~ treatment, data = mice, var.equal = TRUE,
       alternative = "two.sided", conf.level = 0.95)
# Exercise increases lifespan in mice affected by spinocerebellar ataxia type I 
# (two-sample t-test, two-sided: t = 3.15, df = 10, p = 0.01).

# d.
# 95 % CI of the difference between the means is 12.52 < mu1 -mu2 < 72.81.
# The number of days by which life span is extended by exercise is from about
# 12 to 72

### Problem 13-21 ######
finch <- read_csv("datasets/abd/chapter13/chap13q21StressAndIncompatibleMates.csv")

# a.
# Add a column for differences and plot as histogram (n >15), boxplot, q-q plot
finch <- mutate(finch, diff = corticosterone_concentration_compatible - corticosterone_concentration_incompatible)
ggplot(finch) +
  geom_histogram(aes(diff), binwidth = 10)
# Histogram suggests negative skew (smaller corticosterone concentration in females
# that mate with compatible males)
ggplot(finch) +
  geom_boxplot(aes(x = "", y = diff))
# boxplot shows 3 outliers on lower end of distribution
ggplot(finch) +
  geom_qq(aes(sample = diff))
# q-q plot looks pretty bad

# b. Paired t-test would probably not be appropriate because of the skew.
# Note that the skew is negative because I chose to calculate diff as compatible minus
# incompatible.  The skew would be positive if I chose the opposite order!

# c. 
# Log transform data, but because you can only log tranform "-diff", which is fine
# in this particular case as we could have chosen diff = incompatible - compatible
finch <- mutate(finch, log10diff = log2(-diff))
# Still not OK because there is one observation that is NaN, or undefined

# d.
# A sign test is fine, although low power.  We assume that females are independent
# from each other and randomly sampled from the population.

# e.
# Because we are testing the null that compatible - incompatible = difference = 0, use 
# a 2-sided test.
# mu is equal to zero, per the null above.
SignTest(finch$diff, 
         alternative = "two.sided", mu = 0, conf.level = 0.95)
# When mated with "compatible" males of the same genetic type, female Gouldian 
# finches have lower blood corticosterone concentrations (sign test, two-sided: S = 1,
# n = 43, p < 0.0001).



### Problem 13-30 ######
dengue <- read_csv("datasets/abd/chapter13/chap13q30DengueMosquiteTiter.csv")

# a.  
# Strip chart is a total ABD term.  Basically, it shows mean +/- 95% CI and then
# the raw data.  You can create this with ggplot2 using:
ggplot(dengue, aes(x = strain, y = logTiter)) +
  geom_point() +
  stat_summary(fun.data = "mean_cl_boot")


# b.
# To rank data, we use the function dplyr::arrange()
dengue_rank <- arrange (dengue, logTiter)

# c.
# Because testing hypothesis that strains differ, use two-sided Mann-Whitney
wilcox <- wilcox.test(logTiter ~ strain, data = dengue, alternative = "two.sided", conf.level = 0.95)
# Get error message that we cannot compute exact p when there are ties.  But that is OK.
# Approximate p is fine for us.
wilcox

# d. 
# Because some of the original numbers were zero

# e. 
# No because taking logs does not change the ranks