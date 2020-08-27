### Lab 5. t-tests and friends

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

# To perform sign tests, install and load the package DescTools
install.packages("DescTools")
library("DescTools")

# Read in data file, generic version
#<name-you-assign><-read_csv("path-to-file", col_names = TRUE)

### One sample t-test #########################
# Option A
# The function pt() calculates the probability of t less than or equal to a sample value.  Note that this is 
# annoyingly the opposite of what a t-table does.  C'est la vie.

# First step, calculate t_sample.  You will need to define what the sample mean, null hypothesis mean, sample 
# standard deviation, and sample size are.  
null_mean <- 0

# If you are given the values for the sample mean, sd, and n, you can simply define each value as an object 
# in the environment
sample_mean <- 39.3
sample_sd <- 30.7
sample_n <- 31
df <- sample_n -1

# If you are given raw data, read in the data file and define each summary statistic with a simple equation
# Note: you can't use summarise here because it will create a table instead of named objects.

# Read in data
range_shift <- read_csv("datasets/abd/chapter11/chap11q01RangeShiftsWithClimateChange.csv")

# Identify your response variable using the form dataset$variable_name
y<-range_shift$elevationalRangeShift

# Calculate summary statistics
sample_mean <-mean(y)
sample_sd <- sd(y)
sample_n <- as.numeric(length(y))
df <- sample_n -1

# Whether you are given the values for mean/sd/n or calculate them, your next step is calculating t_sample
t_sample <- (sample_mean - null_mean)/(sample_sd/sqrt(sample_n))

# The value I call "negative tail" is the exact probability of obtaining t less than or equal to your t-sample
# If you are testing an alternate hypotheses of "sample mean is less than a certain number" then this is your
# p-value
negative_tail <- pt(t_sample, df)

# If you are testing an alternate hypothesis of "sample mean is greater than a certain number" then you have
# to calculate 1 - negative_tail.
positive_tail <- 1 - negative_tail

# For a two-sided test, the exact probability of obtaining t equal to t_sample or more extreme is calculated
# as:
two_tailed <- 2*(1-pt(abs(t_sample), df))

# Option B
# One-sample t-test can be calculate using t.test. 
# The mu argument gives the value stated in the null hypothesis.

# The code below ASSUMES that you have read in the data file
# Now you have to specify which dataset the values are coming from using the form dataset$variable_name.

# Two-sided
t.test(range_shift$elevationalRangeShift, 
       alternative = "two.sided", mu = 0, conf.level = 0.95)

# One-sided, HA that sample mean is greater than null mean
t.test(range_shift$elevationalRangeShift, 
       alternative = "greater", mu = 0, conf.level = 0.95)

# One-sided, HA that sample mean is less than null mean
t.test(range_shift$elevationalRangeShift, 
       alternative = "less", mu = 0, conf.level = 0.95)




### Paired t-test #########################
# Start with a dataset in untidy format (groups not defined by a categorical variable, two observations (or 
# more) in each row.  Later you will use this untidy dataset to perform the statistical test.
# These data come from Chapter 12 in your book.
untidy_blackbird <- read_csv("datasets/abd/chapter12/chap12e2BlackbirdTestosterone.csv")

# Begin by exploring the data with histograms, boxplots, and q-q plots
# Since the assumptions of normality apply to differences, use mutate() to add a column called diff.
# Note that here diff = After - Before

untidy_blackbird <- mutate(untidy_blackbird, diff = afterImplant - beforeImplant)

ggplot(untidy_blackbird) +
  geom_histogram(aes(diff), binwidth = 10)

ggplot(untidy_blackbird) +
  geom_boxplot(aes(x = "", y = diff))

ggplot(untidy_blackbird)+
  geom_qq(aes(sample = diff))

# Now perform the statistical test.  The boxplot wasn't horrible, and the sample size is 31, so you could
# justify not transforming.  Then again, the Q-Q plot is not so great, so you could also justify transforming.
# What is important to me, is that you justify your choice.

# There are (at least) two methods for paired t-tests.  The first is a one sample t-test on the differences, 
# using the function pt().
# The second uses the function t.test().  Unlike using t.test() for a one sample t-test, a two sample t-test
# specifies each group (i.e., before and after), does not take the argument mu = , and takes the argument 
# paired = TRUE.
# Note that the confidence intervals are for the mean difference.

# Two-sided
t.test(untidy_blackbird$afterImplant, untidy_blackbird$beforeImplant, 
       alternative = "two.sided", paired = TRUE, conf.level = 0.95)

# One-sided, HA that afterImplant is greater than beforeImplant
t.test(untidy_blackbird$afterImplant, untidy_blackbird$beforeImplant, 
       alternative = "greater", paired =  TRUE, conf.level = 0.95)

# One-sided, HA that afterImplant is less than beforeImplant
t.test(untidy_blackbird$afterImplant, untidy_blackbird$beforeImplant, 
       alternative = "less", paired =  TRUE, conf.level = 0.95)

# The most straight-forward way to show var.equal data is to connect each pair with a line.  To do this, you
# first have to make data tidy (each variable has its own column, one observation in each row).

# Generic code to transform untidy data to tidy data
# <new_name> <- <untidy_dataset_name> %>% 
# gather(<one_group>, <other_group>, key = "<heading_for_grouping_variable>", value = "<heading_for_response>")

tidy_blackbird <- untidy_blackbird %>%
  gather(beforeImplant, afterImplant, key="treatment", value = "antibody")

ggplot(tidy_blackbird, aes(x=treatment, y=antibody, group=blackbird)) +
  geom_point(aes(colour=treatment), size=4.5) +
  geom_line(size=1, alpha=0.5) +
  xlab('Testosterone Treatment') +
  ylab('Antibody Production (mOD/min)') +
  scale_colour_manual(values=c("#009E73", "#D55E00"), guide=FALSE) + 
  theme_bw()

### Non-parametric Sign Test #########################

# Although not necessary in either case, it is instructive to perform a sign test on the range_shift and 
# untidy_blackbird data.

# One-sample, Two-sided
SignTest(range_shift$elevationalRangeShift, 
       alternative = "two.sided", mu = 0, conf.level = 0.95)

# One-sample, One-sided, HA that sample mean is greater than null mean
SignTest(range_shift$elevationalRangeShift, 
       alternative = "greater", mu = 0, conf.level = 0.95)

# One-sample, One-sided, HA that sample mean is less than null mean
SignTest(range_shift$elevationalRangeShift, 
       alternative = "less", mu = 0, conf.level = 0.95)

# NOTE, for paired you need to specify the difference variable (in this case diff)
# Two-sided
SignTest(untidy_blackbird$diff, alternative = "two.sided", mu = 0, conf.level = 0.95)

# One-sided, HA that afterImplant is greater than beforeImplant
SignTest(untidy_blackbird$diff, alternative = "greater", mu = 0, conf.level = 0.95)

# One-sided, HA that afterImplant is less than beforeImplant
SignTest(untidy_blackbird$diff, alternative = "less", mu = 0, conf.level = 0.95)

### Two sample t-test #########################

# Pooled variances
# Read in the Ward & Quinn dataset looking at the egg production of predatory snails
ward <- read_csv("datasets/quinn/chpt3/ward.csv")

# Look at the summary statistics
summ_eggs <- ward %>%
  group_by(ZONE) %>% 
  summarise(mean_eggs = mean(EGGS),
            sd_eggs = sd(EGGS),
            n_eggs = n())

# Calculate the ratio between the standard deviations as a loose test of homoscedasticity
ratio <-(max(summ_eggs$sd_eggs))/(min(summ_eggs$sd_eggs))

# Look at histograms, box plots, q-q plots
ggplot(ward) +
  geom_histogram(aes(EGGS), binwidth = 2)+
  facet_wrap(~ZONE)

ggplot(ward) +
  geom_boxplot(aes(x = ZONE, y = EGGS))

ggplot(ward)+
  geom_qq(aes(sample = EGGS, color = ZONE))

# A little right skew indicated in both histogram and but nothing terrible.

# For the two-sample t-test with pooled variance, there are additional arguments.  You need to give the 
# formula (response ~ predictor), identify the data, include var.equal = TRUE.

# Two-sided
t.test(EGGS ~ ZONE, data = ward, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

# NOTE: Group 1 and Group 2 are ordered alphabetically unless you specify otherwise
# In the output of the t-test, the first mean under "sample estimates" is group 1, the second is group 2
# One-sided, HA that Littor is greater than Mussel
t.test(EGGS ~ ZONE, data = ward, var.equal = TRUE, alternative = "greater", conf.level = 0.95)

# One-sided, HA that Littor is less than Mussel
t.test(EGGS ~ ZONE, data = ward, var.equal = TRUE, alternative = "less", conf.level = 0.95)

## Welch's t-test #########################

# Read in the Levin et al dataset from Chapter 12 of your book.  
salmon <- read_csv("datasets/abd/chapter12/chap12e4ChinookWithBrookTrout.csv")

# Suppose we are interested in potential differences in the proportion of surviving native chinook salmon
# in the presence and absence of invasive brook trout.
# Examine the ratio of the variances
summ_surv <- salmon %>%
  group_by(troutTreatment) %>% 
  summarise(mean_surv = mean(proportionSurvived),
            sd_surv = sd(proportionSurvived),
            n_surv = n())

# Calculate the ratio between the standard deviations as a loose test of homoscedasticity
ratio <-(max(summ_surv$sd_surv))/(min(summ_surv$sd_surv))

# Examine plots for evidence of non-normality.  

#Histogram is pretty worthless because n is so small.
ggplot(salmon) +
  geom_histogram(aes(proportionSurvived), binwidth = 0.05)+
  facet_wrap(~troutTreatment)

ggplot(salmon) +
  geom_boxplot(aes(x = troutTreatment, y = proportionSurvived))

ggplot(salmon)+
  geom_qq(aes(sample = proportionSurvived, color = troutTreatment))

# Go forward assuming that normality has been met but homogeneity of variances has not.
# To perform Welch's t-test, all you need to do is remove the argument var.equal = TRUE

# Two-sided
t.test(proportionSurvived ~ troutTreatment, data = salmon, alternative = "two.sided", conf.level = 0.95)


# One-sided, HA that absent is greater than present
t.test(proportionSurvived ~ troutTreatment, data = salmon, alternative = "greater", conf.level = 0.95)

# One-sided, HA that absent is less than present
t.test(proportionSurvived ~ troutTreatment, data = salmon, alternative = "less", conf.level = 0.95)

### Non-parametric Mann-Whitney U Test #########################

# For this we are going to return to the cannibal crickets from Exam 1 Extra Credit
cricket <- read_csv("datasets/abd/chapter13/chap13e5SagebrushCrickets.csv")

ggplot(cricket) +
  geom_histogram(aes(timeToMating), binwidth = 10)+
  facet_wrap(~feedingStatus)

ggplot(cricket) +
  geom_boxplot(aes(x = feedingStatus, y = timeToMating))

ggplot(cricket)+
  geom_qq(aes(sample = timeToMating, color = feedingStatus))

# The fed group is pretty non-normal

# The Mann-Whitney U Test is equivalent to the Wilcoxon rank-sum test.  Similar to our 2-sample t-test 
# examples, we give a formula in the form y ~ x or response ~ predictor.

# Two-sided
wilcox.test(timeToMating ~ feedingStatus, data = cricket, alternative = "two.sided", conf.level = 0.95)

# One-sided, HA that fed greater than starved
wilcox.test(timeToMating ~ feedingStatus, data = cricket, alternative = "greater", conf.level = 0.95)

# One-sided, HA that fed less than starved
wilcox.test(timeToMating ~ feedingStatus, data = cricket, alternative = "less", conf.level = 0.95)


