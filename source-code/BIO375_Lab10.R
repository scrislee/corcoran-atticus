#### Lab 10: Chi-squared and friends #### 

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# install.packages("ggmosaic")
library("ggmosaic")

# install.packages("epitools")
library("epitools")

# install.packages("tibble")
library("tibble")

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

### Exact Binomial test ####

# The simplest way to execute a binomial test is to use the function 
# binom.test()
# This function takes 5 different arguments:
# x: the number of observed successes
# n: the number of trials
# p: hypothesized probability of success *Needs to be a number between 0 and 1*
# alternative: the alternative hypothesis (takes "two.sided", "less", or "greater")
# conf.level: confidence level for the CI returned in analysis

# You don't even need to read in a file to do this!
# Using the willow beetle data in your handout, you can test the (alternate) hypothesis that the frequency of females in
# the  population is greater than 0.5 (the null proportion).  Note that here I am calling "female" the success.

# For the Kevo beetle population 
# I don't trust my mental math these days, so I get R to do arithmetic for me.
28+18 # n = total number of trials
model01 <- binom.test(x= 28, n=46, p=0.5, alternative = "greater", conf.level = 0.95 )
model01

# For the wilderness population
28+13
model02 <- binom.test(x= 28, n=41, p=0.5, alternative = "greater", conf.level = 0.95 )
model02

# So we can conclude that females occured more frequently than expected in the wilderness population in Alaska 
# (binomial test: P < 0.05, n=41) but not in the Kevo population (binomial test: P > 0.05 n=46).

### Chi-squared goodness of fit ####

# Unlike the exact binomial test, chi-sq goodness of fit is often quicker to do by hand than in R.  Nevertheless...

# We will start with a non-biological example from your book, because it is useful to see how the data are
# structured

birth <- read_csv("datasets/abd/chapter08/chap08e1DayOfBirth.csv", 
                  col_types = cols(day = col_factor()))

# This file has a single categorical variable, day, that has 7 levels

# You are interested in testing whether the probability of birth is the same on every day of the week (i.e., 1 in 7)
# Because a year of 365 days does not, in fact, have the exact same number of M Tu W Th F Sa and Su, we start by 
# generating the null expectations for number of births on each day of the week, by calculating, for example, the
# number of Mondays in 1999 divided by the total number of days in 1999.

# The data file has the actual observed data.  We want to know the number of times each day occurs in the database.
# Use summarise to count the number of occurences for each day.
birth_summ <- birth %>%
  group_by(day)%>%
  summarise(day_n = n())


# Add two columns, expected and expected_p onto the summary table with the function add_column()
birth_summ <- add_column(birth_summ, expected= c(52,52,52,52,52,53,52)) %>%
  mutate(expected_p = expected/365)

# All the expected values are greater than 5 so we meet the assumptions of the chi-sq goodness of fit test

# Now use the function chisq.test()
# x = observed counts of each level of your categorical variable
# p = expected probabilities for each level of your categorical variable, must be number between 0 and 1
# x = birth_summ$day_n
# p = birth_summ$expected_p

model03 <-chisq.test(x = birth_summ$day_n, p = birth_summ$expected_p)
model03

### Chi-squared contingency analysis ####

# Contingency analyses are appropriate when you are testing for an association between two or more categorical variables
# Plotting 2 x 2 table data requires ggmosaic, which you installed earlier.

# Example 9.2 from your book asks the question: is there a (negative) association between taking aspirin and getting 
# cancer?  

cancer <- read_csv("datasets/abd/chapter09/chap09e2AspirinCancer.csv")
ggplot(data = cancer) +
  geom_mosaic(aes(x = product(cancer, aspirinTreatment), fill=cancer), na.rm=TRUE)

# First we need to create a 2 x 2 contingency table using the function table() 
cancerTable <- table(cancer$cancer, cancer$aspirinTreatment)
cancerTable

# If dealing with epidemiology, it is important that the first variable listed defines the diseased/undiseased, and the
# second variable listed defines treatment/control groups

# In epidemiology it is common to calculate the odds ratio, which is the ratio of odds of success (getting the disease) 
# between two groups.  For example the odds ratio we will calculate is the odds of getting cancer between women taking
# placebos and women taking aspirin.

# We will use the function oddsratio() from the package epitools
oddsratio(cancerTable, method = "wald")

# $data gives the full contingency table with row and column totals
# $measure gives the odds ratio.  We can conclude that the odds of getting cancer are ever so slightly higher for
# people taking aspirin.

# $p.value gives three p.values, for similar types of statistical test.  Chi-square (the third option) is fine if you
# meet the assumptions (less than or equal to 20% of cells have expected counts less than 5).
# If you have a 2 x 2 table and your expected values are too small to meet the assumptions, then fisher.exact 
# (the second option) is more appropo.

# In order to report your results, you need the sample test statistic and degrees of freedom.  Put the putative disease 
# response variable first, then the treatment/control grouping variable.  The correct = FALSE argument tells R not to 
# perform the Yates correction.  
model04 <- chisq.test(cancer$cancer, cancer$aspirinTreatment, correct = FALSE)
model04

# Report like this: We found that the probability of getting cancer was not different in the aspirin and placebo groups
# (Chi-square test contingency test: Chisq = 0.050, df = 1, p = 0.8224 ).

# Sometimes it is easier just to create a table of counts, number of rows x number of columns
# Then you define the names: row names by column names

tab01 <- matrix(c(1438, 1427, 18496, 18515), 2, 2, byrow=TRUE)
# Add row names, then column names with the function dimnames()
dimnames(tab01) <- list("Outcome" = c("Cancer", "No Cancer"),
                      "Treatment" = c("Aspirin", "No Aspirin"))
as.matrix(tab01)
model05 <- chisq.test(tab01, correct = FALSE)
model05$expected



### Fisher's exact test (2x2) ####

# Use this test when expected cell counts are too low to meet assumptions of Chi-squared

# The book's example for this is vampire bats. Read the scenario on pg.252.
# Using the counts on pg. 253, we can create a table with 2 rows and 2 columns

tab02 <- matrix(c(15,6,7,322), 2, 2, byrow=TRUE)
dimnames(tab02) <- list("Outcome" = c("Bitten by vampire bat", "Not bitten by vampire bat"),
                        "Estrus status" = c("Cows in estrus", "Cows not in estrus"))
as.matrix(tab02)
model05 <- chisq.test(tab02, correct = FALSE)
# R complains because of the violation of assumptions. Just in case you hadn't noticed.
# You receive a warning message "In chisq.test(tab02, correct = FALSE) :
#                                   Chi-squared approximation may be incorrect

model06 <- fisher.test(tab02, alternative = "two.sided")
model06

# If you start with counts in a contingency table, you need to create longform data to graph it
tab03<- tab02 %>%
  as.data.frame() %>%
  as_tibble() %>%
  rownames_to_column("rname")
  
  
  
  




