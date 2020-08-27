### Lab 2. Brief Introduction to RStudio

# Before anything else, verify that your environment is totally clear.
# This is important, because old objects can foul up the works
# Clean up the working environment
rm(list = ls())

# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# At the beginning of a script, it is useful to make sure you have
# downloaded and installed all necessary packages

install.packages("tidyverse") 
# Note that this loads the following packages: ggplot2 purrr 
#   tibble dplyr tidyr stringr readr forcats
library("tidyverse")
# Check for updates
tidyverse_update()

# Create an object called x
x <- 3*4

# View x in the Console
x

# Read in the data file
data<-read_csv("datasets/demos/2019-09-02_class-data.csv",col_names = TRUE,
               col_types = cols(
  name = col_character() )
)

# Generate summary statistics for the object called "data" that consists of
# 12 observations of 4 variables
summary(data)

# The function summary{base} does not provide us with the standard deviation

# We can calculate variance and standard deviation using the function
# summarise{dplyr}
summarise(data, sd_ht = sd(height), sd_arm = sd(right_arm))

# Plot height as a histogram
ggplot(data)+
  geom_histogram(aes(height), binwidth = 10)

# Plot right arm length as a histogram
ggplot(data)+
  geom_histogram(aes(right_arm), binwidth = 5)

# Note that you must have the column name EXACTLY correct.  Try, for example,
# the following code
ggplot(data)+
  geom_histogram(aes(right arm), binwidth = 10)

# Look at the help file for geom_boxpolot
help("geom_boxplot")

# Plot height as a boxplot
ggplot(data)+
  geom_boxplot(aes(x = "", y = height), notch = TRUE, varwidth = TRUE)

# Plot height as a three boxplots, one for each hair color
ggplot(data)+
  geom_boxplot(aes(x = hair_color, y = height), varwidth = TRUE)

### Assignment

# Load a larger dataset, lovett.csv, and assign the name data2.  
# IT IS IMPORTANT THAT YOU ASSIGN THE NAME data2
# Upload dataset found at datasets/quinn/chpt2/lovett.csv
# Enter your code below

data2 <- read_csv("datasets/quinn/chpt2/lovett.csv")

# Calculate summary statistics for SO4 and SO4MOD
# Enter your code below



# Calculate standard deviation for SO4 and SO4MOD
# Enter your code below



# Plot histograms of SO4 and Modified SO4 with appropriate bin sizes
# Enter your code below



# Plot boxplots of SO4 and Modified SO4 using the code below.  
# You do not need to write any new code for this part!

# The code below modifies the dataset so it only contains SO4 and Modified SO4
# using select{dplyr}, and is oriented in long form using gather{tidyr}
data3 <- data2 %>%
  select(contains("SO4"))%>%
  gather(key = "type", value = "measurement", SO4, SO4MOD)

# The code below plots the two variables as boxplots, zooming in on the
# 40-75 range where most of the values are found (coord_cartesian).  The red 
# dots indicate the means (stat_summary).
ggplot(data = data3)+
  geom_boxplot(aes(x = type, y = measurement), notch = TRUE)+
  coord_cartesian(ylim = c(40, 75))+
  stat_summary(aes(x = type, y = measurement), fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3)




