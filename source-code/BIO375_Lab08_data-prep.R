# Cleaning some Tilman data for Lab 8

# Package ID: knb-lter-cdr.273.9 Cataloging System:https://pasta.edirepository.org.
# Data set title: Plant aboveground biomass data: Biodiversity II:  Effects of Plant Biodiversity on Population and Ecosystem Processes.
# Data set creator:  David Tilman -  
# Metadata Provider:    - Cedar Creek LTER 
# Contact:  Dan Bahauddin - Information Manager Cedar Creek Ecosystem Science Reserve  - webmaster@cedarcreek.umn.edu
# Stylesheet v1.3 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@Virginia.edu 
#
#install package tidyverse if not already installed
if(!require(tidyverse)){ install.packages("tidyverse") }  
library("tidyverse") 

bdII_abovegr_biomass <- read_csv("datasets/demos/bdII_abovegr-biomass.csv", 
                                 col_types = cols(Date = col_skip(), Note = col_skip(), 
                                                   Plot = col_character(), 
                                                  Substrip = col_skip()))

bdef <- bdII_abovegr_biomass %>%
  rename(biomass = "Biomass (g/m2)") %>%
  filter(Year == 2000) %>%
  select(Plot, NumSp, biomass) %>%
  group_by(NumSp, Plot) %>%
  summarise(sum(biomass)) %>%
  filter(NumSp != 0) %>%
  rename(biomass = "sum(biomass)")

write_csv(bdef, "datasets/demos/bdef.csv")



# Install package ggfortify, *note* only do install.packages ONCE
# ggfortify is a package that works with ggplot2 to make nice plots
# install.packages("ggfortify")
library("ggfortify")
# multcomp is used for contrasts and multiple comparisons
# install.packages("multcomp")
library("multcomp")
# nlme is used for random effects ANOVA
# install.packages("nlme")
library("nlme")

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()


### Multiple Comparisons for complex designs ####
# The code I provided you for Lab 7 showed you how to compare specific pairs of treatments
# Recall we used multcomp::glht in an example where the predictor variable is a factor called
# parasite and Metschnikowia, Pansporella, Pasteuria, and control are the 4 levels of the
# factor.  We wanted to know, is each parasite different than the no parasite control?

# planned <- glht(model01, linfct = 
#                   mcp(parasite = c("Metschnikowia - control = 0",
#                                    "Pansporella - control = 0",
#                                    "Pasteuria - control = 0")))


# In some cases you might want to compare particular *groups* of treatments to each other. For example, Biodiversity II is a long-term experiment designed to determine how 
# the number of plant species affects the dynamics of ecological processes. Researchers 
# experimentally manipulated the number of plant species in 9m x 9m plots, 
# and measured many response variables including carbon and nitrogen content of the plants.

# Plots were seeded in May 1994 to have 1, 2, 4, 8, or 16 species, with roughly 30 replicates
# of each diversity level. We will look at data from the final year of the study and perform
# a planned contrast to test whether 1 species plots are different from 2, 4, 8, and 16 
# species plots, taken together.  In other words 1 spp vs the average of 2,4,8,16 spp.

# We do this by defining a contrast matrix according to the following rules:
#   1.  groups to be included and excluded in a specific contrast are represented by non-zero
#       and zero coefficients respectively
#   2.  groups contrasted to one another have opposing signs (positive or negative)
#   3.  the number of contrasts cannot exceed p-1
#   4.  within a given contrast, the sum of positive coefficients should sum to 1 and the
#       negative coefficients should sum to -1

# Read in the Cedar Creek data
BDEF <- read_csv("datasets/demos/BDEF.csv", 
                 col_types = cols(NumSp = col_factor(levels = c("1", 
                                                                "2", "4", "8", "16"))))

# Start by performing an ANOVA to test whether mean aboveground biomass, biomass, is 
# different among species diversity treatments, NumSp

BDEF_model01 <- lm(biomass ~ NumSp, data = BDEF)
autoplot(BDEF_model01)
summary.aov(BDEF_model01)

# Now we do the annoying work of specifying comparisons.  
# Look at the levels of our factor, NumSp, paying special attention to the order
levels(BDEF$NumSp)

# Create a contrast matrix for comparing 1 spp group to the average of groups 2, 4, 8, 16
# What you are doing is creating a datatable that has one row with a column for each
# factor level in order (i.e., first column for 1 spp, second column for 2 spp, third for 4 spp)
# Tell the computer that the first group is just 1 spp by giving it the value of positive 1
# Tell the group that you are comparing with a second group that includes all of the other
# factor levels (2, 4, 8, 16).  Must be negative and add to -1.  See below.
contrasts(BDEF$NumSp) <- cbind(c(1, -0.25, -0.25, -0.25, -0.25))
contrasts(BDEF$NumSp)

# You see the treatment levels in the first column, the contrast coefficients in the second, 
# and then a bunch of weird stuff.  R fills in the weird stuff because there are 
# *technically* supposed to be exactly p-1 planned contrasts.  Whatever.

BDEF_model01 <- lm(biomass ~ NumSp, BDEF)
summary(BDEF_model01)
### Multiple Comparisons if package multcomp fails ####
# (looking at you NS)
# First define the anova model using the function aov()

model01_b <- aov(growth.rate ~ parasite, daphnia)
# Then use the function(TukeyHSD)
TukeyHSD(model01_b)


