# Starting with Data

####################################################################################
## Setting Up                                                                     ##
####################################################################################

dir.create("data")
dir.create("data_raw")
dir.create("fig_output")
download.file(url="https://ndownloader.figshare.com/files/2292169",
              destfile = "data_raw/portal_data_joined.csv")


# You are going load the data in R's memory using the function `read_csv()`
# from the `readr` package which is part of the **`tidyverse`**. 
# So, before we can use the `read_csv()` function, we need to load the package. 
# The missing data is encoded as "NULL" in the dataset. 

library(tidyverse)
surveys <- read.csv("data_raw/portal_data_joined.csv")

####################################################################################
## Quick Recap                                                                   ##
####################################################################################
## Creating Objects:
# To create an object, we need to give it a name followed by the
# assignment operator `<-`, and the value we want to give it.
## Functions:
# - Functions are "canned scripts"
# - Predefined, or can be made available by importing R *packages*
# - A function usually gets one or more inputs called *arguments*
# - Can return a single value, and also a set of things, or even a dataset 
## Vector
# - composed by a series of values, can be either numbers or characters. 
# - can be assigned using the `c()` function.
## Dataframes
# - the representation of data in the format of a table
# - columns are vectors that all have the same length
# - each column must contain a single type of data
## Subsetting
# If we want to extract one or several values from a vector or dataframe, we must provide one
# or several indices in square brackets `[` and `]`.
# For vector, supply the index: vector[1]
# For dataframe, supply the row and column: surveys[1,1]

####################################################################################
## Factors                                                                        ##
####################################################################################

## Factors:
# - represent categorical data
# - stored as integers associated with labels 
# - can be ordered or unordered. 
# - look like character vectors, but actually treated as integer vectors

# Once created, factors can only contain a pre-defined set of values, known as
# *levels*. By default, R always sorts levels in alphabetical order. For
# instance, if you have a factor with 2 levels:
sex <- factor(c("male", "female", "female", "male"))
sex

# R will assign `1` to the level `"female"` and `2` to the level `"male"`
# (because `f` comes before `m`, even though the first element in this vector is
# `"male"`). You can see this by using the function `levels()` and you can find
# the number of levels using `nlevels()`:

levels(sex)
nlevels(sex)

# Reordering
sex
sex <- factor(sex, levels = c("male", "female"))
sex # after re-ordering

# Converting a factor to a character vector
as.character(sex)

# Converting factors where the levels appear as numbers to a numeric vector
# It's a little trickier!
# The `as.numeric()` function returns the index values of the factor, not its levels

year_fct <- factor(c(1990, 1983, 1977, 1998, 1990))
as.numeric(year_fct)                     # Wrong! And there is no warning...
as.numeric(as.character(year_fct))       # Works...

####################################################################################
## Renaming factors                                                               ##
####################################################################################
# When your data is stored as a factor, you can use the `plot()` function to get a
# quick glance at the number of observations represented by each factor level:

# bar plot of the number of females and males captured during the experiment:
plot(surveys$sex)

# pull out the data on sex
sex <- surveys$sex

# There are 3 levels
levels(sex)

# replace the first one with "undetermined"
levels(sex)[1] <- "undetermined"
head(sex)

plot(sex)

########## Exercise ########## 
# * Rename “F” and “M” to “female” and “male” respectively.
# * Now that we have renamed the factor level to “undetermined”, 
#   can you recreate the barplot such that “undetermined” is last (after “male”)?

##############################
