# Introducing dplyr and tidyr

####################################################################################
## Data Manipulation using dplyr and tidyr                                        ##
####################################################################################
# We're going to learn some of the most common `dplyr` functions:
# - `select()`: subset columns
# - `filter()`: subset rows on conditions
# - `mutate()`: create new columns by using information from other columns
# - `group_by()` and `summarize()`: create summary statistics on grouped data
# - `arrange()`: sort results
# - `count()`: count discrete values

# Set Up
library(tidyverse)
surveys <- read_csv("data_raw/portal_data_joined.csv")
surveys

####################################################################################
## Selecting columns and filtering rows                                           ##
####################################################################################
# To select columns of a data frame, use `select()`.
# The first argument is the dataframe and the subsequent are the columns to keep.
select(surveys, plot_id, species_id, weight)

# To select all columns except certain ones, put a “-” in front of the variable to exclude it.
select(surveys, -record_id, -species_id)

# To choose rows based on a specific criteria, use `filter()`:
filter(surveys, year == 1995)


####################################################################################
## Pipes                                                                          ##
####################################################################################
# What if you want to select and filter at the same time?
# There are three ways to do this: use intermediate steps, nested functions, or pipes.

# Intermediate steps
surveys2 <- filter(surveys, weight < 5)
surveys_sml <- select(surveys2, species_id, sex, weight)

# Nest functions (i.e. one function inside of another)
surveys_sml <- select(filter(surveys, weight < 5), species_id, sex, weight)

## Pipes
# - take the output of one function and send it directly to the next
# - `%>%`
# - require the `magrittr` package
# - you can type the pipe with 'Ctrl' + 'Shift' + 'M' ('Cmd' + 'Shift' + 'M' for Mac)

surveys %>%
  filter(weight < 5) %>%
  select(species_id, sex, weight)

# If we want to create a new object with this smaller version of the data, we
# can assign it a new name:

surveys_sml <- surveys %>%
  filter(weight < 5) %>%
  select(species_id, sex, weight)

surveys_sml


########## Exercise ########## 
# Using pipes, subset the surveys data to include animals collected before 1995 and 
# retain only the columns `year`, `sex`, and `weight`.

############################## 

####################################################################################
## Mutate                                                                         ##
####################################################################################
# Create new columns based on the values in existing columns

# We might be interested in the weight in kg:
surveys %>%
  mutate(weight_kg = weight / 1000)

# You can also create a second new column based on the first new column within the same call of mutate():
surveys %>%
  mutate(weight_kg = weight / 1000,
         weight_lb = weight_kg * 2.2)

# If this runs off your screen, you can use a pipe to view the head() of the data.
surveys %>%
  mutate(weight_kg = weight / 1000) %>%
  head()

# The first few rows of the output are full of NAs, 
# so if we wanted to remove those we could insert a `filter()` in the chain:
surveys %>%
  filter(!is.na(weight)) %>%
  mutate(weight_kg = weight / 1000) %>%
  head()

# The `!` symbol negates the result, so we're asking for every row where
# `weight` *is not* missing..

########## Exercise ########## 
# Create a new data frame from the surveys data that meets the following criteria: 
# contains only the `species_id` column AND 
# a new column called `hindfoot_half` containing values that are half the `hindfoot_length` values. 
# In this `hindfoot_half` column, there are *no NAs* and all values are *less than 30*.
#
#  **Hint**: think about how the commands should be ordered to produce this data
#  frame!

############################## 

####################################################################################
## Split-apply-combine data analysis and the summarize() function                 ##
####################################################################################

# Many data analysis tasks can be approached using the *split-apply-combine* paradigm:
# 1. split the data into groups
# 2. apply some analysis to each group
# 3. combine the results.

# `group_by()` is often used together with `summarize()`, which collapses each
# group into a single-row summary of that group.  `group_by()` takes as arguments
# the column names that contain the categorical variables for which you want
# to calculate the summary statistics.

# So to compute the mean weight by sex:
surveys %>%
  group_by(sex) %>%
  summarize(mean_weight = mean(weight, na.rm = TRUE))

# You can also group by multiple columns:
# We can exclude missing data from our table using a filter step.
surveys %>%
  filter(!is.na(weight)) %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight))

# If you want to display more data, you can use the print() function at the end of your chain
surveys %>%
  filter(!is.na(weight)) %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight)) %>%
  print(n = 15)

# You can also summarize multiple variables at the same time
surveys %>%
  filter(!is.na(weight)) %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight),
            min_weight = min(weight))

# You can rearrange the result of a query to inspect the values.
surveys %>%
  filter(!is.na(weight)) %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight),
            min_weight = min(weight)) %>%
  arrange(min_weight)

# To sort in descending order, add the `desc()` function.
surveys %>%
  filter(!is.na(weight)) %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight),
            min_weight = min(weight)) %>%
  arrange(desc(mean_weight))

####################################################################################
## Counting                                                                       ##
####################################################################################
# When working with data, we often want to know the number of observations found
# for each factor or combination of factors.
surveys %>%
  count(sex) 

# The `count()` function is shorthand for something we’ve already seen: 
# grouping by a variable, and summarizing it by counting the number of observations in that group. 
# In other words, `surveys %>% count()` is equivalent to:
surveys %>%
  group_by(sex) %>%
  summarise(count = n())

# We can count combination of factors:
surveys %>%
  count(sex, species) 

# `count()` provides the `sort` argument
surveys %>%
  count(sex, sort = TRUE) 

# With the above code, we can proceed with arrange() to sort the table according to 
# a number of criteria so that we have a better comparison. 
# For instance, we might want to arrange the table above in 
# (i) an alphabetical order of the levels of the species and 
# (ii) in descending order of the count:
surveys %>%
  count(sex, species) %>%
  arrange(species, desc(n))

########## Exercise ########## 
# 1. How many animals were caught in each `plot_type` surveyed?
# 2. Use `group_by()` and `summarize()` to find the mean, min, and max hindfoot length 
#    for each species (using `species_id`). Also add the number of observations (hint: see ?n).
# 3. What was the heaviest animal measured in each year? 
#    Return the columns `year`, `genus`, `species_id`, and `weight`.

############################## 

####################################################################################
## Exporting data                                                                 ##
####################################################################################
# Similar to the `read_csv()` function used for reading CSV files into R, there is
# a `write_csv()` function that generates CSV files from data frames.

# In preparation for our next lesson on plotting, we are going to prepare 
# a cleaned up version of the data set that doesn’t include any missing data.
surveys_complete <- surveys %>%
  filter(!is.na(weight),           # remove missing weight
         !is.na(hindfoot_length),  # remove missing hindfoot_length
         !is.na(sex))                # remove missing sex

# Because we are interested in plotting how species abundances have changed through time, 
# we are also going to remove observations for rare species. We will do this in two steps: 
# first we are going to create a data set that counts how often each species has been observed, 
# and filter out the rare species; then, we will extract only the observations for these more common species:
## Extract the most common species_id
species_counts <- surveys_complete %>%
  count(species_id) %>% 
  filter(n >= 50)

## Only keep the most common species
surveys_complete <- surveys_complete %>%
  filter(species_id %in% species_counts$species_id)

# Now we can save this data frame to our `data` directory.
write_csv(surveys_complete, path = "data/surveys_complete.csv")
