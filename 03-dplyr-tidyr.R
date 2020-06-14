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
interviews <- read_csv("data/SAFI_clean.csv", na = "NULL")
interviews

####################################################################################
## Selecting columns and filtering rows                                           ##
####################################################################################
# To select columns of a data frame, use `select()`.
# The first argument is the dataframe and the subsequent are the columns to keep.
select(interviews, village, no_membrs, years_liv)

# To choose rows based on a specific criteria, use `filter()`:
filter(interviews, village == "God")


####################################################################################
## Pipes                                                                          ##
####################################################################################
# What if you want to select and filter at the same time?
# There are three ways to do this: use intermediate steps, nested functions, or pipes.

# Intermediate steps
interviews2 <- filter(interviews, village == "God")
interviews_god <- select(interviews2, no_membrs, years_liv)

# Nest functions (i.e. one function inside of another)
interviews_god <- select(filter(interviews, village == "God"), no_membrs, years_liv)

## Pipes
# - take the output of one function and send it directly to the next
# - `%>%`
# - require the `magrittr` package
# - you can type the pipe with 'Ctrl' + 'Shift' + 'M' ('Cmd' + 'Shift' + 'M' for Mac)

interviews %>%
  filter(village == "God") %>%
  select(no_membrs, years_liv)

# If we want to create a new object with this smaller version of the data, we
# can assign it a new name:

interviews_god <- interviews %>%
  filter(village == "God") %>%
  select(no_membrs, years_liv)

interviews_god


########## Exercise ########## 
# Using pipes, subset the `interviews` data to include interviews
# where respondents were members of an irrigation association (`memb_assoc`)
# and retain only the columns `affect_conflicts`, `liv_count`, and `no_meals`.

############################## 

####################################################################################
## Mutate                                                                         ##
####################################################################################
# Create new columns based on the values in existing columns

# We might be interested in the ratio of number of household members
# to rooms used for sleeping (i.e. avg number of people per room):

interviews %>%
  mutate(people_per_room = no_membrs / rooms)

# We may be interested in investigating whether being a member of an
# irrigation association had any effect on the ratio of household members
# to rooms. We have to remove data from our dataset where the respondent didn't 
# answer the question of whether they were a member of an irrigation association.
# To remove these cases, we could insert a `filter()` in the chain:

interviews %>%
  filter(!is.na(memb_assoc)) %>%
  mutate(people_per_room = no_membrs / rooms)

# The `!` symbol negates the result, so we're asking for every row where
# `memb_assoc` *is not* missing..

########## Exercise ########## 
#  Create a new data frame from the `interviews` data that meets the following
#  criteria: contains only the `village` column and a new column called
#  `total_meals` containing a value that is equal to the total number of meals
#  served in the household per day on average (`no_membrs` times `no_meals`).
#  Only the rows where `total_meals` is greater than 20 should be shown in the
#  final data frame.
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

#So to compute the average household size by village:
interviews %>%
  group_by(village) %>%
  summarize(mean_no_membrs = mean(no_membrs))

# You can also group by multiple columns:
interviews %>%
  group_by(village, memb_assoc) %>%
  summarize(mean_no_membrs = mean(no_membrs))

# We can exclude missing data from our table using a filter step.
interviews %>%
  filter(!is.na(memb_assoc)) %>%
  group_by(village, memb_assoc) %>%
  summarize(mean_no_membrs = mean(no_membrs))

# You can also summarize multiple variables at the same time
interviews %>%
  filter(!is.na(memb_assoc)) %>%
  group_by(village, memb_assoc) %>%
  summarize(mean_no_membrs = mean(no_membrs),
            min_membrs = min(no_membrs))

# You can rearrange the result of a query to inspect the values.
interviews %>%
  filter(!is.na(memb_assoc)) %>%
  group_by(village, memb_assoc) %>%
  summarize(mean_no_membrs = mean(no_membrs), min_membrs = min(no_membrs)) %>%
  arrange(min_membrs)

# To sort in descending order, add the `desc()` function.
interviews %>%
  filter(!is.na(memb_assoc)) %>%
  group_by(village, memb_assoc) %>%
  summarize(mean_no_membrs = mean(no_membrs),
            min_membrs = min(no_membrs)) %>%
  arrange(desc(min_membrs))

########## Exercise ########## 
# Use `group_by()` and `summarize()` to find the mean, min, and max
# number of household members for each village with. Also add the number of
# observations (hint: see `?n`).
############################## 

####################################################################################
## Counting                                                                       ##
####################################################################################
# When working with data, we often want to know the number of observations found
# for each factor or combination of factors.

interviews %>%
  count(village)

# `count()` provides the `sort` argument
interviews %>%
  count(village, sort = TRUE)

####################################################################################
## Reshaping with gather and spread                                               ##
####################################################################################
# In the spreadsheet lesson, 
# we discussed how to structure our data leading to the four rules defining a tidy dataset:
# 1. Each variable has its own column
# 2. Each observation has its own row
# 3. Each value must have its own cell
# 4. Each type of observational unit forms a table
# 
# Here we examine the fourth rule: Each type of observational unit forms a table.
# 
# In `interviews`, each row contains the values of variables associated with each
# record (the unit), values such as the number of household members or posessions
# associated with each record. What if instead of comparing records, we wanted to
# look at differences in households grouped by different types of housing
# construction materials?
# 
# We'd need to create a new table where each row (the unit) is comprised
# of values of variables associated with each housing material (e.g. for
# `respondent_wall_type`). In practical terms this means the values
# of the wall construction materials in `respondent_wall_type` would
# become the names of column variables and the cells would contain `TRUE` or `FALSE`.
# 
# Having created a new table, we can now explore the relationship within and
# between household types - for example we could compare the ratio of household
# members to sleeping rooms grouped by type of construction material. The key
# point here is that we are still following a tidy data structure, but we have
# reshaped the data according to the observations of interest.
# 
# The opposite transformation would be to transform column names into values of
# a variable.
# 
# We can do both these of transformations with two `tidyr` functions, `spread()`
# and `gather()`.

## Spreading
# 
# `spread()` takes three principal arguments:
# 1. the data
# 2. the *key* column variable whose values will become new column names.
# 3. the *value* column variable whose values will fill the new column variables.
# 
# Let's use `spread()` to transform interviews to create new columns for each type
# of wall construction material. We use the pipe as before too. Because both the
# `key` and `value` parameters must come from column values, we will create a
# dummy column (we'll name it `wall_type_logical`) to hold the value `TRUE`, which
# we will then place into the appropriate column that corresponds to the wall
# construction material for that respondent. When using `mutate()` if you give a
# single value, it will be used for all observations in the dataset. We will use
# `fill = FALSE` in `spread()` to fill the rest of the new columns for that row
# with `FALSE`.

interviews_spread <- interviews %>%
  mutate(wall_type_logical = TRUE) %>%
  spread(key = respondent_wall_type, value = wall_type_logical, fill = FALSE)
View(interviews_spread)
View(interviews)

## Gathering
# The opposing situation could occur if we had been provided with data in the form
# of `interviews_spread`, where the building materials are column names, but we
# wish to treat them as values of a `respondent_wall_type` variable instead.
# 
# In this situation we are gathering the column names and turning them into a pair
# of new variables. One variable represents the column names as values, and the
# other variable contains the values previously associated with the column names.
# We will do this in two steps to make this process a bit clearer.
# 
# `gather()` takes four principal arguments:
# 1. the data
# 2. the *key* column variable we wish to create from column names.
# 3. the *value* column variable we wish to create and fill with values
# associated with the key.
# 4. the names of the columns we use to fill the key variable (or to drop).
# 
# To recreate our original data frame, we will use the following:
# 1. the data - `interviews_spread`
# 2. the *key* column will be "respondent_wall_type" (as a character string). This
#    is the name of the new column we want to create.
# 3. the *value* column will be `wall_type_logical`. This will be either `TRUE` or
#    `FALSE`.
# 4. the names of the columns we will use to fill the key variable are
#    `burntbricks:sunbricks` (the column named "burntbricks" up to and including
#    the column named "sunbricks" as they are ordered in the data frame).
# 
interviews_gather <- interviews_spread %>%
  gather(key = respondent_wall_type, value = "wall_type_logical",
         burntbricks:sunbricks)

# This creates a data frame with 524 rows (4 rows per interview respondent). 
# The four rows for each respondent differ only in the
# value of the "respondent_wall_type" and "dummy" columns. 
# 
# Only one row for each interview respondent is informative - we know that if the
# house walls are made of "sunbrick" they aren't made of any other the other
# materials. Therefore, we can get filter our dataset to only keep values where
# `wall_type_logical` is `TRUE`. Because, `wall_type_logical` is already either
# `TRUE` or `FALSE`, when passing the column name to `filter()`, it will
# automatically already only keep rows where this column has the value `TRUE`. We
# can then remove the `wall_type_logical` column. We do all of these steps
# together in the next chunk of code:

interviews_gather <- interviews_spread %>%
  gather(key = "respondent_wall_type", value = "wall_type_logical",
         burntbricks:sunbricks) %>%
  filter(wall_type_logical) %>%
  select(-wall_type_logical)

# View both `interviews_gather` and `interviews_spread` and compare their
# structure. Notice that the rows have been reordered in `interviews_gather` such
# that all of the respondents with a particular wall type are grouped together.

####################################################################################
## Exporting data                                                                 ##
####################################################################################
# Similar to the `read_csv()` function used for reading CSV files into R, there is
# a `write_csv()` function that generates CSV files from data frames.

# In preparation for our next lesson on plotting, we are going to create a
# version of the dataset where each of the columns includes only one
# data value. To do this, we will use spread to expand the
# `months_lack_food` and `items_owned` columns. We will also create a couple of summary columns.

interviews_plotting <- interviews %>%
  ## spread data by items_owned
  mutate(split_items = strsplit(items_owned, ";")) %>%
  unnest() %>%
  mutate(items_owned_logical = TRUE) %>%
  spread(key = split_items, value = items_owned_logical, fill = FALSE) %>%
  rename(no_listed_items = `<NA>`) %>%
  ## spread data by months_lack_food
  mutate(split_months = strsplit(months_lack_food, ";")) %>%
  unnest() %>%
  mutate(months_lack_food_logical = TRUE) %>%
  spread(key = split_months, value = months_lack_food_logical, fill = FALSE) %>%
  ## add some summary columns
  mutate(number_months_lack_food = rowSums(select(., Apr:Sept))) %>%
  mutate(number_items = rowSums(select(., bicycle:television)))

# Now we can save this data frame to our `data_output` directory.
write_csv(interviews_plotting, path = "data_output/interviews_plotting.csv")
