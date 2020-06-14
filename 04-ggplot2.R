# Data visualisation with ggplot2

####################################################################################
## Preparing data                                                                 ##
####################################################################################
library(tidyverse)

interviews <- read_csv("data/SAFI_clean.csv", na = "NULL")

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

####################################################################################
## Plotting with ggplot2                                                          ##
####################################################################################


# To build a ggplot, we will use the following basic template:
# ggplot(data = <DATA>, mapping = aes(<MAPPINGS>)) +  <GEOM_FUNCTION>()

# - use the `ggplot()` function and bind the plot to a specific data frame using
#   the `data` argument
ggplot(data = interviews_plotting)

# - define a mapping (using the aesthetic (`aes`) function), by selecting the variables to be plotted and specifying how to present them in the graph, e.g. as x/y positions or characteristics such as size, shape, color, etc.
ggplot(data = interviews_plotting, aes(x = no_membrs, y = number_items))

# - add 'geoms' – graphical representations of the data in the plot (points,
# lines, bars). **`ggplot2`** offers many different geoms; we will use some
# common ones today, including:
# 
# * `geom_point()` for scatter plots, dot plots, etc.
# * `geom_boxplot()` for, well, boxplots!
# * `geom_line()` for trend lines, time series, etc.
# 
# To add a geom to the plot use the `+` operator. Because we have two continuous variables,
# let's use `geom_point()` first:
ggplot(data = interviews_plotting, aes(x = no_membrs, y = number_items)) +
  geom_point()


# The `+` in the **`ggplot2`** package is particularly useful because it allows
# you to modify existing `ggplot` objects. This means you can easily set up plot
# templates and conveniently explore different types of plots, so the above plot
# can also be generated with code like this:

# Assign plot to a variable
interviews_plot <- ggplot(data = interviews_plotting, aes(x = no_membrs, y = number_items))

# Draw the plot
interviews_plot +
  geom_point()

####################################################################################
## Building your plots iteratively                                                ##
####################################################################################
# Building plots with **`ggplot2`** is typically an iterative process. We start by
# defining the dataset we'll use, lay out the axes, and choose a geom:
ggplot(data = interviews_plotting, aes(x = no_membrs, y = number_items)) +
  geom_point()

# Then, we start modifying this plot to extract more information from it. For
# instance, we can add transparency (`alpha`) to avoid overplotting:
ggplot(data = interviews_plotting, aes(x = no_membrs, y = number_items)) +
  geom_point(alpha = 0.5)

# That only helped a little bit with the overplotting problem. We can also
# introduce a little bit of randomness into the position of our points
# using the `geom_jitter()` function.
ggplot(data = interviews_plotting, aes(x = no_membrs, y = number_items)) +
  geom_jitter(alpha = 0.5)

# We can also add colors for all the points:
ggplot(data = interviews_plotting, aes(x = no_membrs, y = number_items)) +
  geom_jitter(alpha = 0.5, color = "blue")

# Or to color each village in the plot differently, you could use a vector as an input to the argument **`color`**.  
# Because we are now mapping features of the data to a color, instead of setting one color for all points, the color now needs to be set inside a call to the **`aes`** function. **`ggplot2`** will provide a different color corresponding to different values in the vector. We set the value of **`alpha`** outside of the **`aes`** function call because we are using the same value for all points. Here is an example where we color by **`village`**:
ggplot(data = interviews_plotting, aes(x = no_membrs, y = number_items)) +
  geom_jitter(aes(color = village), alpha = 0.5)

# There appears to be a positive trend between number of household
# members and number of items owned (from the list provided). This trend
# does not appear to be different by village.

########## Exercise ########## 
# Use what you just learned to create a scatter plot of `rooms` by `village`
# with the `respondent_wall_type` showing in different colors. Is this a good
# way to show this type of data?


############################## 

####################################################################################
## Boxplot                                                                        ##
####################################################################################
# We can use boxplots to visualize the distribution of rooms for each
# wall type:
ggplot(data = interviews_plotting, aes(x = respondent_wall_type, y = rooms)) +
  geom_boxplot()

# By adding points to a boxplot, we can have a better idea of the number of
# measurements and of their distribution:
ggplot(data = interviews_plotting, aes(x = respondent_wall_type, y = rooms)) +
  geom_boxplot(alpha = 0) +
  geom_jitter(alpha = 0.5, color = "tomato")

# We can see that muddaub houses and sunbrick houses tend to be smaller than
# burntbrick houses.

########## Exercise ########## 
# 1. Boxplots are useful summaries, but hide the *shape* of the distribution. For
# example, if the distribution is bimodal, we would not see it in a
# boxplot. An alternative to the boxplot is the violin plot, where the shape
# (of the density of points) is drawn.
# - Replace the box plot with a violin plot; see `geom_violin()`.
# 2. So far, we've looked at the distribution of room number within wall type. Try
# making a new plot to explore the distribution of another variable within wall
# type.
# - Create a boxplot for `liv_count` for each wall type. Overlay the boxplot
#   layer on a jitter layer to show actual measurements.
# - Add color to the data points on your boxplot according to whether the
#   respondent is a member of an irrigation association (`memb_assoc`).

############################## 

####################################################################################
## Barplots                                                                       ##
####################################################################################
# Barplots are also useful for visualizing categorical data. By default,
# `geom_bar` accepts a variable for x, and plots the number of instances each
# value of x (in this case, wall type) appears in the dataset.
ggplot(data = interviews_plotting, aes(x = respondent_wall_type)) +
    geom_bar()

# We can use the `fill` aesthetic for the `geom_bar()` geom to color bars by
# the portion of each count that is from each village.

ggplot(data = interviews_plotting, aes(x = respondent_wall_type)) +
    geom_bar(aes(fill = village))

# This creates a stacked bar chart. These are generally more difficult to read
# than side-by-side bars. We can separate the portions of the stacked bar that
# correspond to each village and put them side-by-side by using the `position`
# argument for `geom_bar()` and setting it to "dodge".
ggplot(data = interviews_plotting, aes(x = respondent_wall_type)) +
    geom_bar(aes(fill = village), position = "dodge")

# This is a nicer graphic, but we're more likely to be interested in the
# proportion of each housing type in each village than in the actual count of
# number of houses of each type (because we might have sampled different numbers
# of households in each village). To compare proportions, we will first create a
# new data frame (`percent_wall_type`) with a new column named "percent"
# representing the percent of each house type in each village. We will remove
# houses with cement walls, as there was only one in the dataset.

percent_wall_type <- interviews_plotting %>%
    filter(respondent_wall_type != "cement") %>%
    count(village, respondent_wall_type) %>%
    group_by(village) %>%
    mutate(percent = n / sum(n)) %>%
    ungroup()

# Now we can use this new data frame to create our plot showing the
# percentage of each house type in each village.
ggplot(percent_wall_type, aes(x = village, y = percent, fill = respondent_wall_type)) +
     geom_bar(stat = "identity", position = "dodge")


########## Exercise ########## 
# Create a bar plot showing the proportion of respondents in each
# village who are or are not part of an irrigation association
# (`memb_assoc`). Include only respondents who answered that question
# in the calculations and plot. Which village had the lowest proportion of
# respondents in an irrigation association?

############################## 

####################################################################################
## Adding Labels and Titles                                                       ##
####################################################################################
# By default, the axes labels on a plot are determined by the name of the variable
# being plotted. However, ggplot2 offers lots of customization options,
# like specifying the axes labels, and adding a title to the plot with 
# relatively few lines of code. We will add more informative x and y axis
# labels to our plot of proportion of house type by village and also add
# a title.
ggplot(percent_wall_type, aes(x = village, y = percent, fill = respondent_wall_type)) +
    geom_bar(stat = "identity", position = "dodge") +
    ylab("Percent") +
    xlab("Wall Type") +
    ggtitle("Proportion of wall type by village")

####################################################################################
## Faceting                                                                       ##
####################################################################################
# Rather than creating a single plot with side-by-side bars for each
# village, we may want to create multiple plot, where each plot shows the
# data for a single village. This would be especially useful if we had
# a large number of villages that we had sampled, as a large number of
# side-by-side bars will become more difficult to read.

# ggplot2 has a special technique called *faceting* that allows the user to split one
# plot into multiple plots based on a factor included in the dataset. We
# will use it to split our barplot of housing type proportion by village
# so that each village has it's own panel in a multi-panel plot:

ggplot(percent_wall_type, aes(x = respondent_wall_type, y = percent)) +
    geom_bar(stat = "identity", position = "dodge") +
    ylab("Percent") +
    xlab("Wall Type") +
    ggtitle("Proportion of wall type by village") +
    facet_wrap(~ village)

# Usually plots with white background look more readable when printed.  We can set
# the background to white using the function `theme_bw()`. Additionally, you can remove
# the grid:
ggplot(percent_wall_type, aes(x = respondent_wall_type, y = percent)) +
  geom_bar(stat = "identity", position = "dodge") +
  ylab("Percent") +
  xlab("Wall Type") +
  ggtitle("Proportion of wall type by village") +
  facet_wrap(~ village) +
  theme_bw() +
  theme(panel.grid = element_blank())

# What if we wanted to see the proportion of respondents in each village
# who owned a particular item? We can calculate the percent of people
# in each village who own each item and then create a faceted series of
# bar plots where each plot is a particular item. First we need to
# calculate the percentage of people in each village who own each item:

percent_items <- interviews_plotting %>%
    gather(items, items_owned_logical, bicycle:no_listed_items) %>%
    filter(items_owned_logical) %>%
    count(items, village) %>%
    ## add a column with the number of people in each village
    mutate(people_in_village = case_when(village == "Chirodzo" ~ 39,
                                         village == "God" ~ 43,
                                         village == "Ruaca" ~ 49)) %>%
    mutate(percent = n / people_in_village)

# To calculate this percentage data frame, we needed to use the `case_when()`
# parameter within `mutate()`. In our earlier examples, we knew that each house
# was one and only one of the types specified. However, people can (and do) own
# more than one item, so we can't use the sum of the count column to give us the
# denominator in our percentage calculation. Instead, we need to specify the
# number of respondents in each village. Using this data frame, we can now create
# a multi-paneled bar plot.

ggplot(percent_items, aes(x = village, y = percent)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ items) +
    theme_bw() +
    theme(panel.grid = element_blank())

####################################################################################
## Saving ggplot                                                                  ##
####################################################################################

# Define the plot into an object
my_plot <- ggplot(percent_items, aes(x = village, y = percent)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ items) +
  labs(title = "Percent of respondents in each village \n who owned each item",
       x = "Village",
       y = "Percent of Respondents") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 45, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 12),
        text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5))

# Use ggsave()
ggsave("fig_output/name_of_file.png", my_plot, width = 15, height = 10)
