# Data visualisation with ggplot2

####################################################################################
## Preparing data                                                                 ##
####################################################################################
library(tidyverse)

surveys <- read_csv("data_raw/portal_data_joined.csv")
surveys_complete <- surveys %>%
  filter(!is.na(weight),           # remove missing weight
         !is.na(hindfoot_length),  # remove missing hindfoot_length
         !is.na(sex))                # remove missing sex
species_counts <- surveys_complete %>%
  count(species_id) %>% 
  filter(n >= 50)
surveys_complete <- surveys_complete %>%
  filter(species_id %in% species_counts$species_id)

####################################################################################
## Plotting with ggplot2                                                          ##
####################################################################################
# To build a ggplot, we will use the following basic template:

# ggplot(data = <DATA>, mapping = aes(<MAPPINGS>)) +  <GEOM_FUNCTION>()

# - use the `ggplot()` function and bind the plot to a specific data frame using
#   the `data` argument
ggplot(data = surveys_complete)

# - define a mapping (using the aesthetic (`aes`) function), by selecting the variables to be plotted and specifying how to present them in the graph, e.g. as x/y positions or characteristics such as size, shape, color, etc.
ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length))

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
ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point()

# The `+` in the **`ggplot2`** package is particularly useful because it allows
# you to modify existing `ggplot` objects. This means you can easily set up plot
# templates and conveniently explore different types of plots, so the above plot
# can also be generated with code like this:

# Assign plot to a variable
surveys_plot <- ggplot(data = surveys_complete, 
                       mapping = aes(x = weight, y = hindfoot_length))

# Draw the plot
surveys_plot + 
  geom_point()

####################################################################################
## Building your plots iteratively                                                ##
####################################################################################
# Building plots with **`ggplot2`** is typically an iterative process. We start by
# defining the dataset we'll use, lay out the axes, and choose a geom:
ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point()

# Then, we start modifying this plot to extract more information from it. For
# instance, we can add transparency (`alpha`) to avoid overplotting:
ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1)

# We can also add colors for all the points:
ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1, color = "blue")

# Or to color each species in the plot differently, you could use a vector as an input to the argument color. 
# Because we are now mapping features of the data to a color, instead of setting one color for all points, 
# the color now needs to be set inside a call to the **`aes`** function. **`ggplot2`** will provide 
# a different color corresponding to different values in the vector. 
# We set the value of **`alpha`** outside of the **`aes`** function call because we are using the same value 
# for all points. Here is an example where we color by **`species_id`**:
ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1, aes(color = species_id))

# We can also specify the colors directly inside the mapping provided in the ggplot() function. 
ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length, color = species_id)) +
  geom_point(alpha = 0.1)

########## Exercise ########## 
# Use what you just learned to create a scatter plot of `weight` over `species_id` 
# with the `plot_type` showing in different colors. 
# Is this a good way to show this type of data?

############################## 

####################################################################################
## Boxplot                                                                        ##
####################################################################################
# We can use boxplots to visualize the distribution of weight within each species:
ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight)) +
  geom_boxplot()

# By adding points to a boxplot, we can have a better idea of the number of
# measurements and of their distribution:
ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight)) +
  geom_boxplot(alpha = 0) +
  geom_jitter(alpha = 0.3, color = "tomato")

# Notice how the boxplot layer is behind the jitter layer?
# What do you need to change in the code to put the boxplot in front of the points such that it’s not hidden?
ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight)) +
  geom_jitter(alpha = 0.3, color = "tomato") +
  geom_boxplot(alpha = 0)

########## Exercise ########## 
# 1. Boxplots are useful summaries, but hide the *shape* of the distribution. For
# example, if the distribution is bimodal, we would not see it in a
# boxplot. An alternative to the boxplot is the violin plot, where the shape
# (of the density of points) is drawn.
# - Replace the box plot with a violin plot; see `geom_violin()`.
# 
# 2. So far, we’ve looked at the distribution of weight within species. 
# Try making a new plot to explore the distribution of another variable within each species.
# - Create a boxplot for `hindfoot_length`. 
#   Overlay the boxplot layer on a jitter layer `geom_jitter` to show actual measurements.
# - Add color to the data points on your boxplot according to the `plot_id` from which the sample was taken.
# Hint: Check the class for plot_id. Consider changing the class of plot_id from integer to factor. 
# 
# Why does this change how R makes the graph?

############################## 

####################################################################################
## Plotting time series data                                                      ##
####################################################################################
# Let’s calculate number of counts per year for each genus. 
# First we need to group the data and count records within each group:
yearly_counts <- surveys_complete %>%
  count(year, genus)

# Time series data can be visualized as a line plot with years on the x axis and counts on the y axis:
ggplot(data = yearly_counts, mapping = aes(x = year, y = n)) +
  geom_line()

# Unfortunately, this does not work because we plotted data for all the genera together. 
# We need to tell ggplot to draw a line for each genus by including `group = genus`:
ggplot(data = yearly_counts, mapping = aes(x = year, y = n, group = genus)) +
  geom_line()

# We will be able to distinguish genera in the plot if we add colors:
ggplot(data = yearly_counts, mapping = aes(x = year, y = n, color = genus)) +
  geom_line()

####################################################################################
## Faceting                                                                       ##
####################################################################################
# ggplot2 has a special technique called faceting that allows the user to 
# split one plot into multiple plots based on a factor included in the dataset.

# There are two types of facet functions:
# facet_wrap() arranges a one-dimensional sequence of panels to allow them to cleanly fit on one page.
# facet_grid() allows you to form a matrix of rows and columns of panels.

# Both geometries allow to to specify faceting variables specified within vars(). 
# For example, facet_wrap(facets = vars(facet_variable)) or 
# facet_grid(rows = vars(row_variable), cols = vars(col_variable)).

# Let’s start by using facet_wrap() to make a time series plot for each species:
ggplot(data = yearly_counts, mapping = aes(x = year, y = n)) +
  geom_line() +
  facet_wrap(facets = vars(genus))

#Now we would like to split the line in each plot by the sex of each individual measured. 
# To do that we need to make counts in the data frame grouped by year, species_id, and sex:
yearly_sex_counts <- surveys_complete %>%
  count(year, genus, sex)

# We can now make the faceted plot by splitting further by sex using color (within each panel):
ggplot(data = yearly_sex_counts, mapping = aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap(facets =  vars(genus))

# Now let’s use `facet_grid()` to control how panels are organised by both rows and columns:
ggplot(data = yearly_sex_counts, 
       mapping = aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_grid(rows = vars(sex), cols =  vars(genus))

# You can also organise the panels only by rows (or only by columns):
# One column, facet by rows
ggplot(data = yearly_sex_counts, 
       mapping = aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_grid(rows = vars(genus))

# One row, facet by column
ggplot(data = yearly_sex_counts, 
       mapping = aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_grid(cols = vars(genus))  

########## Exercise ########## 
# Use what you just learned to create a plot that depicts 
# how the average weight of each species changes through the years.

##############################

####################################################################################
## ggplot2 themes                                                                 ##
####################################################################################
# Usually plots with white background look more readable when printed. 
# Every single component of a ggplot graph can be customized using the generic theme() function, 
# as we will see below. However, there are pre-loaded themes available that change the 
# overall appearance of the graph without much effort.

# For example, we can change our previous graph to have a simpler white background 
# using the `theme_bw()` function:
ggplot(data = yearly_sex_counts, 
         mapping = aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap(vars(genus)) +
  theme_bw()

# In addition to `theme_bw()``, which changes the plot background to white, 
# ggplot2 comes with several other themes which can be useful to quickly change 
# the look of your visualization. The complete list of themes is available at 
# https://ggplot2.tidyverse.org/reference/ggtheme.html.

# Now, let’s change names of axes to something more informative than ‘year’ and ‘n’ and add a title to the figure:
ggplot(data = yearly_sex_counts, mapping = aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap(vars(genus)) +
  labs(title = "Observed genera through time",
       x = "Year of observation",
       y = "Number of individuals") +
  theme_bw()

# Take a look at the ggplot2 cheat sheet, and think of ways you could improve the plot:
# https://github.com/rstudio/cheatsheets/raw/master/data-visualization-2.1.pdf


####################################################################################
## Arranging and exporting plots                                                  ##
####################################################################################
# Faceting is a great tool for splitting one plot into multiple plots, 
# but sometimes you may want to produce a single figure that contains 
# multiple plots using different variables or even different data frames. 
# The `gridExtra` package allows us to combine separate ggplots into a single figure using `grid.arrange()`:

install.packages("gridExtra")

library(gridExtra)

spp_weight_boxplot <- ggplot(data = surveys_complete, 
                             mapping = aes(x = genus, y = weight)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(x = "Genus", y = "Weight (g)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

spp_count_plot <- ggplot(data = yearly_counts, 
                         mapping = aes(x = year, y = n, color = genus)) +
  geom_line() + 
  labs(x = "Year", y = "Abundance")

grid.arrange(spp_weight_boxplot, spp_count_plot, ncol = 2, widths = c(4, 6))

# In addition to the ncol and nrow arguments, used to make simple arrangements, 
# there are tools for constructing more complex layouts:
# https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.html

# To save the plot, we can use the ggsave() function, 
# which allows you easily change the dimension and resolution of your plot 
# by adjusting the appropriate arguments (width, height and dpi).

my_plot <- ggplot(data = yearly_sex_counts, 
                  mapping = aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap(vars(genus)) +
  labs(title = "Observed genera through time",
       x = "Year of observation",
       y = "Number of individuals") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 12),
        text=element_text(size = 16))
ggsave("yearly_sex_counts.png", my_plot, width = 15, height = 10)

# This also works for grid.arrange() plots
combo_plot <- grid.arrange(spp_weight_boxplot, spp_count_plot, ncol = 2, widths = c(4, 6))
ggsave("combo_plot_abun_weight.png", combo_plot, width = 10, dpi = 300)

####################################################################################
## What's next?                                                                   ##
####################################################################################

# More on data visualisation?
# https://socviz.co/

# How about animation?
# https://github.com/thomasp85/gganimate

# How about interactive plots?
# https://www.r-graph-gallery.com/interactive-charts.html

# Maps?
# https://github.com/dkahle/ggmap

# Networks?
# https://cran.r-project.org/web/packages/ggnetwork/vignettes/ggnetwork.html
