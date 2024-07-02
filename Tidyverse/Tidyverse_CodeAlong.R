#################################################
#                                               #  
#       R: Data management in tidyverse         #
#                                               #
#                  MJ Heise                     #
#                June 27, 2024                  #
#                                               #
#################################################

# CODE DESCRIPTION: This code simulates data for 20 participants to illustrate
# data management functions in tidyverse.

# 1. DATA SIMULATION: 
# Simulate data for n = 20 participants from a theoretical study recording viral
# load over time. 
#
# 2. READ IN CSV:
# How to read in a csv file using <- and =
# 
# 3. MUTATE AND RENAME:
# How to use mutate to create new variables and rename to rename existing variables.
#
# 4. ARRANGE:
# How to use arrange to sort data.
#
# 5. GROUP BY AND SUMMARISE
# How to group data in order to generate descriptive statistics over a portion of the 
# observations (e.g., within each subject).
#
# 6. FILTER, SELECT, SUBSET
# Select rows (using filter) and columns (using select).
#
# 7. PIVOT WIDER AND LONGER
# Pivot_wider() and pivot_longer() to reshape dfs. 


# Libraries
library(tidyverse) #v.2.0.0, data management
library(psych) # v.2.3.9, summary statistics


#### 2. READ IN CSV ####
# Set your working directory
# A wd is the directory on your computer where all of your files will be stored
# and new files will be saved to. 
# Depending on how your username is set up on your computer, a good place
# to start is by running to see your current directory.
getwd()

# On a windows...
setwd('')

# read.csv is a base R function that reads in csv files. To read in other types
# of data files, you can use read_dta (in the haven package, to read stata files),
# and read_excel (in the readxl package, for excel files).
dat <- read.csv('TidyverseTutorialData.csv')

# R uses = and <- in some instances interchangeably. I think of <- as "sending"
# something to a new object/df, but you could also write 
dat = read.csv('TidyverseTutorialData.csv')

# This tutorial is written as comments using the # symbol, and a shortcut for
# is to leave your cursor on the line you want to comment out and then type 
# cntrl+shift+c (windows!) or cmd+shift+c (mac)

#### 3. MUTATE AND RENAME ####
# Tidyverse uses something called a "pipe" to open up your df and you string together
# pipes to call new functions. A pipe looks like this: %>%
# There's a keyboard shortcut to insert a pipe: Ctrl + Shift + M (Windows) or 
# Cmd + Shift + M (Mac).

# Rename: rename changes the name of a variable while keeping the column's contents
# intact. 
# The syntax for rename is rename(NewName = OldName)
# I use camelcase in naming variables, so I will rename all variables in this df.

dat %>%
  rename(subNo = SubjectID,
         sex = Sex,
         age = Age,
         date = Date,
         viralLoad = ViralLoad) -> dat # -> then sends this new df to an object
# called "dat", which will overwrite our old dataframe. 
# For troubleshooting, I recommend creating new objects so that you don't need 
# to re-run all of your previous code if you decide to revise the names in rename().

# Next we will uses mutate to categorize viral load data as suppressed or not
# suppressed.
# We will use case_when to create conditions that when met will fill in that value
# for viralLoadSuppressed. 
# .default = NA means that if the value in viralLoad does not meet either condition
# that viralLoadSuppressed will have an NA for that row. 
dat %>%
  mutate(viralLoadSuppressed = case_when((viralLoad >= 20 & viralLoad <= 200) ~ 'Suppressed',
                                         (viralLoad > 200) ~ 'Unsuppressed',
                                         .default = NA)) -> dat

# We might also want a viralLoadSuppressed variable that is binary (1/0) for
# later data analyses.
# Here I'm using a base R function (ifelse) instead of case_when. Ifelse
# is uglier because it doesn't indent nicely, and slower because case_when is 
# vectorized but ifelse is not. It's good practice to use case_when instead :)
dat %>%
  mutate(viralLoadSuppressed_bin = ifelse(viralLoad >= 20 & viralLoad <=200, 1,
                                          ifelse(viralLoad > 200, 0,
                                                 NA))) -> dat

#### 4. ARRANGE ####
# We can use group_by and arrange to group the output of our df. 
# If we look at our df, we can see that the dates are not in any particular order.
head(dat, 10) # where 10 gives you the number of rows to print

# We can arrange the df by just date (the default is earliest to latest,
# but you can use desc() to get latest to earliest)
dat %>%
  arrange(date) %>%
  head(10)

# Dat arranged by most recent date first
#
#
#

# You can also use arrange and group by multiple variables (e.g., subNo and date)
#
#
#

#### 5. GROUP BY AND SUMMARISE ####
# There are some summary functions in several packages in R -- I like the 
# describe function in the psych package
describe(dat)

# vars          n     mean      sd median trimmed     mad min  max range  skew kurtosis     se
# SubjectID    1 57   10.61    6.03     11   10.64    7.41   1   20    19 -0.06    -1.33   0.80
# Date         2 57     NaN      NA     NA     NaN      NA Inf -Inf  -Inf    NA       NA     NA
# Age          3 57   37.51    7.83     39   37.70    8.90  22   51    29 -0.31    -0.95   1.04


# But you can see that this won't be accurate, the n corresponds to the
# number of observations (i.e., rows) rather than the number of participants.

# We can use group_by to group to create clusters within the df that can be
# mutated/summarised/etc.
# Let's get the number of observations each participant has
dat %>%
  group_by(subNo) %>%
  summarise(numObservations = n())

# Let's get the proportion of visits where the participant was suppressed!
# 
#
#
#
  

#### 6. FILTER, SELECT, SUBSET ####
# Filter will select certain rows that meet a condition
dat %>%
  filter(age >= 35) %>%
  summarise(n = n()) # Now you can see that when we select participants who are 
# 35 or older, there are 36 observations selected from the original 57

# If we want to select participants who have 2+ observations...
dat %>%
  group_by(subNo) %>%
  summarise(numObservations = n()) %>%
  filter(numObservations >= 2) %>%
  select(subNo) -> subList # this saves a list of subject IDs with 2+ observations

dat %>%
  filter(subNo %in% subList$subNo) # This filters subject IDs that were in our list
# called "subList" with 2+ observations

# Filter only observations in 2023 and calculate the proportion of viral suppression
# across all visits
#
#
#
#

# Select will select certain columns from a dataset. This can be helpful if you have 
# really large datasets and are running an analysis that uses a small subset of data, 
# if you want to pivot data longer or wider and including additional columns doesn't make
# sense in the context of the new df, or if you're merging two dfs that share 
# duplicate column names.
dat %>%
  select(subNo, date,viralLoadSuppressed) %>%
  head(10)

# You can perform filter and select within the same function using subset:
dat %>%
  subset(subNo %in% subList$subNo, # this first line is the "filter" part
         select = c(subNo, date)) # this second line is the "select" part

# c(var1, var2, etc.) describes a list, and you use c() to feed the subset function a
# list of variables, whereas select() will identify the list of variables without c(),
# I'm not sure why but that's how subset works!


# Let's use filter and summarise to find the mean and sd of age!
# Age is time-varying, so we will calculate age at study initiation (i.e., age at 
# first visit)
#
#
#
#
#
  

#### 7. PIVOT WIDER AND LONGER ####
# Different analyses require data to be in certain formats -- the two different data 
# formats are long (this is how our data is currently arranged) and wide. Wide data
# will have repeated observations in across columns rather than in rows. Data is reshaped
# using pivot functions -- pivot_longer (to reshape from wide to long) and pivot_wider
# (to reshape from long to wide).

# We will reshape data so that each number visit (e.g., 1st, 2nd, 3rd) is in a new column.
# First, let's make a column that corresponds to visit number
dat %>%
  group_by(subNo) %>%
  arrange(date) %>%
  mutate(visitNumber = paste('Visit ', row_number(), sep = '')) -> dat


# Pivot wider
dat %>%
  pivot_wider(id_cols = c(subNo, sex),
              names_from = visitNumber,
              values_from = viralLoad) -> datWide

# Now we can do analyses that require one row per subject (e.g., t-test, linear regression,
# generalized linear regression)
