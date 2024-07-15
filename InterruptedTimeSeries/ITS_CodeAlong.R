#################################################
#                                               #  
#        R: Linear Mixed Effects Models         #
#                                               #
#                  MJ Heise                     #
#                July 15, 2024                   #
#                                               #
#################################################

# CODE DESCRIPTION: This tutorial builds on material covered in Linear Mixed
# Effects Models: https://github.com/mjheise/TeachingRTutorials. This tutorial fits
# an interrupted time series model for a toy dataset on alcohol use (AUDIT).
#
# 1. READ IN AND TIDY DATA
# Read in data, create 
#
# 2. FIT INTERRUPTED ITME SERIES
# Fit interrupted time series model in which AUDIT score is predicted by an
# interaction between time*treatment (COVID onset).
#
# 3. CHECK ASSUMPTIONS
# Check statistical assumptions of linearity, normal distribution of residuals,
# and homoscedasticity/homogeneity of variance.
#
# 4. VISUALIZE ITS
# Create ggplot to visualize ITS analysis. Export to powerpoint.


# Libraries
library(tidyverse) #v.2.0.0, data management
library(lme4) # v.1.1-34, linear mixed effects models
library(lmerTest) # v.3.1-3, p-values for LMEs
library(ggplot2) # v. v.3.4.3, data visualization
library(car) # v.3.1-2, levene's test
library(lattice) # v.0.22-5, qqmath
library(officer) # V.0.3.15, powerpoint ggplot
library(rvg) # v. 0.2.5, powerpoint ggplot

# Functions
# Save ggplot object in powerpoint slide
# -Input: The ggplot object that you want to save in a powerpoint.
# -Optional inputs: Specified width and height of the outputted graph.
#  If no arguments are specified, the graph will encompass the entire
#  powerpoint slide.
# -Notes: After running the function, a window will open that 
#  allows you to select the powerpoint file. The graph will
#  save on a new slide at the end of the powerpoint.

create_pptx <- function(plt = last_plot(), path = file.choose(), width = 0, height = 0){
  if(!file.exists(path)) {
    out <- read_pptx()
  } else {
    out <- read_pptx(path)
  }
  
  if (width != 0 & height != 0) {
    out %>%
      add_slide(layout = "Title and Content", master = "Office Theme") %>%
      ph_with(value = dml(ggobj = plt), location = ph_location(left = 0, top = 0,
                                                               width = width, height = height)) %>%
      print(target = path)
  } else {
    out %>%
      add_slide(layout = "Title and Content", master = "Office Theme") %>%
      ph_with(value = dml(ggobj = plt), location = ph_location_fullsize()) %>%
      print(target = path)
    
  }
  
}

#### 1. READ IN AND TIDY DATA ####
# Read in data

# Rename variables
dat %>%
  rename(subNo = subj_id,
         visit = trial,
         clinicNo = clinic_id) %>%
  select(-item_id) %>%
  mutate(covidBinary = case_when(covidOnset == 'pre' ~ 0,
                                 covidOnset == 'post' ~ 1)) -> dat

# Create a binary variable (0/1) for COVID-onset, where 0 is pre-COVID and 1 
# is post-COVID, name variable covidBinary
#
#
#

  

#### 2. FIT INTERRUPTED TIME SERIES ####
# Reminder: here was our LME model from last session:
fit.LME <- lmer(audit ~ sex + age + visit + (1|subNo) + (1|clinicNo),  data = dat)

# For an ITS, we are fitting an interaction between continuous time, and the
# interruption period (pre/post)
fit.ITS <- lmer(audit ~ visit*covidBinary + sex + age + (1|subNo) + (1|clinicNo),  data = dat)

# Print model summary
summary(fit.ITS)

# You can also print a formatted table with odds ratio
fit.ITS %>%
  tbl_regression(exp = T)

# How would we interpret this output?
# There's a significant effect of COVID/treatment, where alcohol use increased
# post-COVID. However, there is no interaction between COVID and time,
# so the slope of alcohol use increasing stayed the same pre/post COVID but there
# was a jump post-COVID-onset.

# Because there's a difference across sex, we would also look if there was potentially
# an interaction between sex, COVID, and time.
fit.sex <- lmer(audit ~ visit*covidBinary*sex + age + (1|subNo) + (1|clinicNo),  data = dat)

# Print model summary
summary(fit.sex)

# Here we see that there is no 3-way interaction, so we would interpret the more
# parsimonious (simpler) model (fit.ITS). 


#### 3. CHECK ASSUMPTIONS ####
# This has to be done on complete cases for analyses
# Create df to re-fit model without missing data
dat %>%
  drop_na() -> datComplete

# Fit the linear mixed-effects model
fit.sexComplete <- lmer(audit ~ visit*covidBinary*sex + age + (1|subNo) + (1|clinicNo),  data = datComplete)
summary(fit.sexComplete)

# Test model assumptions:
# - Linearity (visual inspection)
plot(resid(fit.sexComplete), datComplete$audit) 

# - Normal distribution of residuals (visual inspection)
qqmath(fit.sexComplete) 

shapiro.test(dat$audit)

# - Homoscedasticity (we test for equal variance for each sex)
# Levene's test
leveneTest(residuals(fit.sexComplete) ~ datComplete$sex)

# How do these tests of assumptions compare to last week's model (fit.LME) that 
# did not include an interaction between time*treatment?


#### 4. VISUALIZE ITS ####
# Plot 1: Visualize data: AUDIT score over time
ggplot(data = dat, aes(x = visit, y = audit)) +
  geom_point(position = 'jitter', alpha = .7, size = 2) +
  labs(y = 'AUDIT scores over time', x = 'Visit number', title = 'AUDIT over Time') +
  scale_x_continuous(breaks=seq(from = 0, to = 10, by = 1)) +
  theme_minimal()

# Plot 2: Add in regression lines to show "interruption" in AUDIT between visits 5 and 6
ggplot(data = dat, aes(x = visit, y = audit)) +
  geom_point(position = 'jitter', alpha = .7, size = 2) +
  geom_smooth(alpha = .5, method = 'lm', linetype = 1, size = 1, data = subset(dat, covidBinary == 0)) + 
  geom_smooth(alpha = .5, method = 'lm', linetype = 1, size = 1, data = subset(dat, covidBinary == 1)) +
  labs(y = 'AUDIT scores over time', x = 'Visit number', title = 'AUDIT over Time') +
  scale_x_continuous(breaks=seq(from = 0, to = 10, by = 1)) +
  theme_minimal()

# Plot 3: Show sex differences using "fill" and "color"
ggplot(data = dat, aes(x = visit, y = audit)) +
  geom_point(aes(color = sex), position = 'jitter', alpha = .7, size = 2) +
  geom_smooth(aes(fill = sex, color = sex), alpha = .5, method = 'lm', linetype = 1, size = 1, data = subset(dat, covidBinary == 0)) + 
  geom_smooth(aes(fill = sex, color = sex), alpha = .5, method = 'lm', linetype = 1, size = 1, data = subset(dat, covidBinary == 1)) +
  labs(y = 'AUDIT scores over time by sex', x = 'Visit number', title = 'AUDIT over Time') +
  scale_x_continuous(breaks=seq(from = 0, to = 10, by = 1)) +
  theme_minimal()

# If there's time... use fill and color to edit Plot 2 :)
# You can use https://htmlcolorcodes.com/ to specify your color using a hex code
#
#
#

# Save to powerpoint by running the function create_pptx above
# Save one of the plots above as an object titled "plot"
# Create a PowerPoint document
pptx_doc <- read_pptx()

# Add a slide
pptx_doc <- add_slide(pptx_doc, layout = "Title and Content", master = "Office Theme")

# Add the plot to the slide using the rvg package
pptx_doc <- ph_with(pptx_doc, dml(ggobj = plot), location = ph_location_fullsize())

# Save the PowerPoint document
# Rename your powerpoint presentation below with your name
# print(pptx_doc, target = "ITSPlot.pptx")


