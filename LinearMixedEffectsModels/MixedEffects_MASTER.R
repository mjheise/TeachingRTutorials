#################################################
#                                               #  
#        R: Linear Mixed Effects Models         #
#                                               #
#                  MJ Heise                     #
#                July 9, 2024                   #
#                                               #
#################################################

# CODE DESCRIPTION: This code simulates data for 50 participants to illustrate
# fitting mixed effects models in lme4. Data simulation code adapted from DeBruine & Barr,
# https://debruine.github.io/lmem_sim/articles/appendix1a_example_code.html

# 1. DATA SIMULATION
# Simulate data for n = 50 participants from a theoretical study measuring alcohol 
# use disorder (AUDIT) over time. 
#
# 2. TIDYVERSE REVIEW
# Review of last week's tidyverse functions to organize data for analyses.
# 
# 3. SUMMARY STATISTICS
# Learn functions in R to summarize data and to visualiZe missing data patterns. 
#
# 4. EXAMINE MISSING DATA
# Use vis_miss in the naniar package to visualize missing data.
#
# 5. VISUALIZE SUBJECT-LEVEL EFFECTS
# Plot data using ggplot to visualize data patterns.
#
# 6. FIT LME
# Fit mixed effects models in which sex, age, and visit number predict AUDIT score.
#
# 7. VISUALIZE RANDOM INTERCEPTS
# Overlay each subject's random intercept and fixed slope (in red) over their
# raw data points. 
#
# 8. CHECK ASSUMPTIONS
# Check statistical assumptions of linearity, normal distribution of residuals,
# and homoscedasticity/homogeneity of variance.


# Libraries
library(tidyverse) #v.2.0.0, data management
library(lme4) # v.1.1-34, linear mixed effects models
library(lmerTest) # v.3.1-3, p-values for LMEs
library(ggplot2) # v. v.3.4.3, data visualization
library(naniar) # v.1.0.0, visualize missing data
library(car) # v.3.1-2, levene's test
library(lattice) # v.0.22-5, qqmath
library(gtsummary) #v. 1.7.2, tbl_regression function
# devtools::install_github("debruine/lmem_sim") # nested data simulation

#### 1. DATA SIMULATION ####
set.seed(123)

my_sim_data <- function(
    n_subj     = 50,   # number of subjects
    n_obs      = 10,   # number of trials
    n_clinics  = 8,    # number of clinics
    beta_0     = 5,    # grand mean
    beta_1     = 3,    # effect of sex (higher for males)
    omega_0    = 1,    # by-item random intercept sd
    tau_0      = 2,    # by-subject random intercept sd
    clinic_sd  = 1,    # by-clinic random intercept sd
    rho        = 0.2,  # correlation between intercept and slope
    sigma      = 5     # residual (standard deviation)
) {
  
  # Simulate a sample of items (trials)
  items <- data.frame(
    item_id = seq_len(n_obs),
    trial = rep(seq_len(n_obs), times = 1),
    O_0i = rnorm(n = n_obs, mean = 0, sd = omega_0)
  )
  
  # Simulate a sample of subjects
  subjects <- faux::rnorm_multi(
    n = n_subj, mu = 0, sd = c(tau_0), r = rho, 
    varnames = c("T_0s", "T_1s")
  )
  subjects$subj_id <- 1:n_subj
  
  # Assign sex to subjects
  subjects$sex <- sample(c("male", "female"), n_subj, replace = TRUE)
  
  # Assign ages to subjects
  subjects$age <- sample(20:55, n_subj, replace = TRUE)
  
  # Create clinic random effects
  clinics <- data.frame(
    clinic_id = seq_len(n_clinics),
    C_0c = rnorm(n = n_clinics, mean = 0, sd = clinic_sd)
  )
  
  # Assign clinics to subjects
  subjects$clinic_id <- sample(seq_len(n_clinics), n_subj, replace = TRUE)
  
  # Cross subject and item IDs
  data <- crossing(subjects, items) %>%
    left_join(clinics, by = "clinic_id") %>%
    mutate(
      e_si = rnorm(nrow(.), mean = 0, sd = sigma),
      trial_effect = ifelse(trial <= 5, -0.5 * trial, 0.5 * (trial - 5)),
      overall_effect = ifelse(trial <= 5, -2, 2),
      sex_effect = ifelse(sex == "male", beta_1, 0),
      raw_RT = beta_0 + T_0s + O_0i + C_0c + sex_effect + trial_effect + overall_effect + e_si
    )
  
  # Scale RT to the range of 0-20 and round to no decimal points
  data <- data %>%
    mutate(
      audit = round(20 * (raw_RT - min(raw_RT)) / (max(raw_RT) - min(raw_RT)))
    ) %>%
    select(subj_id, item_id, trial, sex, age, clinic_id, audit)
  
  # Create missing data information
  missing_data <- data %>%
    group_by(subj_id) %>%
    summarise(
      n_missing = sample(1:3, 1),
      missing_rows = list(sample(n_obs, n_missing))
    ) %>%
    unnest(missing_rows)
  
  # Introduce missing data
  data <- data %>%
    left_join(missing_data, by = c("subj_id", "trial" = "missing_rows")) %>%
    mutate(audit = ifelse(!is.na(n_missing), NA, audit)) %>%
    select(-n_missing)
  
  return(data)
}


# Simulate the data
simulated_data <- my_sim_data()

# View the first few rows of the simulated data
head(simulated_data)

# Add in variable for pre/post-SIP
simulated_data %>%
  mutate(covidOnset = case_when(trial <= 5 ~ 'pre',
                                trial >= 6 ~ 'post')) -> simulated_data

#### 2. TIDYVERSE REVIEW ####
# Read in csv data file
dat <- read.csv('')

# Rename variables:
simulated_data %>%
  rename(subNo = subj_id,
         visit = trial,
         clinicNo = clinic_id) %>%
  select(-item_id) -> dat


#### 3. SUMMARY STATISTICS ####
# Number of female and male sex assigned at birth
dat %>%
  group_by(subNo) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(sex) %>%
  summarise(NumberOfSubjects = n())

# Number of visits for each participant
dat %>%
  filter(!is.na(audit)) %>%
  group_by(subNo) %>%
  summarise(n = n())


# Number of participants from each clinic
dat %>%
  group_by(subNo) %>%
  slice(1) %>%
  group_by(clinicNo) %>%
  summarise(n = n())

# Range of audit scores
range(dat$audit, na.rm = T)

# describe function in the psych package
describe(dat$audit)

# structure
str(dat)

# table function
table(dat$sex)
table(dat$visit)


#### 4. EXAMINE MISSING DATA ####
# We're going to use a function in the package "naniar" to examine missing data
dat %>%
  select(visit, sex, age, clinicNo, audit, covidOnset) %>%
  vis_miss()


#### 5. VISUALIZE DATA ####
# Visualize data: Differences in AUDIT score by sex
ggplot(data = dat, aes(x = visit, y = audit)) +
  geom_point(position = 'jitter', alpha = .7, size = 2) +
  geom_smooth(aes(color = sex, fill = sex), method = 'lm') +
  labs(y = 'AUDIT scores over time by sex', x = 'Visit number', title = 'AUDIT over Time') +
  scale_x_continuous(breaks=seq(from = 0, to = 10, by = 1)) +
  theme_minimal()

# Visualize a subset of data
# For large datasets, it is helpful to take a randomfacet_wrap by subject
# Take a random sample of 10 IDs
ids <- sample(dat$subNo, 10)

# Subset subjects in the id list
dat %>%
  filter(subNo %in% ids) -> datPlot

# Plot AUDIT data for 10 subjects
ggplot(data = datPlot, aes(x = visit, y = audit)) +
  geom_point(position = 'jitter', alpha = .7, size = 2) +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(y = 'AUDIT scores over time by sex', x = 'Visit number', title = 'AUDIT over Time') +
  scale_x_continuous(breaks=seq(from = 0, to = 10, by = 1)) +
  theme_minimal() + 
  facet_wrap(~subNo)


#### 6. FIT LME ####
fit.LME <- lmer(audit ~ sex + age + visit + (1|subNo) + (1|clinicNo),  data = dat)

# Print model summary
summary(fit.LME)

# You can also print a formatted table with odds ratio
fit.LME %>%
  tbl_regression(exp = T)

# Change reference category
dat %>%
  mutate(sex_refM = relevel(factor(sex), ref = 'male')) -> dat

# Re-fit model with new reference
fit.LME.male <- lmer(audit ~ sex_refM + age + visit + (1|subNo) + (1|clinicNo),  data = dat)

# Print model summary
summary(fit.LME.male)

# You can also print a formatted table with odds ratio
fit.LME.male %>%
  tbl_regression(exp = T)


#### 7. VISUALIZE RANDOM EFFECTS ####
# Extract the fixed effect intercept and slope for visit
fixedEffects <- fixef(fit.LME)
fixedIntercept <- fixedEffects["(Intercept)"]
fixedSlope <- fixedEffects["visit"]

# Extract random effects
randomEffects <- ranef(fit.LME)

# Extract subject intercepts and convert to a data frame
subjectIntercepts <- randomEffects$subNo
datSubjectIntercepts <- data.frame(subNo = rownames(subjectIntercepts), randomIntercept = subjectIntercepts[, "(Intercept)"])

# Compute the combined intercept for each subject
datSubjectIntercepts <- datSubjectIntercepts %>%
  mutate(combinedIntercept = fixedIntercept + randomIntercept)

# Merge the combined intercepts with the original data frame
datSubjectIntercepts %>%
  mutate(subNo = as.integer(subNo)) -> datSubjectIntercepts

dat <- dat %>%
  left_join(datSubjectIntercepts, by = "subNo")

# Subset subjects in the id list
dat %>%
  filter(subNo %in% ids) -> datPlot

# Plot each subject's raw data with their combined intercept using the fixed effect slope
ggplot(data = datPlot, aes(x = visit, y = audit)) +
  geom_point(position = 'jitter', alpha = .7, size = 2) +
  geom_smooth(method = 'lm', se = FALSE) +  
  labs(y = 'AUDIT scores over time by sex', x = 'Visit number', title = 'AUDIT over Time') +
  scale_x_continuous(breaks = seq(from = 0, to = 10, by = 1)) +
  theme_minimal() +
  facet_wrap(~ subNo) +
  geom_abline(aes(intercept = combinedIntercept, slope = fixedSlope), color = "red", linetype = "dashed")


#### 8. CHECK ASSUMPTIONS ####
# This has to be done on complete cases for analyses
# Create df to re-fit model without missing data
dat %>%
  drop_na() -> datComplete

# Fit the linear mixed-effects model
fit.LMEComplete <- lmer(audit ~ sex + age + visit + (1|subNo) + (1|clinicNo), data = datComplete)
summary(fit.LMEComplete)

# Test model assumptions:
# - Linearity (visual inspection)
plot(resid(fit.LMEComplete), datComplete$audit) 

# - Normal distribution of residuals (visual inspection)
qqmath(fit.LME) 

shapiro.test(dat$audit)

# - Homoscedasticity (we test for equal variance for each sex)
# Levene's test
leveneTest(residuals(fit.LMEComplete) ~ datComplete$sex)


