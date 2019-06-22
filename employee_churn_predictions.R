# This project aims to uncover the factors that lead to employee attrition/churn/turnover from a company, i.e gradual loss 
# of employees over a period of time. Various contributing factors behind attrition are analyzed. The input files used for this
# project are artificially generated (distribution close to real-world data). 

# Loading the readr and dplyr packages

library(readr)
library(dplyr)

# Retrieving the current working directory

getwd()

# Importing the organization data

org <- read_csv("org.csv")

# Checking the structure of org dataset, the dplyr way

org

glimpse(org)

# Count active and inactive employees in the dataset

org %>% count(status)

# Calculate average turnover rate
# Approximately 18% customers are inactive in the database

org %>% summarize(turnover_rate = mean(turnover))

# Calculating turnover rate at each level and visualizing the same
# Analyst and Specialist turnover rate is maximum

df_level <- org %>% group_by(level) %>% 
                summarize(turnover_rate = mean(turnover))

df_level

# Importing ggplot2 library

library(ggplot2)

# Plotting bar plot between employee level and turnover rate

ggplot(df_level, aes(x = level, y = turnover_rate)) + geom_col()

# Calculating the employee location wise turnover rate

df_location <- org %>% 
  group_by(location) %>% 
  summarize(turnover_location = mean(turnover))

# Checking the results
df_location

# Visualizing the results with bar graph
ggplot(df_location, aes(x = location, y = turnover_location)) +
  geom_col()

# Select the employees at Analyst and Specialist level in a new dataframe
org2 <- org %>%
  filter(level %in% c("Analyst", "Specialist"))

# Validate the results
org2 %>% 
  count(level)

# Importing the rating dataset
rating <- read_csv('rating.csv')

# View the structure of rating dataset
glimpse(rating)

# Joining rating to organization dataset
org2 <- left_join(org, rating, by = "emp_id")

# Printing results
org2

# Importing the survey data
survey <- read_csv('survey.csv')

# Viewing the structure of rating dataset
glimpse(survey)

# Joining survey to org2 dataset for creating final merged org_final dataset
org_final <- left_join(org2, survey, by = "mgr_id")

# Printing results
org_final

# Comparing manager effectiveness scores for active and inactive employees
# Manager effectiveness score for inactive employees is lesser.

ggplot(org_final, aes(x = status, y = mgr_effectiveness)) +
  geom_boxplot()

# Reading the org_final dataset from drive

org_final <- read_csv('org_final.csv')

# View the structure of the dataset
glimpse(org_final)

# Number of variables in the dataset
Number_Of_Variables <- 34

# Comparing the travel distance of Active and Inactive employees
# Travel distance is much more for the Inactive emloyees

ggplot(org_final, aes(x = status, y = distance_from_home)) +
  geom_boxplot()

## Feature Engineering Section
##############################

# Creating new variable emp_age_diff = age difference between employee and their respective manager
# Idea is to explore attrition rates linked with manager age (young or old)

# Adding age_diff variable to the main data frame

emp_age_diff <- org_final %>% 
  mutate(age_diff = mgr_age - emp_age) 

# Plotting the distribution of age difference
# The median age difference for inactive employees is slightly higher than for 
# active employees. This may or may not be statistically significant.

ggplot(emp_age_diff, aes(x = status, y = age_diff)) + 
  geom_boxplot()

# Creating new variable job hop index = total experience / number of companies worked
# A job hopper is a person who switches jobs frequently for career advancement opportunities.

# Adding attribute job_hop_index
emp_jhi <- emp_age_diff %>% 
  mutate(job_hop_index = total_experience / no_previous_companies_worked)

glimpse(emp_jhi)

# Comparing job hopping index of Active and Inactive employees
# Median value is almost same for both

ggplot(emp_jhi, aes(x = status, y = job_hop_index)) + 
  geom_boxplot()

# Importing lubridate library for dealing with dates

library(lubridate)

# Creating new variable emp_tenure which is length of time employee spent in the org

# Converting relevant date columns from character to date

date_of_joining <- as.Date(emp_jhi$date_of_joining, format='%d/%m/%y')
cutoff_date <- as.Date(emp_jhi$cutoff_date, format='%d/%m/%y')
last_working_date <- as.Date(emp_jhi$last_working_date, format='%d/%m/%y')

emp_tenure <- emp_jhi %>%
  mutate(tenure = ifelse(status == "Active", 
                         time_length(difftime(date_of_joining, cutoff_date), 
                                     "years"), 
                         time_length(difftime(date_of_joining, last_working_date), 
                                     "years")))

# Compare tenure of active and inactive employees
# Median tenure of inactive employees is less than the tenure of active employees.
# Active employees spent greater time in the company

ggplot(emp_tenure, aes(x = status, y = tenure)) + 
  geom_boxplot()

# Analyzing compensation of employees

# Plot the distribution of compensation, histogram

ggplot(emp_tenure, aes(x = compensation)) + 
  geom_histogram(bins=7)

# Plotting distribution of compensation across all employee levels

ggplot(emp_tenure, 
       aes(x = level, y = compensation)) + 
  geom_boxplot()

# Comparing compensation of Active and Inactive employees across levels
# Variation to be noted between Analyst and Specialist level

ggplot(emp_tenure, 
       aes(x = level, y = compensation, fill = status)) + 
  geom_boxplot()

# Deriving compa-ratio = actual compensation / median compensation for assessing competitiveness of each employee's pay

# Add median_compensation and compa_ratio
emp_compa_ratio <- emp_tenure %>%  
  group_by(level) %>%   
  mutate(median_compensation = median(compensation), 
         compa_ratio = compensation / median_compensation)

# Look at the median compensation for each level       

emp_compa_ratio %>% 
  distinct(level, median_compensation)

# Adding compa_level
# "Above" if compa_ratio > 1, "Below" otherwise

emp_final <- emp_compa_ratio  %>%  
  mutate(compa_level = ifelse(compa_ratio > 1, "Above", "Below"))

# Compare compa_level for Active & Inactive employees
# A greater proportion of inactive employees were paid less than median compensation.

ggplot(emp_final, aes(x = status, fill = compa_level)) + 
  geom_bar(position = "fill")

# Determining information value of independent variables
# Measuring predictive power of independent variables

# Loading Information package
library(Information)

# Computing Information Value 
IV <- create_infotables(data = emp_final, y = "turnover")

# Print Information Value 
# Accepted standard: IV < 0.15 (Poor predictive power)
# Accepted standard: IV between 0.15 and 0.4 (Moderate predictive power)
# Accepted standard: IV > 0.4 (Strong predictive power)

IV$Summary

# Building a predictive model to analyze whether an employee will leave the org
###############################################################################

# Load caret
library(caret)

# Setting seed to get uniform results
#set.seed(100)

# Store row numbers for training dataset
index_train <- createDataPartition(emp_final$turnover, p = 0.5, list = FALSE)

# Create training dataset
train_set <- emp_final[index_train, ]

# Create testing dataset
test_set <- emp_final[-index_train, ]

# Ensuring both train and test set have same proportion of active and inactive employees

# Calculate turnover proportion in train_set
print('Turnover proportion in train_set')
train_set %>% 
  count(status) %>% 
  mutate(prop = n / sum(n))

# Calculate turnover proportion in test_set
print('Turnover proportion in test_set')
test_set %>% 
  count(status) %>% 
  mutate(prop = n / sum(n))

# Dropping irrelevant variables and saving the resulting object as train_set_multi

train_set_multi <- train_set %>%
  select(-c(emp_id, mgr_id, 
            date_of_joining, last_working_date, cutoff_date, 
            mgr_age, emp_age, 
            median_compensation, 
            department, status, job_hop_index))

glimpse(train_set_multi)

# Building a simple logistic regression model only using percent_hike predictor

simple_log <- glm(turnover ~ percent_hike, 
                  family = "binomial", data = train_set_multi)

# Printing summary

# The significance codes (shown by asterisks) are intended for quickly ranking the 
# significance of each variable. The significance codes indicate how certain we can be 
# that the independent variable has an impact on the dependent variable.

summary(simple_log)

# Building a multiple logistic regression model. Loading necessary packages.

library(hablar)
library(IDPmisc)
library(car)

#train_set_multi <- na.omit(train_set_multi)

# Dealing with NA/NAN/Inf values present in the dataframe

s(train_set_multi)
NaRV.omit(train_set_multi)

# Building multiple logistic regression model

multi_log <- glm(turnover ~ ., family = "binomial", 
                 data = train_set_multi)

# Print summary
summary(multi_log)

# Measuring Multicolinearity
# Steps are calculating VIF (Variable Inflation Factor) of the model, if VIF > 5, variable will be removed
# For multiple variables with VIF > 5, only the variable with highest VIF will be removed

# Check for multicollinearity
vif(multi_log)

# Which variable has the highest VIF?
highest <- "level"

# Removing level and checking for the highest VIF variable again
# This is done to prevent multicolinearity

model_1 <- glm(turnover ~ . - level, family = "binomial", 
               data = train_set_multi)

# Check multicollinearity again
vif(model_1)

# Which variable has the highest VIF value now?
highest <- "compensation"

# Removing compensation and fitting the model again 

final_log <- glm(turnover ~ . - compensation, family = "binomial", 
               data = train_set_multi)

# Building Final Logistic Regression Model
##########################################

# Predicting probability for turnover in training data (in-sample predictions)

# Make predictions for training dataset

prediction_train <- predict(final_log, newdata = train_set_multi, 
                            type = "response")


prediction_train_simple <- predict(simple_log, newdata = train_set_multi, 
                                                   type = "response")
                            
prediction_train[c(205, 645)]
print("Probability for turnover(MLR) for employee 205: 99%")
print("Probability for turnover(MLR) for employee 645: 19%")

prediction_train_simple[c(205, 645)]
print("Probability for turnover(SLR) for employee 205: 47%")
print("Probability for turnover(SLR) for employee 645: 16%")


# Making predictions for testing dataset (out of sample predictions)

prediction_test <- predict(final_log, newdata = test_set, 
                           type = "response")

# Looking at the prediction range for both train and test sets for MLR

hist(prediction_train)

hist(prediction_test)

# Creating confusion matrix for analyzing model performance
# First classifying predicted probabilities as 1 or 0 by using cut-off
# Classify predictions using a cut-off of 0.5

prediction_categories <- ifelse(prediction_test > 0.5, 1, 0)

# Constructing a confusion matrix

conf_matrix <- table(prediction_categories, test_set$turnover)
conf_matrix

confusionMatrix(conf_matrix)

# Designing Retention Strategy (Knowing who may leave)

# Loading tidypredict 

library(tidypredict)

# Calculating probability of turnover for active employees

emp_risk <- emp_final %>%  
  filter(status == "Active") %>% 

# Adding predictions using the final model
tidypredict_to_column(final_log)

# Look at the employee's probability of turnover (selecting top 5)

emp_risk %>% 
  select(emp_id, fit) %>% 
  top_n(5, wt = fit)

# If turnover probability is between 0.8 and 1, employee considered high risk
# If turnover probability is between 0.8 and 0.6, employee considered medium risk
# If turnover probability is between 0.5 and 0.6, employee considered low risk

# Create turnover risk buckets

emp_risk_bucket <- emp_risk %>% 
  mutate(risk_bucket = cut(fit, breaks = c(0, 0.5, 0.6, 0.8, 1), 
                           labels = c("no-risk", "low-risk", 
                                      "medium-risk", "high-risk")))

# Calculating ROI for proactive turnover monitoring
# percent_hike was one of the key factors in determining employee turnover
# Classification of employees based on percent hike is done as follows:
# 0 to 10, if 0 <= percent_hike <= 10
# 11 to 15, if 11 <= percent_hike <= 15
# 16 to 20, if 16 <= percent_hike <= 20

# Plotting histogram of percent hike
ggplot(emp_final, aes(x = percent_hike)) + 
  geom_histogram(binwidth = 3)

# Creating salary hike_range of Analyst level employees
emp_hike_range <- emp_final %>% 
  filter(level == "Analyst") %>% 
  mutate(hike_range = cut(percent_hike, breaks = c(0, 10, 15, 20),
                          include.lowest = TRUE, 
                          labels = c("0 to 10", 
                                     "11 to 15", "16 to 20")))

# Calculating the turnover rate for each salary hike range 
df_hike <- emp_hike_range %>% 
  group_by(hike_range) %>% 
  summarize(turnover_rate_hike = mean(turnover))

# Checking the results
# Average turnover rate for employees in the hike range 0 to 10 is 23% more 
# compared to the employees in 11 to 15.

df_hike

# Visualizing the results
ggplot(df_hike, aes(x = hike_range, y = turnover_rate_hike)) +  
  geom_col()







