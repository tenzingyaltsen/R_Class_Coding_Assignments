# Assignment 4 - Martians

# Read the data (ufo_subset.csv) into a dataframe
# Check and correct structural issues
# Identify the possible data (content) issues
# Your downstream analysis will require the variables 'country', 'shape' and 'duration seconds'. Analyze these columns, identify any problems (e.g. missingness, outliers, inconsistency etc) and implement your solution (e.g. remove, impute etc).
# NUFORC officials comment on sightings that may be hoax. Identify the best way to identify and remove these sightings from the dataset.
# Add another column to the dataset (report_delay) and populate with the time difference in days, between the date of the sighting and the date it was reported.
# Remove the rows where the sighting was reported before it happened.
# Create a table with the average report_delay per country.
# Create a histogram using the 'duration seconds' column.

# Data Dictionary:
# sighting's time: Contains date and time of sighting
# city: City in which UFO was sighted
# state: State code in which UFO was sighted
# country: Country code of Sighting
# shape: Shape of the UFO
# duration seconds: Duration of the Sighting in seconds
# duration hours min: Duration of the sighting in hours and min
# comments: Sighting description
# date posted: Posted date of the sighting
# latitude: Latitude coordinate of the sighting
# longitude: Longitude coordinate of the sighting

#-------------------------------------------------------------------------------
# Plan
# 1. Create a data frame using the ufo data
# 2. look at the data frame to see any obvious issues
# 3. Create a second array that focusses on the variables 'country', 'shape', 'duration seconds', 'datetime', and 'date_posted'. Edit the titles. Fix data types
# 4. NUFORC officials have put "((HOAX??))" in the comments. Screen them out
# 5. Add another column to the dataset called 'report_delay' that caculates the time difference in days between the date of the sighting and the date it was reported.
# 6. Remove the rows where the sighting was reported before it happened.
# 7. Create a table with the average report_delay per country.
# 8. Create a histogram using the 'duration seconds' column.

# NOTE TO REVIEWER
# install Lubridate
library(dplyr)
library(tidyr)
library(lubridate)

UFOData <- read.csv("ufo_subset.csv")

UFODataClean <- UFOData

UFODataClean$datetime <- strptime(UFODataClean$datetime, format = "%Y-%m-%d %H:%M")
UFODataClean$date_posted <- strptime(UFODataClean$date_posted, format = "%d-%m-%Y")

table(UFODataClean$country) #identify the list of countries at play. They are all either Australian, Canadian, German, British, or American
EmptyCountries <- UFODataClean$country=='' #find where the country value is missing

MatchedStateCountry <- unique(UFODataClean[,3:4])
MatchedStateCountry <- MatchedStateCountry[-which(MatchedStateCountry[,1] == ""|MatchedStateCountry[,2] == ""), ]
# the result is that the states "dc", "sk", and "wa" are shared across 2 different countries
# there are countries that are not represented: (france), (venezuala), india, thailand, spain, iran, afghanistan, norway, israel, south korea, honduras, sweden, germany, poland, mexico, japan, china, dominican republic, puerto rico, malaysia, bangladesh, u.a.r., italy, kuwait, romania, ukraine, bulgaria, nepal, bolivia, philippines, bahrain, kosovo, cyprus
