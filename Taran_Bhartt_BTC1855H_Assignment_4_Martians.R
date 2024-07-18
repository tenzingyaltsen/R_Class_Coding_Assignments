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

# Read the UFO data and make a copy
UFOData <- read.csv("ufo_subset.csv")
UFODataClean <- UFOData

# Improve the times for teh UFO data copy
UFODataClean$datetime <- strptime(UFODataClean$datetime, format = "%Y-%m-%d %H:%M")
UFODataClean$date_posted <- strptime(UFODataClean$date_posted, format = "%d-%m-%Y")

# Figure out which countries are present, and then create a set for country and state
table(UFODataClean$country) #identify the list of countries at play. They are all either Australian, Canadian, German, British, or American

MatchedStateCountry <- unique(UFODataClean[,3:4])
MatchedStateCountry <- MatchedStateCountry[-which(MatchedStateCountry[,1] == ""|MatchedStateCountry[,2] == ""), ] #remove the rows that don't have any data as they are not valid pairs
# the result is that the states "dc", "sk", and "wa" are shared across 2 different countries
# there are countries that are not represented: (france), (venezuala), india, thailand, spain, iran, afghanistan, norway, israel, south korea, honduras, sweden, germany, poland, mexico, japan, china, dominican republic, puerto rico, malaysia, bangladesh, u.a.r., italy, kuwait, romania, ukraine, bulgaria, nepal, bolivia, philippines, bahrain, kosovo, cyprus

CountryNames <- read.delim("Country_Names.txt") #make sure you have this file downloaded to the local drive

UFODataClean2 <- UFODataClean

for (i in 1:203){
  ChosenCountryName <- CountryNames[i,]
  CountryLocation <-grep(tolower(ChosenCountryName),UFODataClean$city)
  UFODataClean2$country[CountryLocation] <- ChosenCountryName
}

# dc state is american if the city has "washington" or "georgetown"
# sk state has all of the country names taken care of
# wa state has some city values that are unknown
USWashingtonState <- c(grep("washington",UFODataClean2$city),grep("georgetown", UFODataClean2$city)) #find all of the washingtons and georgetowns
EmptyCountries <- which(UFODataClean2$country=='') #find the remaining empty countries
UFODataClean2$country[intersect(USWashingtonState,EmptyCountries)] <- "United States of America"


for (i in 1:65){ # convert empty country codes to country names based on the state IDs
  ChosenStateName <- MatchedStateCountry[i,1]
  StateLocation <-grep(tolower(ChosenStateName),UFODataClean2$state)
  UFODataClean2$country[intersect(StateLocation,EmptyCountries)] <- MatchedStateCountry[i,2]
}

# Update the country short forms to match the full country names
UFODataClean2$country[UFODataClean2$country=="us"] <- "United States of America"
UFODataClean2$country[UFODataClean2$country=="ca"] <- "Canada"
UFODataClean2$country[UFODataClean2$country=="gb"] <- "Great Britain"
UFODataClean2$country[UFODataClean2$country=="au"] <- "Australia"
UFODataClean2$country[UFODataClean2$country=="uk/"] <- "Great Britain"

# After the above processing, 2466 previously empty country locations have been filled in. The missing locations are for those that were mispelled, are US territories, or were taken in places without an obvious country ID, i.e. the I.S.S. or during an airplane flight
EmptyCountriesCount <- sum(UFODataClean2$country=='')

