# CR: <- Indicates a code review comment. 

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

#' CR: This is helpful to include, especially for getting a glimpse of 
#' the data prior to cleaning.
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
# 3. Create a second array that focusses on the variables 'country', 'shape', 
#    'duration seconds', 'datetime', and 'date_posted'. Edit the titles. Fix data types
# 4. NUFORC officials have put "((HOAX??))" in the comments. Screen them out
# 5. Add another column to the dataset called 'report_delay' that caculates the 
#    time difference in days between the date of the sighting and the date it was reported.
# 6. Remove the rows where the sighting was reported before it happened.
# 7. Create a table with the average report_delay per country.
# 8. Create a histogram using the 'duration seconds' column.

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# NOTE TO REVIEWER
# install the following packages. You can use the following code:
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("lubridate")
# install.packages("graphics")
# install.packages("sp")
# install.packages("rworldmap")

library(dplyr)
library(tidyr)
library(lubridate)
library(graphics)
library(sp)
library(rworldmap)
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Read the UFO data and make a copy
UFOData <- read.csv("ufo_subset.csv")
UFODataClean <- UFOData

#' CR: Maybe try using the glimpse() or str() functions to get an overview
#' of the data prior to honing in on a specific variable.

# Improve the times for the UFO data copy, converting them from characters to POSIX
UFODataClean$datetime <- strptime(UFODataClean$datetime, format = "%Y-%m-%d %H:%M")
UFODataClean$date_posted <- strptime(UFODataClean$date_posted, format = "%d-%m-%Y")

#' CR: This is a helpful feature and identifies the issue of ambiguous
#' "state" (i.e., which country's state).

# Figure out which countries are present, and then create a vertex that matches 
# country and state
MatchedStateCountry <- unique(UFODataClean[,3:4]) #identify the unique 
# combinations of state and country, including where one is matched to an empty cell
MatchedStateCountry <- MatchedStateCountry[-which(MatchedStateCountry[,1] == ""|MatchedStateCountry[,2] == ""), ] 
#remove the rows that don't have any data as they are not valid pairs

# the result is that the states "dc", "sk", and "wa" are shared across 2 
# different countries
# there are countries that are not represented: France, Venezuela, India, 
# Thailand, Spain, Iran, Afghanistan, Norway, Israel, South Korea, Honduras, 
# Sweden, Germany, Poland, Mexico, Japan, China, Dominican Republic, Malaysia,
# Bangladesh, Italy, Kuwait, Romania, Ukraine, Bulgaria, Nepal, Bolivia,
# Philippines, Bahrain, Kosovo, Cyprus, etc.

# Get the other country names that are not represented in the current country column
CountryNames <- read.delim("Country_Names.txt")
# make sure you have this file downloaded to the local drive
# the countries come from the US State Department. Certain names have been modified
# to reflect reality, and the way names are presented in the data set

UFODataClean2 <- UFODataClean # create a copy data frame

# go through the city column and identify if the country was named
for (i in 1:203){ #for each of the 203 countries
  ChosenCountryName <- CountryNames[i,]
  CountryLocation <-grep(tolower(ChosenCountryName),UFODataClean$city)
  # Make the country name in lowercase to match the format of the city column
  UFODataClean2$country[CountryLocation] <- ChosenCountryName
}

# State Issues--resolve the following:
# dc state is assigned to both Australia and the USA. At this point, all 
# Australian dc states have been accounted for
# Likewise, wa state has had all of its Australian values accounted for
MatchedStateCountry <- MatchedStateCountry[-c(56,63),]
EmptyCountries <- which(UFODataClean2$country=='') #find the remaining empty countries


for (i in 1:65){ # convert empty country codes to country names based on the state IDs
  ChosenStateName <- MatchedStateCountry[i,1]
  StateLocation <-grep(tolower(ChosenStateName),UFODataClean2$state)
  UFODataClean2$country[intersect(StateLocation,EmptyCountries)] <- MatchedStateCountry[i,2]
}

#' CR: Nice step, having the full country name is likely better for the
#' end viewer.

# Update the country short forms to match the full country names
UFODataClean2$country[UFODataClean2$country=="us"] <- "United States of America"
UFODataClean2$country[UFODataClean2$country=="ca"] <- "Canada"
UFODataClean2$country[UFODataClean2$country=="gb"] <- "Great Britain"
UFODataClean2$country[UFODataClean2$country=="au"] <- "Australia"
UFODataClean2$country[UFODataClean2$country=="uk/"] <- "Great Britain"

# remove hoaxes
FakeSightings <- (grep("((HOAX??))",UFODataClean2$comments)) 
#determine the indices with fake sightings, which include a "((HOAX??))"
UFODataClean3 <- UFODataClean2[-FakeSightings,] 
#remove the rows with fake sightings

# After the above processing, 2466 previously empty country locations have been 
# filled in. The missing locations are for those that were mispelled, are US 
# territories, or were taken in places without an obvious country ID, 
# i.e. the I.S.S. or during an airplane flight
# The number of empty countries is as follows:
EmptyCountriesCount <- sum(UFODataClean2$country=='')

#create the data frame that will be used to create the histogram
UFODataKeyDetails <- UFODataClean3[c(1,4:6,9)] 
UFODataKeyDetails <- UFODataKeyDetails %>% #rename the column headings
  rename(
    sighting_date = datetime,
    recording_date = date_posted,
    duration_seconds = duration.seconds
  )%>% #create a new column that reflects the difference in days between UFO 
       #sighting and reporting
  mutate(report_delay = difftime(recording_date,sighting_date,units = "days"))
# the resulting difference in times are reported in days. Some negative times 
# are less than a day, but may just be a result of the sighting time having hours
# and seconds, while the reporting time is only the date

TimeTravellers <- which(UFODataKeyDetails$report_delay<=-1) 
#determine the rows where the recording of a UFO preceded the sighting
UFODataKeyDetails <- UFODataKeyDetails[-TimeTravellers,] 
#remove time travellers

# CR: You create/assign a table here but don't display it to the user.

# Create a table for each country's average report delay
UFOReportingDelay <- UFODataKeyDetails%>%
  group_by(country) %>% 
  #groups by the countries, this does not change the frontend output but modifies the backend
  summarize(mean_report_delay =mean(report_delay, na.rm = T))
#you need na.rm=T for the average because in row 10193, the United States only has a reporting date

EmptyCountries <- which(UFOReportingDelay=='')
#locate which countries don't have a reporting delay
UFOReportingDelay <- UFOReportingDelay[-EmptyCountries,] 
#remove these countries from the data frame

UFOReportingDelay <- arrange(UFOReportingDelay, desc(mean_report_delay))
# arrange the reporting delays in descending order, from longest to shortest
# this gives the subsequent bar plots some semblance of order

# Plotting UFO sighting delays
# NOTE TO REVIEWER: the length of the country names requires large margins. In 
# order for the plots to render correctly, expand the size of the plot window
# if it is not expanded then the following error will occur:
# "Error in plot.new() : figure margins too large"

#' CR: Nice bar plots! Maybe adjusting the time unit for the y-axis might
#' help the graphs not be as compressed.
# CR: What does the par() function do?

par(mar=c(9,4,4,1)) #set the margins to fit the country names
barplot(as.numeric(unlist(UFOReportingDelay[,2])), names.arg=unlist(UFOReportingDelay[,1]),las=2,
        ylab = "Days From Sighting",
        main = "UFO Reporting Delay for 105 Countries")
# Note to reviewer, since 105 countries have been identified by the dataset,
# not all of the countries show up

par(mar=c(7,4,4,1)) #set the margins to fit the country names
barplot(as.numeric(unlist(UFOReportingDelay[1:20,2])), names.arg=unlist(UFOReportingDelay[1:20,1]),las=2,
        ylab = "Days From Sighting",
        main = "Top 20 Worst Countries for Prompt UFO Reporting")

par(mar=c(14,4,4,1)) #set the margins to fit the country names
barplot(as.numeric(unlist(UFOReportingDelay[85:105,2])), names.arg=unlist(UFOReportingDelay[85:105,1]),las=2,
        ylab = "Days From Sighting",
        main = "Top 20 Best Countries for Prompt UFO Reporting")

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Getting countries from coordinates
# this section of code uses code from the Stack Overflow topic: 
# "Convert latitude and longitude coordinates to country name in R"

coordinateList <- UFODataClean[c(11,10)] #create a coordinate list from the given
# latitude and longitudes

# establish the basic map that the coordinates will be measured against
# this map has 244 countries and territories registered
countriesSP <- getMap(resolution='low')

# convert the list of points to a SpatialPoints object
pointsSP = SpatialPoints(coordinateList, proj4string=CRS(proj4string(countriesSP)))  

# use 'over' to get indices of the Polygons object containing each SpatialPoints object
# these indices refer to specific points on the Earth
indices = over(pointsSP, countriesSP)

# return the ADMIN names of each country, where the ADMIN names are the official 
# country names. Note, this dataset separates countries from their territories,
# i.e. Peurto Rico is separated from the USA
CountriesFromCoordinates <- indices$ADMIN

UFODataMark2 <- UFODataClean[c(1,8,9)]
UFODataMark2$country <- CountriesFromCoordinates

FakeSightings <- (grep("((HOAX??))",UFODataMark2$comments)) #determine the indices with fake sightings
UFODataMark2 <- UFODataMark2[-FakeSightings,] #remove the rows with fake sightings

UFODataMark2 <- UFODataMark2 %>% #rename the column headings
  rename(
    sighting_date = datetime,
    recording_date = date_posted,
  )%>%
  mutate(report_delay = difftime(recording_date,sighting_date,units = "days"))
# the resulting difference in times are reported in days. Some negative times 
# are less than a day, but may just be a result of the sighting time having hours 
# and seconds, while the reporting time is only the date

TimeTravellers <- which(UFODataMark2$report_delay<=-1) 
#determine the rows where the recording of a UFO preceded the sighting
UFODataMark2 <- UFODataMark2[-TimeTravellers,] #remove time travellers

# Create a table for each country's average report delay

UFOReportingDelayMark2 <- UFODataMark2%>%
  group_by(country) %>% # groups by the countries, this does not change the 
                        # frontend output but modifies the backend
  summarize(mean_report_delay =mean(report_delay, na.rm = T)) 
# you need na.rm=T for the average because in row 10193, 
# the United States only has a reporting date

EmptyCountries <- which(is.na(UFOReportingDelayMark2)) 
#determine which rows have missing countries
UFOReportingDelayMark2 <- UFOReportingDelayMark2[-EmptyCountries,] 
#remove the missing countries from the data frame

UFOReportingDelayMark2 <- arrange(UFOReportingDelayMark2, desc(mean_report_delay)) 
#arrange the data frame from longest reporting delay to shortest reporting delay

# Plotting UFO sighting delays
par(mar=c(9,4,4,1)) #set the margins to fit the country names
barplot(as.numeric(unlist(UFOReportingDelayMark2[,2])), names.arg=unlist(UFOReportingDelayMark2[,1]),las=2,
        ylab = "Days From Sighting",
        main = "UFO Reporting Delay for 112 Countries & Territories")
# Note to reviewer, since 112 countries have been identified by the dataset, 
# not all of the countries show up

par(mar=c(7,4,4,1)) #set the margins to fit the country names
barplot(as.numeric(unlist(UFOReportingDelayMark2[1:20,2])), names.arg=unlist(UFOReportingDelayMark2[1:20,1]),las=2,
        ylab = "Days From Sighting",
        main = "Top 20 Worst Countries for Prompt UFO Reporting")

par(mar=c(14,4,4,1)) #set the margins to fit the country names
barplot(as.numeric(unlist(UFOReportingDelayMark2[85:105,2])), names.arg=unlist(UFOReportingDelayMark2[85:105,1]),las=2,
        ylab = "Days From Sighting",
        main = "Top 20 Best Countries for Prompt UFO Reporting")


