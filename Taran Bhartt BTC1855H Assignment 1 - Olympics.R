# Taran Bhartt BTC1855H Assignment 1 - Olympics
# Dear reviewer, make sure that "olympic_events.csv" is in the working directory

# 1. Read the dataset and load into a dataframe.
olympic <- read.csv("olympic_events.csv", stringsAsFactors = T)
# the class is a data frame
# By reading the strings as factors at inception, various summary and logical
# functions can be completed. Furthermore, several of the strings actually refer
# to categorical variables, such as medal type, or country of origin.


# 2. Print the first few lines of the dataset using the head() function.
head(olympic)


# 3. Find the rows where Age is missing.
MissingAge <- which(is.na(olympic$Age))
# the "which" function when combined with the "is.na" function locates the rows where
# the age is not listed/is NA. The "is.na" function creates a logical vector, which
# the "which" function locates the position of the resulting TRUE values


# 4. Calculate the average age of the athletes and save it as avg_age. Explain
#    how you dealt with missing values.
UniqueOlympic <- olympic[!duplicated(olympic[,c(1,10)]),]
# "duplicated" is a function that identifies subsequent occurrences of a particular
# element within the data.frame. By using the "!" R locates the first occurrence
# of a particular element. The "[1,10]" index specifies that the elements to be
# checked against the "duplicated" function are the ID and Year. The unique elements
# are assigned to a new data frame.
# This is done to prevent Olympians that competed in multiple events within the 
# same year from skewing the year-specific averages by counting their multiple 
# events as if they were separate contestants
avg_age <- aggregate(UniqueOlympic$Age~UniqueOlympic$Year, UniqueOlympic, FUN=mean)
# adding "na.rm = TRUE" is unnecessary with the "aggregate" function
# create a data frame where the average age of contestants for each year of the 
# Olympics is listed. The "FUN=mean" condition specifies that the average should be used to
# create the summary statistics to be assigned to the "avg_age" data frame
# the "~" is called a formula. In this case it defines the relationship between the 
# variables Age and Year. Age is the response variable, and Year is the group variable
colnames(avg_age) <- c("Year", "Average Age")
# The "colnames" function renames the "avg_age" column names to be more accurate


# 5. Print a message that reads “The average age of the athletes is nn”. 
#    Hint: Research print function and see how you can print static text 
#    together with a variable value.
cat("The average age of the athletes is", mean(avg_age[,2]), "years old across all years of the Olympics.")
print("The average age for specific years of the Olympics is as follows:")
print(avg_age)


# 6. Repeat steps 3 and 5 to calculate average height of the athletes and save 
#    it as avg_height.
avg_height <- aggregate(UniqueOlympic$Height~UniqueOlympic$Year, UniqueOlympic, FUN=mean)
# adding "na.rm = TRUE" is unnecessary
colnames(avg_height) <- c("Year", "Average Height") #rename column names
cat("The average height of the athletes is", mean(avg_height[,2]), "cm old across all years of the Olympics.")
print("The average height for specific years of the Olympics is as follows:")
print(avg_height)


# 7. Show the number of male and female athletes in tabular form (remember factors)
Athlete <- olympic[!duplicated(olympic[,1]),]
# "!duplicated" ensures that only the first instance of an athlete ID 
# (as specified by the the "olympic[,1]" indexing) is counted in the new array "Athlete"
summary(Athlete$Sex)
# This summary does not include multiple instances of the same athlete
# returning to the Olympics to compete in different years


# 8. Show the number of athletes per country.
table(Athlete$NOC)
# This summary does not include multiple instances of the same athlete
# returning to the Olympics to compete in different years


# 9. Calculate the number of gold medals per country 
olympic$Medal <- factor(olympic$Medal, levels = c("Gold", "Silver", "Bronze"))
# assigns an order to the ordinal medal values in the "olympic" data frame
CountryMedals <- aggregate(olympic$Medal~olympic$NOC, olympic, FUN=summary)
# create a new data frame containing summaries of the now ordered medals by country
colnames(CountryMedals) <- c("Country", "Medal")
# rename the columns of the data frame "CountryMedals" to be more concise

# If you want to see the countries listed in order of most gold medals won then run the following code
# CountryMedals <- CountryMedals[order(CountryMedals$Medal[,"Gold"], decreasing=TRUE),]

print("The following is the list of medals by type earned by each Country across all Olympics")
CountryMedals


# 10. Once you have submitted your R script, study the Code Review section 
#     and documentation below to familiarize yourself with the review process. 
#     You'll automatically receive the code to review once the submissions are closed.
