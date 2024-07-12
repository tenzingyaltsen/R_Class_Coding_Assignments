#' The dataset "weather.rds"used in this demo came from this excellent course 
#' in data cleaning hosted on Datacamp (https://www.datacamp.com/courses/cleaning-data-in-r)
#' Downloaded in May 2021

library("dplyr")
library('tidyr')

# Load weather.rds
weather <- readRDS("weather.rds") #if this file was in a higher level than the working directory you would need to use "../" to go up

# Verify that weather is a data.frame
class(weather)

# Check the dimensions of the data set
dim(weather)

# View the column names
names(weather) #there are no leading or lagging spaces

# View the structure of the data
str(weather)

# Look at the structure using dplyr's glimpse()
glimpse(weather)

# View a summary of the data
summary(weather) #not terribly useful in this case because rows X1 to X31 are all characters

# Take a closer look at the data by viewing the top and bottom rows
head(weather)
tail(weather)

#' This weather dataset suffers from one of the five most common 
#' symptoms of messy data: column names are values. 
#' Column names X1-X31 represent days of the month. 
#' They should be values of a variable called day.
#' pivot_longer() the columns X1:X31 as day

w2 <- pivot_longer(weather, X1:X31, names_to = "day", values_to = "value") #pivot_longer takes a dataset and rotates it to make it longer. In this case it has turned all of the columns of days into a single column listing the day
#Remove X's from day column
w2$day <- stringr::str_replace(w2$day, "X","")

#' Another common symptom of messy data: values are variable names. 
#' Values in the measure column should be variables (column names) 
#' in our dataset. pivot_wider() the measure column
w3 <- pivot_wider(w2, year:day, names_from = measure, values_from = value) #The complement to pivot_longer, pivot_wider takes a set of variables within their narrow column and expands them into separate column names
# in pivot_wider, you enter the dataset, the columns you want to use to reorder the new columns (id_cols), the column you want to expand (id_expand), what you should be using for column headings (names_from), and what should be the values inside the new column's rows (values_from)

# Check to see if I can safely remove rows where temp is null
# checking to see if only invalid dates had null temperatures
print(w3 %>% filter(is.na(Max.TemperatureF)), n=40) #the n=40 tells you to print 40 lines, but this isn't neccesary, especially because there's only 37 lines

# Remove the empty data rows as found by the above ^
w4 <- w3 %>% filter(!is.na(Max.TemperatureF))

#Unite the year, month and day columns 
# equivalent to paste all three into a new column and
# drop the old columns
w5 <- unite(w4, obs_date, year, month, day, sep = "-")

# Convert obs_date into date
w5$obs_date <- lubridate::ymd(w5$obs_date)

# I am thinking about imputing some value for T(race) amounts
# of precipitation. Let's see what the range looks like first
summary(w5)
# Didn't work with strings! Let,s try to see the unique vales
w5 %>% select(PrecipitationIn) %>% unique() #use the unique() function along with the pipe to look at the unique values
w5 <- w5 %>% arrange(PrecipitationIn)
head(w5$PrecipitationIn)
tail(w5$PrecipitationIn) #shows that there are non-numeric values. When sorting character data to see if you can convert it to numbers, punctuation will be at the head, and alphabetical characters will be in the tail after the numeric characters

# Trace amounts should be between 0.00 and 0.01
# having no experts around, ı arbitrarily pick 0.001
w6 <- w5 %>%
  mutate(PrecipitationIn = case_when(
    PrecipitationIn == 'T' ~ "0.001", #since the data is still in character form, "T" is replaced with the character "0.001" and not the number, 0.001
    .default = PrecipitationIn))
# Alternatively can use str_replace because it is a string column!
# w5$PrecipitationIn <- stringr::str_replace(w5$PrecipitationIn, "T", "0.001")


# Convert character columns into numeric
w7 <- mutate_each(w6, funs(as.numeric), Max.TemperatureF:CloudCover) #mutate_each is a multi-column mutate function, and in this case turns everything from Max.TemperatureF to CloudCover to numeric values
