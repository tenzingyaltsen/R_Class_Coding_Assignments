# Join Example

athlete <- read.csv("olympic_events.csv")

athlete <- rename(athlete, Country = NOC)

gap <- gapminder::gapminder_unfiltered

# Join gap to athlete

bigdata <- athlete %>% #the first dataset is the left dataset
  inner_join(gap, by=c("Team"="country")) #Since the left table uses "Team" and the right table has a lowercase "country"
 #the problem is that this produces a many-many relationship, where multiple Team names get multiplied by the multiple country names
# this creates an exponentially larger table of mostly meaningless data

# alternative mechanism is to join using multiple variables
betterdata <- athlete %>% 
  left_join(gap, by=c("Team"="country", "Year"="year")) %>% #need to specify that the CAPITALIZED "Year" is teh same as teh LOWERCASE "year"
  # since this is a left join, it ensures that anything that doesn't match the Team names (which are usually countries) and the year the athlete competed in, is cut out. The names of the right dataset are cut out
  group_by(Team) %>%
  summarize(AvgHeight = mean(Height, na.rm = T),
            AvgGDP = mean(gdpPercap, na.rm = T))
 # the result are some Teams that do not have any associated means since their team name wasn't the name of a country, i.e. the Team "30. Februar" has no associated gdp by which to average

# Plot average height versus average gdp
plot(x=betterdata$AvgHeight, y=betterdata$AvgGDP)


