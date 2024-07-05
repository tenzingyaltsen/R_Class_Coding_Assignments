#Tidyverse Examples

# "data()" shows what datasets are installed and loaded to r

library(dplyr)

mydata <- datasets::CO2
?CO2

arrange(mydata, conc) #this reorders data, in this case, mydata is arranged by the Plant variable, but now it is arranged by concentration

mydata %>% #this is a Magritte pipe operator, that "pipes" one object into the net line
  arrange(conc) %>% #by putting the arrange inside the pipe, it removes the need to specify the dataset
  head(n=7) #by putting another pipe at the end of the above line, it again removes the need to specify the dataset
  # "mydata" was piped into the arrange function, the resorted output of which was piped into the head function
  # if you wanted to count the number of rows, and added a pipe operator, it wouldn't give the count of mydata, but the head, outputting 7 instead of 84

mynewdata <- mydata %>%
  arrange(desc(conc)) # order by concentration, descending. This is similar to the sort funcytion, but faster

mydata %>%
  arrange(Plant, desc(conc)) #this first arranges the data based on teh ordered factor Plant, and then within the ordered sections arrange them by descending Concentration

# madeup = 1.1*uptake only for chilled samples
mynewdata <- mydata %>%
  mutate(madeup = uptake) %>%
  filter(Treatment == "chilled") %>% #this filers for the chilled samples
  mutate(madeup = uptake*1.1) #mutate either recalculates an existing column, or adds a new column. Here it creates a new column that is 10% higher than the uptake column
  #OH NO! THIS SIMPLY REMOVES ALL OF THE UNCHILLED SAMPLES

# let's try a "case_when" instead so that data is not dropped
mynewdata <- mydata %>%
  mutate(madeup = case_when( #case_when is like an if then else statement, but is very useful for modifying by conditions
    Treatment == "chilled" ~ uptake*1.1,
    .default = uptake
  ))


# Summary statistics
mynewdata %>%
  group_by(Type) %>% #groups by the type of plants, however this does not change the frontend output but modifies the backend
  summarize(NoOfSamples = n(), stdDev = sd(madeup)) #this outputs how the data is grouped. It then counts the number of samples of teh group, and the standard deviation of the data within each group. You cannot use summarize on non-grouped data.
# at the very end, all of the data has been removed to leave a summarized tibble, and so use summarize as the last step

# Summary statistics on two columns
mynewdata %>%
  group_by(Type, Treatment) %>% #this will group it by two columns
  summarize(NoOfSamples = n(), stdDev = sd(madeup)) #the output is a 4 by 4 tibble where each Type has two rows of Treatment

# add a group-calculated standard deviation to the data table
newdata <- mynewdata %>%
  group_by(Type, Treatment) %>% #create buckets
  mutate(StdDev = sd(madeup)) %>% #since it is already grouped, you don't need to use a case_when. Since you're applying the same function to all the data, groups are more helpful, wheras previously we were applying different functions to different groups (hence case_when)
  ungroup() # ungroup so that the buckets are destroyed

#selecting columns and renaming
newdata %>%
  select(Treatment,
         Type,
         Plant,
         MagicNumber = StdDev) #to rename, put the new name first and the old name second

  