library(tidyverse)

flights <- read_csv('flights2022-h2.csv')
head(flights)


airlines <- read_csv('airlines.csv')
head(airlines)


airports <- read_csv('airports.csv')
head(airports)

# Join the flights, airlines, and airports data frames together
complex_join <- flights %>%
  left_join(airlines, by = 'carrier') %>%
  rename(airline_name = name) %>%
  left_join(airports, by = c('dest' = 'faa')) %>%
  rename(airports_name = name)

head(complex_join)

''' You’re joining the airlines table to flights,
using the common column carrier as the key.

flights$carrier = airline code (e.g., "AA", "UA", "DL")

airlines$carrier = same codes

airlines$name = full airline name (e.g., "American Airlines")

After this line, your table will have a new name column from the airlines table.
rename(airline_name = name) %>%
You rename the name column (which came from airlines) to airline_name
→ so it’s clear which “name” it refers to (since you’ll join airports next, which also has a name column).

left_join(airports, by = c("dest" = "faa")) %>%
Now you join another dataset — airports —
matching:

flights$dest → destination airport code (e.g., "LAX")

airports$faa → same airport code

So this adds airport information (like name, location, lat/long) to each flight record.

rename(airport_name = name)
✏️ The airports table also has a name column (e.g., “Los Angeles Intl”).
You rename it to airport_name for clarity.


'''
  

# find flight duration in hours

transformed_data <- complex_join %>% 
  mutate(flight_duration = air_time/60)

head(transformed_data)

# Determine the average flight duration and number of flights for each airline and airport combination

analysis_result <- transformed_data %>%
  group_by(airline_name, airports_name) %>%
  summarize(avg_flight_duration = mean(flight_duration, na.rm =TRUE),
            count = n()) %>%
  ungroup()

head(analysis_result)

''' 
1️⃣ transformed_data %>%

This starts a pipeline using the pipe operator (%>%).

It passes your transformed_data dataframe through the following operations.

2️⃣ group_by(airline_name, airport_name)

This groups the data by each combination of airline_name and airport_name.

Meaning: every unique airline–airport pair becomes one group.

3️⃣ summarize(...)

This collapses each group into one summarized row using the following calculations:

avg_flight_duration = mean(flight_duration, na.rm = TRUE)
→ Calculates the average flight duration per group.
→ na.rm = TRUE means missing (NA) values are ignored.

count = n()
→ Counts how many flights (rows) there are in each group.

4️⃣ ungroup()

This removes the grouping, so the resulting dataframe (analysis_result) is a normal tibble again.

Without ungroup(), future operations might still behave as if the data is grouped.


'''
# From which airline and to which city do the most flights from NYC go to?

frequent_analysis <- analysis_result %>% arrange(desc(count)) %>% head(1)
head(frequent_analysis)


# Which airline and to which airport has the longest average flight duration (in hours) from NYC?
longest_flight <- analysis_result %>% arrange(desc(avg_flight_duration)) %>% head(1)
head(longest_flight)


# What was the least common destination airport departing from JFK?
transformed_data %>%
  filter(origin == 'JFK') %>%
  group_by(airports_name) %>%
  summarize(count = n()) %>%
  arrange(count)

'''
1️⃣ transformed_data %>%

The %>% is the pipe operator, which passes the result of one operation as input to the next.

transformed_data is your starting dataframe.

2️⃣ filter(origin == "JFK")

Keeps only the rows where the origin column equals "JFK".

Essentially, you’re focusing on flights that started from JFK airport.

3️⃣ group_by(airport_name)

Groups the filtered data by airport_name.

After this, any summarization will be calculated per airport.

4️⃣ summarize(count = n())

Creates a new column called count that counts the number of rows in each group (i.e., flights per airport).

n() is a special function in dplyr that returns the number of rows in the current group.

5️⃣ arrange(count)

Orders the summarized results in ascending order of count.

Airports with the fewest flights from JFK will appear at the top.




'''
  
least <- "Eagle County Regional Airport"






