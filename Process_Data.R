# Load libraries
library(tidyverse)
library(lubridate)
library(janitor)

# Set working directory to the data directory
setwd("/home/jeffer/Documents/Data Analysis Portfolio/Cyclistic_Case_Study/Data/")

# Load datasets into dataframes
dt1 <- read_csv("202110-divvy-tripdata.csv")
dt2 <- read_csv("202111-divvy-tripdata.csv")
dt3 <- read_csv("202112-divvy-tripdata.csv")
dt4 <- read_csv("202201-divvy-tripdata.csv")
dt5 <- read_csv("202202-divvy-tripdata.csv")
dt6 <- read_csv("202203-divvy-tripdata.csv")
dt7 <- read_csv("202204-divvy-tripdata.csv")
dt8 <- read_csv("202205-divvy-tripdata.csv")
dt9 <- read_csv("202206-divvy-tripdata.csv")
dt10 <- read_csv("202207-divvy-tripdata.csv")
dt11 <- read_csv("202208-divvy-tripdata.csv")
dt12 <- read_csv("202209-divvy-publictripdata.csv")

# Check whether the set of data.frames are row-bindable.
# Returns TRUE if there are no mis-matching rows (name and data type)
compare_df_cols_same(dt1, dt2, dt3, dt4, dt5, dt6, dt7, dt8, dt9, dt10, dt11, dt12)

# Stack individual data frames into one data frame
all_trips <- bind_rows(dt1, dt2, dt3, dt4, dt5, dt6, dt7, dt8, dt9, dt10, dt11, dt12)

# Clean Data

# Inspect the new table that has been created
colnames(all_trips)  #List of column names
dim(all_trips)  #Dimensions of the data frame (# of rows and columns)
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics


# Verify that categorical variables are consistent and how many fall in each category
# All values in member_casual should be either member or casual
# All values in rideable type should be classic_bike, docked_bike or electric_bike
table(all_trips$member_casual)
table(all_trips$rideable_type)

# Add a Column for trip duration (in seconds)
all_trips$ride_length <- all_trips$ended_at - all_trips$started_at

# Convert ride_length to numeric so we can perform calculations with it
all_trips$ride_length <- as.numeric(all_trips$ride_length)

# Remove invalid data 
# Get summary of the column ride_length
summary(all_trips$ride_length)

# The minimum value of ride_length is negative. 
# See how many rows there are where tip_duration is negative
sum(all_trips$ride_length < 0)

# There are also 115280 rows where ride_length < 60
sum(all_trips$ride_length < 60)

# Remove all rows where trip duration < 60 as they're most likely not real rides
# Create a new data frame since data is being removed
index <- which(all_trips$ride_length < 60)
all_trips_v2 <- all_trips[-c(index),]

# Get a statistical summary of the column ride_length
summary(all_trips_v2$ride_length) 

# Now the minimum value is 60, but it seems to be another problem, the max value
# seems way too high.

# See first 100 rows ordered by highest to lowest trip duration
all_trips_v2 %>%
  arrange(desc(ride_length)) %>%
  print(n = 100)

# All results returned are for casual users and docked_bike
# Get all rows where "rideable_type" is "docked_bike" and user is "member"
all_trips_v2 %>%
  filter(member_casual == "member", rideable_type == "docked_bike") %>%
  arrange(desc(ride_length))

# 0 rows returned. It seems that docked bikes are not included in the subscription
# or for some other reason their rides in docked bikes are recorded as casual users.

# Since this analysis is focused in the differences between members and casual 
# users and there is no distinction between them when using docked bikes it's 
# necessary to remove data about docked bikes
index <- which(all_trips_v2$rideable_type == "docked_bike")
all_trips_v2 <- all_trips_v2[-c(index),]
table(all_trips_v2$rideable_type)

# Add columns that list the date, month, day, and year of each ride
# This will allow aggregation of ride data for each hour, day, month or year
# Before this it was only possible to aggregate at the ride level
all_trips_v2$date <- as.Date(all_trips_v2$started_at) #The default format is yyyy-mm-dd
all_trips_v2$month <- format(as.Date(all_trips_v2$date), "%m")
all_trips_v2$day <- format(as.Date(all_trips_v2$date), "%d")
all_trips_v2$year <- format(as.Date(all_trips_v2$date), "%Y")
all_trips_v2$day_of_week <- format(as.Date(all_trips_v2$date), "%A")
all_trips_v2$hour <- format(as.POSIXct(all_trips_v2$started_at), format = "%H")

# Export cleaned dataset to a csv file
write.csv(all_trips_v2, file = "cleaned_data.csv")