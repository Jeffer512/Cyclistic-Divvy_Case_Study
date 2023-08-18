# Load libraries
library(tidyverse)
library(lubridate)
library(leaflet)
library(viridis)
library(nombre)
library(ggpubr)

# Set scipen so plots don't show in scientific notation 
options(scipen = 1000000000)  

# Load dataset
cleaned_data <- read.csv("/home/jeffer/Documents/Data Analysis Portfolio/Cyclistic_Case_Study/Data/cleaned_data.csv")

# Descriptive analysis on ride_length (all figures in seconds)
summary(cleaned_data$ride_length)

# Compare members and casual users
aggregate(cleaned_data$ride_length ~ cleaned_data$member_casual, FUN = mean)

## Summarize by month
df_month <- cleaned_data %>%
  group_by(member_casual, month) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>%
  arrange(month) 

# Create visualization for number of rides by month  
vis_month <- df_month %>% 
  ggplot(aes(reorder(month, (((as.integer(month) + 2) %% 12) + 1)), 
             number_of_rides, 
             fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(x = "Month", y = "Number of Rides") +
  scale_fill_viridis(discrete = T, 
                     name = "Rider Type", labels=c('Casual', 'Member')) + 
  scale_y_continuous(breaks = seq(25000, 450000, 50000)) + 
  geom_vline(xintercept = 3.5) +
  geom_text(aes(x = 3.5, y = 225000,  label = "2022"), 
            angle = 90, size = 7, check_overlap = T, vjust = 1.2) +
  theme_bw()

# Create visualization for average duration of rides by month  
vis_month2 <- df_month %>% 
  ggplot(aes(reorder(month, (((as.integer(month) + 2) %% 12) + 1)), 
             average_duration, 
             fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(x = "Month", y = "Number of Rides") +
  scale_fill_viridis(discrete = T, 
                     name = "Rider Type", labels=c('Casual', 'Member')) + 
  geom_vline(xintercept = 3.5) +
  geom_text(aes(x = 3.5, y = 1450,  label = "2022"), 
            angle = 90, size = 7, check_overlap = T, vjust = 1.2) +
  theme_bw()

# Plot the two visualizations by month together 
ggarrange(vis_month, vis_month2, common.legend = T, legend = "bottom")

## See the average ride time and number of rides by day of week for members vs casual users
cleaned_data %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>%
  arrange(day_of_week)

# The days of the week are out of order. Let's fix that.
# Convert "day_of_week" to ordered vector 
cleaned_data$day_of_week <- wday(cleaned_data$started_at, label = TRUE) 

# Number of rides and average duration by day of week and rider type
df_week <- cleaned_data %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n(),
            average_duration =  mean(ride_length),
            total_duration = sum(ride_length)) %>%
  arrange(day_of_week) %>%
  ungroup() %>%
  mutate(number_of_rides_percent = (number_of_rides / sum(number_of_rides)))

# Create table to use for plot legend
totals <- df_week %>%
  group_by(member_casual) %>%
  summarise(total = sum((number_of_rides_percent)))

# Visualize number of rides by day of week and rider type
vis_week <- df_week %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides_percent, 
             fill = member_casual, color = member_casual)) +
  geom_col(position = "dodge") +
  labs(x ="Day of Week", y = "Total Rides (percentage)") +
  scale_y_continuous(breaks = seq(0.01,0.10, 0.01), 
                     labels = scales::percent_format(scale = 100)) +
  theme_bw() +
  theme(legend.title = element_text(size = 15),
        legend.text = element_text(size = 10)) +
  ggtitle("Number of rides by day of week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_viridis(discrete = T, 
                      name = "Total Rides (%)", 
                      labels = paste(c("Casual:", "Member:"), 
                                     scales::percent(totals$total))) +
  scale_fill_viridis(discrete = T,
                     name = "Rider Type", labels=c('Casual', 'Member'))

# Visualize average duration by day of week and rider type
vis_week2 <- df_week %>% 
  ggplot(aes(x = day_of_week, y = average_duration, 
             fill = member_casual, color = member_casual)) +
  geom_col(position = "dodge") +
  xlab("Day of week") +
  ylab("Average duration (seconds)") +
  theme_bw() +
  ggtitle("Average ride duration by day of week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_viridis(discrete = T, 
                      name = "Total Rides (%)", 
                      labels = paste(c("Casual:", "Member:"), 
                                     scales::percent(totals$total))) +
  scale_fill_viridis(discrete = T,
                     name = "Rider Type", labels=c('Casual', 'Member')) +
  scale_y_continuous(breaks = seq(0, 1500, 250))

# Plot the two visualizations by week together 
ggarrange(vis_week2, vis_week, ncol=2, common.legend = TRUE, legend = "bottom")  

## Number of rides and average trip duration by rider type, hour and day of week
df_hour <- cleaned_data %>%
  group_by(member_casual, day_of_week, hour) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length),
            total_duration= sum(ride_length)) %>%
  arrange(day_of_week, hour) %>%
  ungroup() %>%
  mutate(number_of_rides_percent = (number_of_rides/sum(number_of_rides)))

# Create table to use for plot legend
totals_hour <- df_hour %>%
  filter(!(day_of_week %in% c("Sun","Sat"))) %>%
  group_by(member_casual) %>%
  summarise(total = sum(number_of_rides_percent))

# Create table to use for plot legend
totals_hour2 <- df_hour %>%
  filter(day_of_week %in% c("Sun","Sat")) %>%
  group_by(member_casual) %>%
  summarise(total = sum(number_of_rides_percent))

# Visualize number of rides by user type and hour (Monday to Friday)
vis_hour <- df_hour %>%
  filter(!(day_of_week %in% c("Sun","Sat"))) %>%
  ggplot(aes(x = hour, y = number_of_rides_percent, 
             fill = member_casual, colour = member_casual)) +
  scale_fill_viridis(discrete = T, name = "Rider Type", 
                     labels=c('Casual', 'Member')) +
  scale_color_viridis(discrete = T,name = "Total Rides (%)",
                      labels = paste(c("Casual:", "Member:"), 
                                     scales::percent(totals_hour$total))) +
  labs(x ="Hour", y = "% of Total Week Rides") +
  geom_col(position = "dodge") +
  scale_y_continuous(breaks = seq(0.001,0.10, 0.002), labels = 
                       scales::percent_format(scale = 100)) +
  theme_bw() +
  ggtitle("Number of rides by hour (Monday to Friday)") +
  theme(plot.title = element_text(hjust = 0.5))

# Visualize number of rides by user type and hour (Saturday and Sunday)
vis_hour2 <- df_hour %>%
  filter(day_of_week %in% c("Sun","Sat")) %>%
  ggplot(aes(x = hour, y = number_of_rides_percent, 
             fill = member_casual, color = member_casual)) +
  scale_fill_viridis(discrete = T, name = "Rider Type", 
                     labels=c('Casual', 'Member')) +
  scale_color_viridis(discrete = T,name = "Total Rides (%)",
                      labels = paste(c("Casual:", "Member:"), 
                                     scales::percent(totals_hour2$total))) +
  labs(x ="Hour", y = "% of Total Week Rides") +
  geom_col(position = "dodge") +
  scale_y_continuous(breaks = seq(0.001,0.10, 0.002), labels = 
                       scales::percent_format(scale = 100)) +
  theme_bw() +
  ggtitle("Number of rides by hour (Saturday and Sunday)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill  = guide_legend(order = 1),
         color = guide_legend(order = 2))

# Plot visualizations by hour together
ggarrange(vis_hour, vis_hour2, ncol = 1, common.legend = F, legend = "bottom")

## Number of rides and average duration by date
df_date <- cleaned_data %>%
  group_by(member_casual, date) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length),
            total_duration = sum(ride_length)) %>%
  arrange(date) 

# Statistical summary of some values by rider type
df_date %>%
  group_by(member_casual) %>%
  summarise(average_number_of_rides = mean(number_of_rides),
            max_number_of_rides =  max(number_of_rides),
            median_number_of_rides =  median(number_of_rides),
            min_number_of_rides =  min(number_of_rides),
            average_total_duration = mean(total_duration),
            max_total_duration = max(total_duration),
            min_total_duration = min(total_duration),
            max_average_duration = max(average_duration),
            min_average_duration = min(average_duration),
            median_average_duration = median(average_duration))

# See the days of the year with the highest number of rides
df_date %>%
  arrange(-number_of_rides) %>%
  print(n=100)

# Visualize boxplot for number of rides by rider type
vis_date <- df_date %>%
  ggplot(aes(member_casual, number_of_rides, fill = member_casual)) +
  geom_boxplot(outlier.colour = "blue") +
  scale_fill_viridis(discrete = T, name = "Rider Type",
                     labels=c('Casual', 'Member')) +
  labs(x = "Rider Type", y = "Number of Rides") +
  stat_summary(fun = mean, geom = "point", col = "red") +
  stat_summary(fun = mean, geom = "text", col = "red",  
               vjust = 1.5, aes(label = paste("Mean:", round(..y.., digits = 1)))) +
  scale_y_continuous(breaks = seq(0, 200000, 2500)) +
  theme_bw() 

## Use of different bike types by rider type
bikes <- cleaned_data %>%
  group_by(member_casual, rideable_type) %>%
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length),
            total_duration = sum(ride_length)) %>%
  arrange(member_casual)

# Visualize number of rides by bike type for members
vis_bikes <- bikes %>%
  filter(member_casual == "member") %>%
  mutate(number_of_rides_percent = number_of_rides / sum(number_of_rides)) %>%
  ggplot(aes(x ="", y = number_of_rides, fill = rideable_type)) +
  geom_bar(stat="identity", width = 1) +
  scale_fill_viridis(discrete = T, name = "Bike Type", 
                     labels=c('Classic', 'Electric')) +
  coord_polar("y", start=0) +
  theme_void() +
  geom_text(aes(label = scales::percent(number_of_rides_percent)),
            colour = c("white","black"),
            position = position_stack(vjust = 0.5)) +
  ggtitle("Members") +
  theme(plot.title = element_text(hjust = 0.5)) 

# Visualize number of rides by bike type for casual riders
vis_bikes2 <- bikes %>%
  filter(member_casual == "casual") %>%
  mutate(number_of_rides_percent = number_of_rides / sum(number_of_rides)) %>%
  ggplot(aes(x ="", y = number_of_rides, fill = rideable_type)) +
  geom_bar(stat="identity", width = 1) +
  scale_fill_viridis(discrete = T, name = "Bike Type", 
                     labels=c('Classic', 'Electric')) +
  coord_polar("y", start=0) +
  theme_void() +
  geom_text(aes(label = scales::percent(number_of_rides_percent)),
            colour = c("white","black"),
            position = position_stack(vjust = 0.5)) +
  ggtitle("Casual Users") +
  theme(plot.title = element_text(hjust = 0.5))

# Visualize total use (in seconds) by bike type for members
vis_bikesx <- bikes %>%
  filter(member_casual == "member") %>%
  mutate(total_duration_percent = total_duration / sum(total_duration)) %>%
  ggplot(aes(x ="", y = total_duration, fill = rideable_type)) +
  geom_bar(stat="identity", width = 1) +
  scale_fill_viridis(discrete = T, name = "Bike Type", 
                     labels=c('Classic', 'Electric')) +
  coord_polar("y", start=0) +
  theme_void() +
  geom_text(aes(label = scales::percent(total_duration_percent)),
            colour = c("white","black"),
            position = position_stack(vjust = 0.5)) +
  ggtitle("Members") +
  theme(plot.title = element_text(hjust = 0.5))

# Visualize total use (in seconds) by bike type for casual riders
vis_bikesx2 <- bikes %>%
  filter(member_casual == "casual") %>%
  mutate(total_duration_percent = total_duration / sum(total_duration)) %>%
  ggplot(aes(x ="", y = total_duration, fill = rideable_type)) +
  geom_bar(stat="identity", width = 1) +
  scale_fill_viridis(discrete = T, name = "Bike Type", 
                     labels=c('Classic', 'Electric')) +
  coord_polar("y", start=0) +
  theme_void() +
  geom_text(aes(label = scales::percent(total_duration_percent)),
            colour = c("white","black"),
            position = position_stack(vjust = 0.5)) +
  ggtitle("Casual Users") +
  theme(plot.title = element_text(hjust = 0.5))

# Arrange both visualizations for number of rides by bike together and add a title
p <- ggarrange(vis_bikes, vis_bikes2, ncol = 2, legend = "none") %>%
  annotate_figure(top = text_grob("Number of rides by bike type (%)",
                                  size = 14))

# Arrange both visualizations for total duration by bike together and add a title
p2 <- ggarrange(vis_bikesx, vis_bikesx2, ncol = 2, 
                common.legend = TRUE, legend = "bottom") %>%
  annotate_figure(top = text_grob("Total trip time by bike type (%)",
                                  size = 14))

# Plot visualizations by bike type
ggarrange(p, p2, ncol = 1)

## Visualize most popular routes
routes <- cleaned_data %>%
  mutate(route = paste(start_station_name, "-", end_station_name)) %>%
  group_by(route, member_casual) %>%
  summarise(total = n()) %>%
  arrange(desc(total)) %>%
  ungroup()

# Rows 1 and 2 are "NA - NA" so this rides don't have a start nor end station 
sum(routes$total[c(1,2)]) / nrow(cleaned_data) * 100
# 8.54% of rides don't have a start station name nor end station name

# Remove those 2 rows
routes <- routes[-c(1, 2),]

# Create a column with the sum of the number of rides for members and casual 
# riders by station
routes <- routes %>%
  group_by(route) %>%
  mutate(total_total = sum(total)) %>%
  arrange(-total_total)
  
# Visualize 10 most popular routes by number of rides 
vis_routes <- routes %>%
  ungroup() %>%
  head(20) %>%
  ggplot(aes(y = route,
             x = total, fill = total,
             alpha = member_casual,
             color = member_casual,
             group = member_casual)) +
  geom_col() +
  scale_fill_viridis(discrete = F, begin = 0.20, end = 0.75, direction = -1,
                     name = "Total") +
  scale_color_viridis(option = "D", discrete = T, begin = 0, end = 1,
                      name = "Rider Type", labels=c('Casual', 'Member')) +
  scale_y_discrete(limits = unique(routes$route[seq(20, 1)])) +
  scale_alpha_manual(values=c(0.6, 1), name = "Rider Type",
                     labels=c('Casual', 'Member')) +
  theme_bw() + 
  ggtitle("Most popular routes") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Number of rides", y = "Route")

vis_routes

## Create Map with stations by number of rides

# Create dataframe with the number of rides by station and their locations
map_data <- cleaned_data %>%
  group_by(start_station_name) %>%
  arrange(date) %>%
  summarise(lng = first(start_lng),
            lat = first(start_lat), 
            first_ride = first(date),
            number_of_rides = n()) %>%
  arrange(-number_of_rides) 

map_data

# Remove first column (NA)
map_data <- map_data[-1,]

# Create dataframe with the number of rides by station and their locations 
# including only rides from members. Will be used for labels
map_data_member <- cleaned_data %>%
  filter(member_casual == "member") %>%
  group_by(start_station_name, member_casual) %>%
  arrange(date) %>%
  summarise(lng = first(start_lng),
            lat = first(start_lat), 
            number_of_rides = n()) %>%
  arrange(-number_of_rides) 

# Create dataframe with the number of rides by station and their locations 
# including only rides from casual riders. Will be used for labels
map_data_casual <- cleaned_data %>%
  filter(member_casual == "casual") %>%
  group_by(start_station_name, member_casual) %>%
  arrange(date) %>%
  summarise(lng = first(start_lng),
            lat = first(start_lat), 
            number_of_rides = n()) %>%
  arrange(-number_of_rides) 

# Create labels to be used on the map
labels <- paste(
  "<strong>", map_data$start_station_name, 
  "</strong><br>Number of Rides:", map_data$number_of_rides,
  "</strong><br>Number of Rides (Casual):", scales::percent(
    map_data_casual$number_of_rides[
      match(map_data$start_station_name, map_data_casual$start_station_name)]
    / map_data$number_of_rides, accuracy = 4),
  "</strong><br>Number of Rides (Member):", scales::percent(
    map_data_member$number_of_rides[
      match(map_data$start_station_name, map_data_member$start_station_name)] 
    / map_data$number_of_rides, accuracy = 4),
  "</strong><br>", nom_ord(which(map_data$start_station_name == 
                                   map_data$start_station_name), cardinal = F),
  "Station by number of rides",
  "</strong><br>First Ride in Time Period:", map_data$first_ride
) %>%
  lapply(htmltools::HTML)

# Set domain and color palette viridis. reverse = T to make higher values darker 
pal <- colorNumeric(
  palette = 'viridis',
  domain = seq(10, 64010, 1000),
  reverse = T)

# Create map
map <- leaflet(map_data) %>% 
  setView(lng = -87.623177, lat = 41.881832, zoom = 11) %>%
  addCircleMarkers(radius = ~sqrt(sqrt(number_of_rides)), 
                   label = ~labels,
                   color = ~pal(number_of_rides)) %>% 
  addMarkers(data = head(map_data, 10),
             label = head(labels, 10)) %>%
  addTiles() %>%
  addLegend("topright",
            pal = pal, 
            values = c(seq(10, 64010, 1000), NA),
            title = "Number of Rides",
            na.label = "<10",
            labels = c("blue", "red", "green","pink"),
            opacity = 1,)

map

# There is a warning saying some values will be treated as NA, this is expected.
# NA values will be used to show stations with less than 10 rides with other color.
# NA not showing correctly in the legend
css_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
html_fix <- htmltools::tags$style(type = "text/css", css_fix)  # Convert CSS to HTML
map %>% htmlwidgets::prependContent(html_fix)                  # Insert into leaflet HTML code
