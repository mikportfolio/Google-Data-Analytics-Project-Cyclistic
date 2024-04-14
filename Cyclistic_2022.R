#Cyclistic Annual Analysis for the Year 2020
#Date: 03/04/23 01:09AM
#By: Mik PE
#Dataset: Cyclistic (from Google Data Analytics Certificate)

#################################################
# Step 1: Install packages                      #
#################################################

# Install required packages and libraries

#install.packages("tidyverse") # pipes
#install.packages("lubridate") # date formatting
#install.packages("ggplot2")   # ggplot2
#install.packages("scales")    # values in vector or data frame
#install.packages("dplyr")     # readr

#library(tidyverse)
#library(lubridate)
#library(ggplot2)
#library(scales)
#library(dplyr)
#library(zoo)                  # for yearqtr

#################################################
# Step 2: Upload datasets                       #
#################################################

## Rename column names in all csv files for consistency purposes
## Upload data from drive
Trip_Jan22 <- read_csv("D:\\2. Analytics Data Sets\\Cyclistic Data 2022\\tripdata_202201.csv")
Trip_Feb22 <- read_csv("D:\\2. Analytics Data Sets\\Cyclistic Data 2022\\tripdata_202202.csv")
Trip_Mar22 <- read_csv("D:\\2. Analytics Data Sets\\Cyclistic Data 2022\\tripdata_202203.csv")
Trip_Apr22 <- read_csv("D:\\2. Analytics Data Sets\\Cyclistic Data 2022\\tripdata_202204.csv")
Trip_May22 <- read_csv("D:\\2. Analytics Data Sets\\Cyclistic Data 2022\\tripdata_202205.csv")
Trip_Jun22 <- read_csv("D:\\2. Analytics Data Sets\\Cyclistic Data 2022\\tripdata_202206.csv")
Trip_Jul22 <- read_csv("D:\\2. Analytics Data Sets\\Cyclistic Data 2022\\tripdata_202207.csv")
Trip_Aug22 <- read_csv("D:\\2. Analytics Data Sets\\Cyclistic Data 2022\\tripdata_202208.csv")
Trip_Sep22 <- read_csv("D:\\2. Analytics Data Sets\\Cyclistic Data 2022\\tripdata_202209.csv")
Trip_Oct22 <- read_csv("D:\\2. Analytics Data Sets\\Cyclistic Data 2022\\tripdata_202210.csv")
Trip_Nov22 <- read_csv("D:\\2. Analytics Data Sets\\Cyclistic Data 2022\\tripdata_202211.csv")
Trip_Dec22 <- read_csv("D:\\2. Analytics Data Sets\\Cyclistic Data 2022\\tripdata_202212.csv")

colnames(Trip_Jan22)
View(Trip_Jan22)

#################################################
# Step 3: Bind & Inspect dataset                #
#################################################

AllTrips_2022 <- bind_rows(Trip_Jan22,
                           Trip_Feb22,
                           Trip_Mar22,
                           Trip_Apr22,
                           Trip_May22,
                           Trip_Jun22,
                           Trip_Jul22,
                           Trip_Aug22,
                           Trip_Sep22,
                           Trip_Oct22,
                           Trip_Nov22,
                           Trip_Dec22)

## Inspect data frame
glimpse(AllTrips_2022)
str(AllTrips_2022)
nrow(AllTrips_2022)
dim(AllTrips_2022)
summarise(AllTrips_2022 %>% 
            group_by(usertype),
          count = n())

#################################################
# Step 3: Standardize & Clean Data              #
#################################################

## Data needed: standardization (username variables), remove unnecessary data (start_lat, start_lng, end_lat, end_lng), time (day, month, year) 

## Inspect consistency of "usertype" field & standardize
AllTrips_2022 <-  AllTrips_2022 %>%
  mutate(usertype = recode(usertype,
                           "member" = "Subscriber",
                           "casual" = "Customer"))
table(AllTrips_2022$usertype)
summary(AllTrips_2022)

## Remove lat, long fields
AllTrips_2022 <- AllTrips_2022 %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng))

## Remove usertype "NA"
drop_na(AllTrips_2022, usertype)
summary(AllTrips_2022)

#################################################
# Step 4: Insert Time Frames                    #
#################################################

## Add field for day, month, and year for each ride
AllTrips_2022$Date <- as.Date(AllTrips_2022$start_time, format = "%d/%m/%Y")                       # Standardize Date
View(AllTrips_2022)
AllTrips_2022 <- AllTrips_2022 %>%
  dplyr::mutate(Year = lubridate::year(Date),                           # Input Year
                Month = lubridate::month(Date),                         # Input Month
                Day = lubridate::day(Date))                             # Input Day

AllTrips_2022$Day_in_Week <- weekdays(AllTrips_2022$Date)               # Input Day of Week
AllTrips_2022$WeekNumber <- strftime(AllTrips_2022$Date, 
                                     format = "%V")                     # Input Week

AllTrips_2022$Quarter <- as.yearqtr(AllTrips_2022$Date, 
                                    format = "%Y-%m-%d")                # Input Quarter

#################################################
# Step 5: Calculate & Clean Ride Length         #
#################################################

## Convert start_time and end_time from character format into POSIXlt format
AllTrips_2022$start_time <- as.POSIXlt(AllTrips_2022$start_time, 
                                       format = "%d/%m/%Y %H:%M", 
                                       tz="EST")
AllTrips_2022$end_time <- as.POSIXlt(AllTrips_2022$end_time, 
                                     format = "%d/%m/%Y %H:%M", 
                                     tz="EST")

## Add a "ride_length" calculation to all_trips (in seconds)
AllTrips_2022$ride_length <- difftime(AllTrips_2022$end_time, 
                                      AllTrips_2022$start_time)

View(AllTrips_2022) # some ride_length have negative values
str(AllTrips_2022)

## Convert ride_length to numerical data to enable analysis, check format
is.factor(AllTrips_2022$ride_length)
AllTrips_2022$ride_length <- as.numeric(as.character(AllTrips_2022$ride_length))
is.numeric(AllTrips_2022$ride_length)

AllTrips_2022_V2 <- AllTrips_2022[(AllTrips_2022$ride_length > 0),]                   # Remove ride_length < 0
AllTrips_2022_V2 <- AllTrips_2022_V2[!with(AllTrips_2022_V2, 
                                           is.na(AllTrips_2022_V2$from_station_name) & is.na(AllTrips_2022_V2$from_station_name)),]
#AllTrips_2022_V2 <- drop_na(AllTrips_2022, from_station_name)                        # Remove entries NA stations
#AllTrips_2022_V2 <- AllTrips_2022_V2[!(AllTrips_2022$from_station_name == "NA")]     # Remove entries for bikes were taken out of docks (HQ)
                           
#################################################
# Step 6: Conduct Descriptive Analysis          #
#################################################

## Analyze mean, median, max and min ride_length
mean(AllTrips_2022_V2$ride_length)
median(AllTrips_2022_V2$ride_length)  
max(AllTrips_2022_V2$ride_length)
min(AllTrips_2022_V2$ride_length)
summary(AllTrips_2022_V2$ride_length)

## Analyse total, mean, media, min and max by usertype
summarise(AllTrips_2022_V2 %>%  group_by(usertype),
          total_ride_length = sum(ride_length),
          mean_ride_length = mean(ride_length),
          median_ride_length = median(ride_length),
          max_ride_length = max(ride_length),
          min_ride_length = min(ride_length))

## Reorder Day_in_Week from Sunday to Saturday
AllTrips_2022_V2$Day_in_Week <- ordered(AllTrips_2022_V2$Day_in_Week, levels=c("Sunday",
                                                                               "Monday", 
                                                                               "Tuesday", 
                                                                               "Wednesday", 
                                                                               "Thursday", 
                                                                               "Friday", 
                                                                               "Saturday"))

## The average ride time by each day for subscriber vs customer users
aggregate (AllTrips_2022_V2$ride_length ~ AllTrips_2022_V2$usertype + 
             AllTrips_2022_V2$Day_in_Week, 
           FUN = mean)

## Analyze ridership data by usertype and weekday
AllTrips_2022_V2 %>% 
  group_by(usertype, Day_in_Week) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(usertype, Day_in_Week)

## Analyse Top 10 station by number of rides booked for Subscriber
AllTrips_2022_V2 %>% 
  filter(usertype == "Subscriber") %>% 
  group_by(from_station_name) %>% 
  summarise(number_of_rides = n(), 
            avg_ride_length = mean(ride_length), 
            avg_ride_length_min = (mean(ride_length)/60)) %>% 
  arrange(desc(number_of_rides)) %>% 
  head(10)

## Analyse Top 10 station by number of rides booked for Customer 
AllTrips_2022_V2 %>% 
  filter(usertype == "Customer") %>% 
  group_by(from_station_name) %>% 
  summarise(number_of_rides = n(), 
            avg_ride_length = mean(ride_length), 
            avg_ride_length_min = (mean(ride_length)/60)) %>% 
  arrange(desc(number_of_rides)) %>% 
  head(10)


## Note: The scientific notation is turned off to display entire integers on the ggplot for numerical distribution
options(scipen=999)


#################################################
# Step 6: Visualizations                        #
#################################################

## Q1: The Number of Rides by Usertype in a Week 
AllTrips_2022_V2 %>% 
  group_by(usertype, Day_in_Week) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length)) %>% 
  arrange(usertype, Day_in_Week) %>% 
  ggplot(aes(x = Day_in_Week,
             y = number_of_rides, 
             fill = usertype)) + 
  geom_col(position = "dodge") + 
  labs(title="Total Number of Rides by UserType in A Week", 
       x = "Week Day", 
       y = "Number of Rides") + 
  theme(axis.text.x = element_text(angle = 60, 
                                   hjust = 1))


## Q2: Total Number of Rides by Customer and Subscribers 
ggplot(AllTrips_2022_V2, aes(x = fct_infreq(usertype), 
                           fill = usertype)) + 
  geom_bar(width = 0.5)+
  guides(fill = FALSE) +
  labs(x = NULL, 
       y = "Number of rides", 
       title = "Total rides of Subscribers vs Customers",
       subtitle = "Data taken from 2022 Report") +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))


## Q3: Average Duration by Usertype
AllTrips_2022_V2 %>% 
  group_by(usertype, Day_in_Week) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length)) %>%  
  arrange(usertype, Day_in_Week) %>% 
  ggplot(aes(x = Day_in_Week, 
             y = average_duration, 
             fill = usertype)) + 
  geom_col(position = "dodge") + 
  labs(title="Average Duration by UserType", 
       x = "Week Day", 
       y = "Average Duration") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

## Q3: The Number of Rides by Bike Type
AllTrips_2022_V2 %>% 
  group_by(bike_type, usertype) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length)) %>%  
  arrange(bike_type, usertype) %>% 
  ggplot(aes(x = bike_type, 
             y = number_of_rides, 
             fill = usertype)) + 
  geom_col(position = "dodge") + 
  labs(title="Total Number of Rides by BikeId", 
       x = "Week Day", 
       y = "Number of Rides") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
## Q4: Average Ride Duration For Each User Type For One Year
AllTrips_2022_V2 %>% 
  group_by(usertype, Month) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length)) %>%  
  arrange(usertype, Month) %>% 
  ggplot(aes(x = Month, 
             y = average_duration, 
             group = usertype)) + 
  geom_line(aes(color = usertype)) + 
  geom_point() + 
  scale_x_continuous(breaks = pretty_breaks(12)) +
  labs(title = "Average Ride Duration For Each User Type In One Year", 
       x = "Month", 
       y = "Average Duration (sec)", 
       color = "User Type")


## Q5: Number Of Rides For Each User Type For One Year 
AllTrips_2022_V2 %>% 
  group_by(usertype, Month) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length)) %>%  
  arrange(usertype, Month) %>% 
  ggplot(aes(x = Month, 
             y = number_of_rides, 
             group = usertype)) + 
  geom_line(aes(color = usertype)) + 
  geom_point() + 
  scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-4)) +
  scale_x_continuous(breaks = pretty_breaks(12)) +
  labs(title = "Number of Rides For Each User Type In One Year", 
       x = "Month", 
       y = "Number of Rides", 
       color = "User Type")


## Q6: Top 20 station by number of rides booked by Subscribers 
AllTrips_2022_V2 %>% 
  filter(usertype == "Subscriber") %>% 
  group_by(from_station_name) %>% 
  summarise(number_of_rides = n(), 
            avg_ride_length = mean(ride_length), 
            avg_ride_length_min = mean(ride_length)/60) %>%  
  arrange(-number_of_rides) %>% 
  head(20)

## Q7: Top 20 station by number of rides booked by Customers 
AllTrips_2022_V2 %>% 
  filter(usertype == "Customer") %>% 
  group_by(from_station_name) %>% 
  summarise(number_of_rides = n(), 
            avg_ride_length = mean(ride_length), 
            avg_ride_length_min = mean(ride_length)/60) %>%  
  arrange(-number_of_rides) %>% 
  head(20)


#################################################
# Step 7: Export Summary                        #
#################################################
counts <- aggregate(AllTrips_2022_V2$ride_length ~ AllTrips_2022_V2$usertype + AllTrips_2022_V2$Day_in_Week, FUN = mean)

write.csv(counts, file = "C:\\Users\\Mudrik\\Desktop\\Capstone_Project_1.csv", row.names = FALSE)



