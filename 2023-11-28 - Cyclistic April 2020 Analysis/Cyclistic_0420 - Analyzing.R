#Cyclistic April 2020 Analysis
#Date: 03/12/23 18:47PM
#By: Mik PE
#Dataset: Cyclistic (from Google Data Analytics Certificate)
#Script: Descriptive Analysis

#Analyse total, mean, media, min and max
summarise(Cyclistic_0420 %>%  group_by(usertype),
          total_ride_length = sum(ride_length),
          mean_ride_length = mean(ride_length),
          median_ride_length = median(ride_length),
          min_ride_length = min(ride_length),
          max_ride_length = max(ride_length))

#Analyse differences in mean by usertype and Day in Week 
aggregate(Cyclistic_0420$ride_length ~ Cyclistic_0420$usertype + Cyclistic_0420$Day_in_Week, FUN = mean)

#Reorder Day_in_Week
Cyclistic_0420$Day_in_Week <- ordered(Cyclistic_0420$Day_in_Week, levels=c("Sunday",
                                                                           "Monday", 
                                                                           "Tuesday", 
                                                                           "Wednesday", 
                                                                           "Thursday", 
                                                                           "Friday", 
                                                                           "Saturday"))

# Analyze ridership data by type and weekday
Cyclistic_0420 %>% mutate(weekday = wday(start_time,label = TRUE)) %>% 
  group_by(usertype, weekday) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(usertype, weekday)

# Turned off scientific notation so that the entire number is displayed on the ggplot for numerical distribution
options(scipen=999)
