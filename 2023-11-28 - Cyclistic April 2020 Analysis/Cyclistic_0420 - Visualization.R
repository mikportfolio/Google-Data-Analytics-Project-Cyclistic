#Cyclistic April 2020 Analysis
#Date: 03/12/23 20:08PM
#By: Mik PE
#Dataset: Cyclistic (from Google Data Analytics Certificate)
#Script: Visualization

#================================================================
# Step 3(A): The number of rides by usertype in a week          #
#================================================================

Cyclistic_0420 %>% 
  mutate(weekday = wday(start_time, 
                        label = TRUE)) %>% 
  group_by(usertype, weekday) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length)) %>%  
  arrange(usertype, weekday) %>% 
  ggplot(aes(x = weekday, 
           y = number_of_rides, 
           fill = usertype)) + 
  geom_col(position = "dodge") + 
  labs(title = "Total Number of Rides by UserType in A Week", 
       x = "Week Day", 
       y = "Number of Rides") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

## Members have more number of rides than casual riders consistently throughout the week.
## Casuals are less likely to ride bikes in comparison to subscribers from Mondays to Fridays. 
## Casuals are more likely to bike rides on Saturdays and Sundays.


#================================================================
# Step 3(A): Average duration by usertype by Week Day           #
#================================================================

Cyclistic_0420 %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>%  
  group_by(usertype, weekday) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length)) %>% 
  arrange(usertype, weekday) %>% 
  ggplot(aes(x = weekday, 
             y = average_duration, 
             fill = usertype)) + 
  geom_col(position = "dodge")+ 
  labs(title = "Average Duration by UserType", 
       x = "Week Day", 
       y = "Average Duration") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

#  Accoding to this graph,
## The average duration during the week is higher for customers in comparison to subscribers.


#================================================================
# Step 3(A): The number of rides by Bike Id                     #
#================================================================

Cyclistic_0420 %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>% 
  group_by(bike_id, 
           usertype) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length)) %>% 
  arrange(bike_id, usertype) %>% 
  ggplot(aes(x = bike_id, 
             y = number_of_rides, 
             fill = usertype)) + 
  geom_col(position = "dodge") + 
  labs(title = "Total Number of Rides by BikeId", 
       x = "Week Day", 
       y = "Number of Rides") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

## Members and and casual riders uses docked bikes only.
## Members uses the docked bikes more often.

#==============================================================================
# Step 3(A): Average Ride Duration For Each User Type For One Month by Week   #                  #
#==============================================================================

Cyclistic_0420 %>% 
  group_by(usertype, Week) %>%  
  summarise(average_duration = mean(ride_length)) %>%  
  arrange(usertype, Week) %>% 
  ggplot(aes(x = Week, 
             y = average_duration, 
             group = usertype)) + 
  geom_line(aes(color = usertype)) + 
  geom_point() + 
  labs(title = "Average Ride Duration For Each User Type In One Year", 
       x = "Week", 
       y = "Average Duration (sec)", 
       color = "User Type")

View(Cyclistic_0420)

#  For ride duration(s) in April,
## Casual and member riders peak had average duration peak in Week 14 continued a downwards trajectory into Week 18 
## The lowest month for casual and member riders is Week 18.


#================================================================
# Step 3(A): Number Of Rides For Each User Type For One Year    #
#================================================================

Cyclistic_0420 %>% 
  group_by(usertype, Week) %>%  
  summarise(number_of_rides = n()) %>%  
  arrange(usertype, Week) %>% 
  ggplot(aes(x = Week, 
             y = number_of_rides, 
             group = usertype)) + 
  geom_line(aes(color = usertype)) + 
  geom_point() + 
  scale_y_continuous(labels = label_number(suffix = "K", scale = 1e-4)) + 
  labs(title = "Number of Rides For Each User Type In April 2020", 
       x = "Week", 
       y = "Number of Rides",
       color = "User Type")

#  According to this graph, 
## Rides a slight increase in Week 15 and 17, steep decline in Week 18. 
## Both customers and subscribers shared a high peak in Week 15.