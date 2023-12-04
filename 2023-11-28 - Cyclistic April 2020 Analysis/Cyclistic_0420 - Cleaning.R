#Cyclistic April 2020 Analysis
#Date: 28/11/23 11:58PM
#By: Mik PE
#Dataset: Cyclistic (from Google Data Analytics Certificate)
#Script: Cleaning

######################################
# Step 1: Install packages           #
######################################

#1.1 Install required packages and libraries
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



######################################
# Step 2: Upload dataset             #
######################################

#2.1: Rename column names in Excel .csv file for consistency purposes

#2.2: Upload data from drive
Cyclistic_0420 <- read_csv("D:\\R Projects\\Cyclistic Data 2022\\202004-divvy-tripdata.csv")
View(Cyclistic_0420)

#2.2: Inspect data frame
glimpse(Cyclistic_0420)
str(Cyclistic_0420)
nrow(Cyclistic_0420)
dim(Cyclistic_0420)
row.names(Cyclistic_0420$usertype)
summarise(Cyclistic_0420 %>% 
            group_by(usertype), 
          count = n())

#2.3: Inspect usertype is consistent
table(Cyclistic_0420$usertype)


######################################
# Step 3: Process Data               #
######################################

#3.1: Remove lat, long fields
Cyclistic_0420 <- Cyclistic_0420 %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))

drop_na(Cyclistic_0420, usertype)
summary(Cyclistic_0420)


#3.2: Format Dates
Cyclistic_0420$start_time <- as.POSIXlt(Cyclistic_0420$start_time, format = "%d/%m/%Y %H:%M", tz="EST")
Cyclistic_0420$end_time <- as.POSIXlt(Cyclistic_0420$end_time, format = "%d/%m/%Y %H:%M", tz="EST")
Cyclistic_0420$ride_length <- difftime(Cyclistic_0420$end_time, Cyclistic_0420$start_time)
View(Cyclistic_0420) # some ride_length have negative values


#3.3: Assign Date, Day, Month, Year, and Day in Week
Cyclistic_0420$Date <- format(as.Date(Cyclistic_0420$start_time))

Cyclistic_0420$Month <- format(as.Date(Cyclistic_0420$Date), 
                        "%m")
Cyclistic_0420$Day <- format(as.Date(Cyclistic_0420$Date), 
                      "%d")
Cyclistic_0420$Year <- format(as.Date(Cyclistic_0420$Date), 
                       "%Y")
Cyclistic_0420$Day_in_Week <- format(as.Date(Cyclistic_0420$Date), 
                              "%A")
View(Cyclistic_0420)

#3.4: Convert ride_length to numerical data to enable analysis, check format
is.factor(Cyclistic_0420$ride_length)
Cyclistic_0420$ride_length <- as.numeric(as.character(Cyclistic_0420$ride_length))
is.numeric(Cyclistic_0420$ride_length)

#3.5: Remove ride_length < 0
Cyclistic_0420$ride_length <- Cyclistic_0420(!Cyclistic_0420$ride_length < 0))


