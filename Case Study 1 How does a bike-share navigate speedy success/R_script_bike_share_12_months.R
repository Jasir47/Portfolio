

library(readr)
library(tidyverse) 
library(lubridate)  
library(ggplot2) 
getwd()
setwd("C:/Users/jasir/Jasir Docs/work/Google Data Analytics/case-study/projects/Case Study 1 How does a bike-share navigate speedy success/Data- Motivate International Inc/last 12 months")
getwd()

#=====================
# STEP 1: COLLECT DATA
#=====================

data2 <- read_csv("2022-10.csv")
data3 <- read_csv("2022-11.csv")
data4 <- read_csv("2022-12.csv")
data5 <- read_csv("2023-01.csv")
data6 <- read_csv("2023-02.csv")
data7 <- read_csv("2023-03.csv")
data8 <- read_csv("2023-04.csv")
data9 <- read_csv("2023-05.csv")
data10 <- read_csv("2022-06.csv")
data11 <- read_csv("2022-07.csv")
data12 <- read_csv("2022-08.csv")
data13 <- read_csv("2022-09.csv")

#=====================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#=====================

colnames(data12)
colnames(data13)

str(data2)
str(data3)

# Stack individual data frames into one big data frame

all_trips <- bind_rows(data2,data3,data4,data5,data6,data7,data8,data9,data10,data11,data12,data13)
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng )) 
all_trips <- all_trips[,-12]


#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================

colnames(all_trips)
nrow(all_trips)
dim(all_trips)
head(all_trips)
str(all_trips)
summary(all_trips)

# Add columns that list the date, month, day, and year of each ride

all_trips$date <- as.Date(all_trips$started_at,"%d/%m/%Y")
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Add a "ride_length_sec" calculation to all_trips (in seconds)

all_trips$ride_length_sec <- as.numeric(all_trips$ride_length,units = "secs")
is.numeric(all_trips$ride_length_sec)

# Remove "bad" data

all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
all_trips_v3 <- all_trips_v2 %>%
  filter(started_at !='NA' )

#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================

str(all_trips_v3)
all_trips_v3 %>%
  summarize( avg = mean(ride_length_sec) , 
             Max = max(ride_length_sec),
             Min = min(ride_length_sec),
             Median= median(ride_length_sec))
summary(all_trips_v3)

# Compare members and casual users

aggregate(all_trips_v3$ride_length_sec ~ all_trips_v3$member_casual, FUN = mean)
aggregate(all_trips_v3$ride_length_sec ~ all_trips_v3$member_casual, FUN = median)
aggregate(all_trips_v3$ride_length_sec ~ all_trips_v3$member_casual, FUN = max)
aggregate(all_trips_v3$ride_length_sec ~ all_trips_v3$member_casual, FUN = min)

aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual + all_trips_v3$day_of_week, FUN = mean)

# days of the week are out of order

all_trips_v3$day_of_week <- ordered(all_trips_v3$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# average ride time by each day for members vs casual users

aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual + all_trips_v3$day_of_week, FUN = mean)


all_trips_v3 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%                  #groups by user type and weekday
  summarise(number_of_rides = n()						          	#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length_sec)) %>% 	# calculates the average duration
  arrange(member_casual, weekday)	                      # sorts

# Let's visualize the number of rides by rider type

all_trips_v3 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length_sec)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration

all_trips_v3 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length_sec)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")


#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================


counts <- aggregate(all_trips_v3$ride_length_sec ~ all_trips_v3$member_casual + all_trips_v3$day_of_week, FUN = mean)
write.csv(counts, "summary_v3.csv", row.names=TRUE)





