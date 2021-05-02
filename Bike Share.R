library(tidyverse)
library(lubridate) 
library(ggplot2)  
getwd() 


# COLLECTING DATA (2020 - March 2021)


oct_2020 <- read_csv("202010-divvy-tripdata.csv")
nov_2020 <- read_csv("202011-divvy-tripdata.csv")
dec_2020 <- read_csv("202012-divvy-tripdata.csv")
jan_2021 <- read_csv("202101-divvy-tripdata.csv")
feb_2021 <- read_csv("202102-divvy-tripdata.csv")
mar_2021 <- read_csv("202103-divvy-tripdata.csv")




#================================================================
# STEP 2: DATA WRANGING AND COMBINING INTO A SINGLE FILE
#================================================================


dec_2020$start_station_id = as.double(dec_2020$start_station_id)
jan_2021$start_station_id = as.double(jan_2021$start_station_id)
feb_2021$start_station_id = as.double(feb_2021$start_station_id)
mar_2021$start_station_id = as.double(mar_2021$start_station_id)


dec_2020$end_station_id = as.double(dec_2020$end_station_id)
jan_2021$end_station_id = as.double(jan_2021$end_station_id)
feb_2021$end_station_id = as.double(feb_2021$end_station_id)
mar_2021$end_station_id = as.double(mar_2021$end_station_id)
                   

alltrips <- bind_rows(oct_2020,nov_2020,
                      dec_2020, jan_2021, feb_2021, mar_2021)

alltrips <-  mutate(alltrips, ride_id = as.character(ride_id))
                   
view(alltrips)
 
alltrips$ID <- seq.int(nrow(alltrips))

view(alltrips)

colnames(alltrips)  #List of column names
nrow(alltrips)  #rows are in data frame?
dim(alltrips)  #Dimensions of the data frame
summary(alltrips)  #Statistical summary of data

#=================================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#=================================================================

# The data can only be aggregated at the ride-level, which is too granular. We will want to add some additional columns of data -- such as day, month, year -- that provide additional opportunities to aggregate the data.
# Create calculated field for length of ride for analysis
# There are some rides where tripduration shows up as negative, including several hundred rides where Divvy took bikes out of circulation for Quality Control reasons. We will want to delete these rides.



# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year, before completing these 
# operations we could only aggregate at the ride level
alltrips$date <- as.Date(alltrips$started_at) 
alltrips$month <- format(as.Date(alltrips$date), "%m")
alltrips$day <- format(as.Date(alltrips$date), "%d")
alltrips$year <- format(as.Date(alltrips$date), "%Y")
alltrips$day_of_week <- format(as.Date(alltrips$date), "%A")

# Add a "ride_length" calculation to all_trips (in seconds)
alltrips$ride_length <- difftime(alltrips$ended_at,alltrips$started_at)

# Inspect the structure of the columns
str(alltrips)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(alltrips$ride_length)
alltrips$ride_length <- as.numeric(as.character(alltrips$ride_length))
is.numeric(alltrips$ride_length)



# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for
# quality by Divvy or ride_length was negative
# removing missing values
# We will create a new version of the dataframe since data is being removed
all_trips <- alltrips[!(alltrips$start_station_name == "HQ QR" | alltrips$ride_length<0),]
all_trips <- na.omit(all_trips)
view(all_trips)
nrow(all_trips)


#=====================================
#DESCRIPTIVE ANALYSIS
#=====================================

summary(all_trips$ride_length)

# Compare members and casual users
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN = mean)
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN = median)
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN = max)
aggregate(all_trips$ride_length ~ all_tripsmember_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips$ride_length ~ all_trips$member_casual + all_trips$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_trips$day_of_week <- ordered(all_trips$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# average ride time by each day for members vs casual users
aggregate(all_trips$ride_length ~ all_trips$member_casual + all_trips$day_of_week, FUN = mean)


write.csv(Agg_data,"C:/Users/sanja/Desktop/Bike_Share/Agg_data.csv", row.names = FALSE)


