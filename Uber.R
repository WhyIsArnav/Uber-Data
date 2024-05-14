library(tidyverse)
library(tidytext)
library(textdata)
library(dplyr)
library(stringr)
library(reshape2)
library(shiny)
library(ggplot2)
library(lubridate)
library(ggthemes)
library(DT)
library(scales)
library(leaflet)
library(leaflet.extras)

setwd('~/Documents/DATA332/Projects/Uber Data')

df1 <- read.csv("uber-raw-data-apr14.csv")
df2 <- read.csv("uber-raw-data-may14.csv")
df3 <- read.csv("uber-raw-data-jun14.csv")
df4 <- read.csv("uber-raw-data-jul14.csv")
df5 <- read.csv("uber-raw-data-aug14.csv")
df6 <- read.csv("uber-raw-data-sep14.csv")

uber <- rbind(df1,df2,df3,df4,df5,df6)

saveRDS(uber, file = "uber_data.rds")

#uber <- readRDS("uber_data.rds")

uber$Date.Time <- as.POSIXct(strptime(uber$Date.Time, "%m/%d/%Y %H:%M:%S"))
uber$Date <- as.Date(uber$Date.Time)
uber$Time <- format(uber$Date.Time, "%H:%M:%S")
#uber$Date.Time <- NULL

uber <- uber %>%
  mutate(Month = format(Date.Time, "%m"),
         Day = format(Date.Time, "%d"),
         Year = format(Date.Time, "%Y"),
         Hour = format(Date.Time, "%H"),
         Minute = format(Date.Time, "%M"))

uber$Month <- as.numeric(uber$Month)
uber$Day <- as.numeric(uber$Day)
uber$Year <- as.numeric(uber$Year)
uber$Hour <- as.numeric(uber$Hour)
uber$Minute <- as.numeric(uber$Minute)

##########

#1
trips_by_hour <- uber %>%
  group_by(Hour) %>%
  summarise(Count = n()) %>%
  arrange(Hour)

#2
trips_by_month_hour <- uber %>%
  group_by(Month, Hour) %>%
  summarise(Count = n()) %>%
  arrange(Month, Hour)

trips_by_month_hour_graph <- ggplot(trips_by_month_hour, aes(x = Hour, y = Count, fill = factor(Month, levels = 1:12, labels = month.name))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Uber Trips by Hour and Month", x = "Hour of the Day", y = "Number of Trips", fill = "Month") +
  scale_x_continuous(breaks = seq(0, 23, by = 1)) +
  scale_fill_brewer(palette = "Set3") +  
  theme_minimal()

ggsave("~/Documents/DATA332/Projects/Uber Data/Images/Trips by hour and month.png", trips_by_month_hour_graph, width = 10, height = 6, dpi = 300, bg = "white")

#3
trips_by_hour_graph <- ggplot(trips_by_hour, aes(x = Hour, y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Uber Trips by Hour", x = "Hour of the Day", y = "Number of Trips") +
  scale_x_continuous(breaks = seq(0, 23, by = 1)) +
  scale_y_continuous(labels = comma) +  # Use comma function to format y-axis labels with commas
  theme_minimal()

ggsave("~/Documents/DATA332/Projects/Uber Data/Images/trips_by_hour_graph.png", trips_by_hour_graph, width = 10, height = 6, dpi = 300, bg = "white")

#4
trips_by_day <- uber %>%
  group_by(Day) %>%
  summarise(Count = n()) %>%
  arrange(Day)

trips_by_day_graph <- ggplot(trips_by_day, aes(x = Day, y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Uber Trips by Day of the Month", x = "Day of the Month", y = "Number of Trips") +
  scale_x_continuous(breaks = unique(trips_by_day$Day)) +  # Set breaks based on unique days in the dataset
  theme_minimal()

ggsave("~/Documents/DATA332/Projects/Uber Data/Images/trips_by_day_graph.png", trips_by_day_graph, width = 10, height = 6, dpi = 300, bg = "white")

#5
trips_by_month_day <- uber %>%
  group_by(Month, Day, Weekday = weekdays(Date)) %>%
  summarise(Trip_Count = n()) %>%
  arrange(Month, Day) 

trips_by_month_day_graph <- ggplot(trips_by_month_day, aes(x = Weekday, y = Trip_Count, fill = factor(Month))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Day of the Week", y = "Number of Trips", fill = "Month") +
  scale_fill_discrete(name = "Month", labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep")) +  # Customize month labels
  theme_minimal() +  # Optional: Use a minimal theme
  ggtitle("Number of Uber Trips by Month and Day of the Week") 

ggsave("~/Documents/DATA332/Projects/Uber Data/Images/trips_by_month_day_graph.png", trips_by_month_day_graph, width = 10, height = 6, dpi = 300, bg = "white")

#6 
trips_by_base_month <- uber %>%
  group_by(Base, Month) %>%
  summarise(Trip_Count = n()) %>%
  arrange(Month, Base) 

trips_by_base_month_graph <- ggplot(trips_by_base_month, aes(x = Base, y = Trip_Count, fill = factor(Month))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Base", y = "Number of Trips", fill = "Month") +
  scale_fill_discrete(name = "Month", labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep")) +  
  theme_minimal() +  
  ggtitle("Number of Uber Trips by Base and Month") +
  scale_y_continuous(labels = scales::comma) 

ggsave("~/Documents/DATA332/Projects/Uber Data/Images/trips_by_base_month_graph.png", trips_by_base_month_graph, width = 10, height = 6, dpi = 300, bg = "white")



##########
#Heatmaps 

#1
trips_by_hour_day <- uber %>%
  group_by(Day = as.integer(format(Date.Time, "%d")),  # Extract day of the month as integer
           Hour = as.numeric(format(Date.Time, "%H"))) %>%  # Extract hour as numeric
  summarise(Trip_Count = n()) %>%
  ungroup()

trips_by_hour_day_heatmap <- ggplot(trips_by_hour_day, aes(x = Day, y = Hour, fill = Trip_Count)) +
  geom_tile() +
  scale_fill_gradient(low = "green", high = "red") +  # Use red and green color gradient
  labs(x = "Day of Month", y = "Hour of Day", fill = "Number of Trips") +
  theme_minimal() +
  ggtitle("Uber Trips Heatmap by Hour and Day of Month")

ggsave("~/Documents/DATA332/Projects/Uber Data/Images/trips_by_hour_day_heatmap.png", trips_by_hour_day_heatmap, width = 10, height = 6, dpi = 300, bg = "white")

#2
trips_by_month_day <- uber %>%
  group_by(Month = format(Date.Time, "%m"),  # Extract month as two-digit numeric format
           Day = as.integer(format(Date.Time, "%d"))) %>%  # Extract day of the month as integer
  summarise(Trip_Count = n()) %>%
  ungroup()

trips_by_month_day_heatmap <- ggplot(trips_by_month_day, aes(x = Day, y = Month, fill = Trip_Count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "Day of Month", y = "Month", fill = "Number of Trips") +
  theme_minimal() +
  ggtitle("Uber Trips Heatmap by Month and Day")

ggsave("~/Documents/DATA332/Projects/Uber Data/Images/trips_by_month_day_heatmap.png", trips_by_month_day_heatmap, width = 10, height = 6, dpi = 300, bg = "white")


#3
uber$DayOfWeek <- weekdays(uber$Date)
heatmap_data <- table(uber$Base, uber$DayOfWeek)
heatmap_data <- as.data.frame(heatmap_data)
colnames(heatmap_data) <- c("Base", "DayOfWeek", "Count")
heatmap_data$DayOfWeek <- factor(heatmap_data$DayOfWeek, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

trips_by_bases_week_heatmap <- ggplot(heatmap_data, aes(x = DayOfWeek, y = Base, fill = Count)) +
  geom_tile() +
  scale_fill_gradient(low = "darkorange", high = "darkblue") +  # Custom color gradient
  labs(x = "Day of Week", y = "Base", title = "Uber Trips by Base and Day of Week") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("~/Documents/DATA332/Projects/Uber Data/Images/trips_by_bases_week_heatmap.png", trips_by_bases_week_heatmap, width = 10, height = 6, dpi = 300, bg = "white")


##########

heatmap_data <- uber %>%
  group_by(Lat, Lon, Hour) %>%
  summarise(Count = n())

geospatial_map <- leaflet() %>%
  addTiles() %>%
  addHeatmap(
    data = heatmap_data,
    lng = ~Lon, 
    lat = ~Lat,  
    intensity = ~Count, 
    blur = 20,    
    radius = 15,
    minOpacity = 0.1,  
    max = max(heatmap_data$Count)  
  ) %>%
  addLegend(
    position = "bottomright",  
    pal = colorNumeric(palette = "viridis", domain = heatmap_data$Count),  
    values = heatmap_data$Count,  
    title = "Number of Trips"  
  )

saveWidget(geospatial_map, "~/Documents/DATA332/Projects/Uber Data/Images/geospatial_map.html")





