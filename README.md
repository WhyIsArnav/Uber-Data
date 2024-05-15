# UBER DATA ANALYSIS

### Author: Arnav Shrestha

<div>
<img src ="Images/UberImage.png" width = "450")>
</div>

## Overview:
This project looks at Uber data at a certain location and tries to analyze various insights and questions related to the dataset like trips by the hour, Trips by Hour and Month, Trips Every Hour, trips by Day and Month, Trips by Bases and Month, and various heatmaps. 

## Directory 📖
The columns that were used are: 
- Lat
- Long
- Base
- Date
- Time
- Month
- Day
- Year
- Hour
- Minute
- Day of week 

## Data Cleaning 🧹

**1. Put all csv file into one single dataframe**

```r

df1 <- read.csv("uber-raw-data-apr14.csv")
df2 <- read.csv("uber-raw-data-may14.csv")
df3 <- read.csv("uber-raw-data-jun14.csv")
df4 <- read.csv("uber-raw-data-jul14.csv")
df5 <- read.csv("uber-raw-data-aug14.csv")
df6 <- read.csv("uber-raw-data-sep14.csv")

uber <- rbind(df1,df2,df3,df4,df5,df6)
```

**2. Fix the date and time format and use the mutate function**

```r
uber$Date.Time <- as.POSIXct(strptime(uber$Date.Time, "%m/%d/%Y %H:%M:%S"))
uber$Date <- as.Date(uber$Date.Time)
uber$Time <- format(uber$Date.Time, "%H:%M:%S")

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
```

## Data analysis 
**1. Making pivot table for trips by hour and graph**
```r
trips_by_hour <- uber %>%
  group_by(Hour) %>%
  summarise(Count = n()) %>%
  arrange(Hour)

trips_by_hour_graph <- ggplot(trips_by_hour, aes(x = Hour, y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Uber Trips by Hour", x = "Hour of the Day", y = "Number of Trips") +
  scale_x_continuous(breaks = seq(0, 23, by = 1)) +
  scale_y_continuous(labels = comma) +  # Use comma function to format y-axis labels with commas
  theme_minimal()
```

**2. Making pivot table for trips by hour each month and graph**
```r
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
```

**3. Making pivot table for trips each day and graph**
```r
trips_by_day <- uber %>%
  group_by(Day) %>%
  summarise(Count = n()) %>%
  arrange(Day)

trips_by_day_graph <- ggplot(trips_by_day, aes(x = Day, y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Uber Trips by Day of the Month", x = "Day of the Month", y = "Number of Trips") +
  scale_x_continuous(breaks = unique(trips_by_day$Day)) +  # Set breaks based on unique days in the dataset
  theme_minimal()
```

**4. Making pivot table for trips each day during the month and graph**

```r
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

```

**5. Making pivot table for trips each month by base and graph**

```r
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
```

**6. Making heatmap for trips by base**

```r
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
```


**7. Making graph for each base**


```r
ggplot(df_data, aes(Base)) + 
  geom_bar(fill = "cyan2") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases")
```

**8. Making graph for each base each month**

```r
ggplot(df_data, aes(Base, fill = month)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and Month") +
  scale_fill_manual(values = colors)
```

**9. Making graph for each base each day of the week**


```r
ggplot(df_data, aes(Base, fill = dayofweek)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and DayofWeek") +
  scale_fill_manual(values = colors)
```

**10. Making pivot and heat map for each day and hour**

```r
day_and_hour <- df_data %>%
  group_by(day, hour) %>%
  dplyr::summarize(Total = n())


ggplot(day_and_hour, aes(day, hour, fill = Total)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low="pink", high="violet") +
  ggtitle("Heat Map by Hour and Day")
```

**11. Make heat map for each day and month**

```r
ggplot(day_month_group, aes(day, month, fill = Total)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low="lightblue", high="darkblue") +
  ggtitle("Heat Map by Month and Day")
```

**12. Make heat map for each day of the week and month**

```r
ggplot(month_weekday, aes(dayofweek, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day of Week")

```


**13. Make heat map for bases and month**

```r
month_base <-  df_data %>%
  group_by(Base, month) %>%
  dplyr::summarize(Total = n()) 

day0fweek_bases <-  df_data %>%
  group_by(Base, dayofweek) %>%
  dplyr::summarize(Total = n()) 

ggplot(month_base, aes(Base, month, fill = Total)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low="#06aa85", high="#0590f5") +
  ggtitle("Heat Map by Month and Bases")

```

**14. Make heat map for bases and day of the week**

```r
ggplot(day0fweek_bases, aes(Base, dayofweek, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Bases and Day of Week")
```
