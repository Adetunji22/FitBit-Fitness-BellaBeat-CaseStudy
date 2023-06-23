#### Setting up the tool

```{r warning=FALSE}

library(tidyverse)
library(janitor)
library(skimr) 

```

#### Importing the data set

```{r}
daily_activity <- read.csv("file:///C:/Users/NEWGENERATION/Desktop/dailyActivity_merged.csv")
sleep_day <- read.csv("file:///C:/Users/NEWGENERATION/Desktop/sleepDay_merged.csv")

```

##### having a quick glance at the data

```{r}
head(daily_activity)
head(sleep_day)

```

#### Data cleaning and Manipulation

* Checking for null or missing values
```{r}
sum(is.null(daily_activity))
sum(is.null(sleep_day))
```

* checking for duplicates values
```{r}
sum(duplicated(daily_activity))
sum(duplicated(sleep_day))

```

```{r}
sleep_day <- distinct(sleep_day)
```
  
```{r}
sum(duplicated(sleep_day))
```
 
 * checking for number of distinct Id's
```{r}
n_distinct(daily_activity$Id)
n_distinct(sleep_day$Id)

```

 * taking a look at the data frame column names
```{r}
colnames(daily_activity)
colnames(sleep_day)
```

* checking the column names are of correct datatypes
```{r}
str(daily_activity)
str(sleep_day)
```
 

 * correcting the wrong data type
```{r}
daily_activity <- daily_activity %>% 
  mutate(ActivityDate = mdy(ActivityDate))

sleep_day <- sleep_day %>% 
  mutate(SleepDay = mdy_hms(SleepDay))

```
 
  * confirming the dataframes are of correct data types
```{r}
str(daily_activity$ActivityDate)
str(sleep_day$SleepDay)
 
```

 * Quick statistical view of the users activities
```{r}
summary(daily_activity)
 summary(sleep_day)
```

 
##### Transforming And Manipulating The DataFrames
 
```{r}
 daily_activity <- daily_activity %>% 
  mutate(weekdays= weekdays(ActivityDate))

 daily_activity <- daily_activity %>%    
  mutate(totalminutes = VeryActiveMinutes+FairlyActiveMinutes+LightlyActiveMinutes+SedentaryMinutes)


daily_activity <- daily_activity %>% 
  select(Id,weekdays,TotalSteps,TotalDistance,totalminutes,Calories) 

sleep_day <- sleep_day %>% 
  mutate(weekdays = weekdays(SleepDay))

sleep_day <- sleep_day %>% 
  select(Id, weekdays, TotalMinutesAsleep,TotalTimeInBed)  

```

```{r}
head(daily_activity)
head(sleep_day)
```


## ANALYZE AND SHARE
 
**In this phase i will be analyzing and visualizing the data to communicate my findings and observations.

```{r}
averagesteps <- daily_activity %>% 
  group_by(weekdays) %>% 
  summarise(averagesteps =round(mean(TotalSteps),1)) %>% 
  arrange(averagesteps) 

ggplot(data = averagesteps) +
  geom_bar(mapping = aes(x = weekdays, y = averagesteps), fill = 'blue', stat = "identity") +
  geom_text(aes(x = weekdays,y = averagesteps,label = averagesteps),vjust = -0.5) +
  labs(x= "days of week", y = "avg.steps", title = "AVERAGE NUMBER OF STEPS BY DAYS OF WEEKS")
```


```{r}
caloriesburnt <- daily_activity %>% 
   group_by(weekdays) %>% 
   summarise(totalcalories = sum(Calories))

ggplot( data = caloriesburnt) +
  geom_bar(mapping = aes(x= weekdays, y = totalcalories),fill = 'blue', stat ="identity") +
  labs(x = "days of week" , y = "calories burnt", title = "CALORIES BURNT FOR EACH DAYS OF THE WEEK")

```
 

```{r}
 ggplot(data = daily_activity) +
    geom_point(mapping = aes(x = TotalSteps, y = Calories),color = 'blue') +
    geom_smooth(aes(x =TotalSteps, y = Calories),color = 'black',method ="lm",se=FALSE)+
    labs(title = "RELATIONSHIP BETWEEN TOTAL STEPS AND CALORIES BURNT")

ggplot(data = daily_activity) +
    geom_point(mapping = aes(x = totalminutes, y = Calories), color = 'blue') +
    labs(title = " DAILY ACTIVITIES MINUTES VS CALORIES BURNT", X =" daily minutes",y = "Caloriesburnt")
```
 

```{r}
 ggplot(data = sleep_day) +
    geom_point(mapping = aes(x = TotalMinutesAsleep, y = TotalTimeInBed)) +
    labs(title = "total minutes asleep vs total time in bed")
```
