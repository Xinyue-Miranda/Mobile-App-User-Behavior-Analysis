library(bigrquery)
library(tidyverse)
#library(DBI)
library(lubridate)


#### Downloading Actify Data for May ####
#setwd("~/Box Sync/Actify_Jonathan_Corbin")

# bq_projects()
# bq_project_datasets(project)
# 
# project <- "actify-platform"
# dataset <- "actify-platform.analytics_188140948"
# 
# 
# con <- DBI::dbConnect(bigquery(), project)
# sql <- "SELECT user_pseudo_id, event_date, event_name, 
# EXTRACT(DATETIME FROM TIMESTAMP_MICROS(user_first_touch_timestamp) AT TIME ZONE 'Europe/Amsterdam') as first_touch_timestamp,
# EXTRACT(DATETIME FROM TIMESTAMP_MICROS(event_timestamp) AT TIME ZONE 'Europe/Amsterdam') as timestamp,
# event.key, event.value.string_value
# FROM `actify-platform.analytics_188140948.events_201905*`,
# UNNEST(event_params) as event,
# UNNEST(user_properties) as user
# WHERE _TABLE_SUFFIX BETWEEN '01' AND '31';"
# 
# activity_may2019 <- DBI::dbGetQuery(con, sql)

#### Uploading Actify Data (Once data has been saved to Box) ####

#load(#ADD INFO HERE)
load("actifyVitality_May2019.RData")

#### Adding new variables ####

  #Changing event date to time variable
activity_may2019$event_date <- as.Date((activity_may2019$event_date), format = "%Y%m%d")

  #Adding column with number of observations for each user ID
activity_may2019 <- activity_may2019 %>%
  group_by(user_pseudo_id) %>%
  summarize(idNumObs = n()) %>%
  right_join(activity_may2019, by = "user_pseudo_id")

  #Creating a timing variable based on the difference between the event timestamp and their first_touch_timestamp
activity_may2019 <-  activity_may2019 %>%
  mutate()



####Single day participants####

activitySingleDay <- activity_may2019 %>%
  group_by(user_pseudo_id) %>%
  mutate(lastTimeStamp = max(timestamp)) %>%
  ungroup() %>%
  filter(lastTimeStamp - first_touch_timestamp < 86400) 

  #Counting number of single users and mean time (in seconds)         
activitySingleDay %>%
  summarize(meanTime = mean((lastTimeStamp - first_touch_timestamp)/3600),
            sdTime = sd((lastTimeStamp - first_touch_timestamp)/3600),
            minTime = min((lastTimeStamp - first_touch_timestamp)/3600),
            maxTime = max((lastTimeStamp - first_touch_timestamp)/3600),
            countIDs = length(unique(user_pseudo_id)))



#Looking at just mini-related activity over first day
activitySingleDay %>%
  filter(str_detect(event_name, "add"),
         str_detect(event_name, "complete"),
         str_detect(event_name, "undo", negate = T)) %>%
  mutate(diffTime = (timestamp - first_touch_timestamp)/3600,
         diffTime = as.numeric(diffTime)) %>%
  filter(diffTime > 1) %>%
  ggplot(aes(x = diffTime, fill = event_name)) +
  geom_histogram(bins = 100)

#############
activitySingleDay %>%
  filter(str_detect(event_name, "add")|
         str_detect(event_name, "complete")|
         str_detect(event_name, "undo")) %>%
  mutate(diffTime = (timestamp - first_touch_timestamp)/3600,
         diffTime = as.numeric(diffTime)) %>%
  filter(diffTime > 1) %>%
  ggplot(aes(x = diffTime, fill = event_name)) +
  geom_histogram(bins = 25)
#############

  #Looking at all activity (except for a few uninformative events) throughout first day
activitySingleDay %>%
  group_by(event_name) %>%
  summarize(numEvents = n()) %>%
  filter(str_detect(event_name, "navigate_to_screen", negate=T),
         str_detect(event_name, "user_engagement", negate=T),
         str_detect(event_name, "screen_view", negate=T),
         str_detect(event_name, "error", negate=T)) %>%
  ggplot(aes(x = event_name, y = numEvents)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

############
activitySingleDay %>%
  group_by(event_name) %>%
  summarize(numEvents = n()) %>%
  filter(str_detect(event_name, "navigate_to_screen")|
         str_detect(event_name, "user_engagement")|
         str_detect(event_name, "screen_view")|
         str_detect(event_name, "error")) %>%
  ggplot(aes(x = event_name, y = numEvents)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#############

activity_may2019 %>%
  filter(str_detect(event_name, "add_mini_from_suggestion")) %>%
  mutate(diffTime = (timestamp - first_touch_timestamp),
         diffTime = as.numeric(diffTime)) %>%
  group_by(diffTime, event_name) %>%
  summarize(eventFrequency = length(event_name)) %>%
  ggplot(aes(x = diffTime/3600, y = eventFrequency, color = event_name)) +
  geom_col()
  




activityMayMiniSeries <- activity_may2019 %>%
  filter(str_detect(event_name, "add") |
         str_detect(event_name, "complete"),
         str_detect(event_name, "undo", negate = T))

unique(activityMayMiniSeries$event_name)

activityMayMiniSeries %>%
  group_by(user_pseudo_id,event_name) %>%
  summarize(count = length(event_name)) %>%
  ungroup() %>%
  filter(str_detect(event_name, "completed")) %>% glimpse()
  spread(event_name, count) %>%
  mutate(miniCompletedTotal = mini_completed + mini_comp)

activityMayMiniSeries %>%
  group_by(user_pseudo_id) %>%
  filter(event_name)
  filter()
