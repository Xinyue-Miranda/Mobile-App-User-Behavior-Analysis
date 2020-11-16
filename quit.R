library(bigrquery)
library(tidyverse)
library(lubridate)


load("actifyVitality_May2019.RData")


## filter only reminder-related events
activity_reminder <- activity_may2019 %>%
  filter( str_detect(event_name, "reminder") ) %>%
  rename(
    event = event_name
  ) %>% 
  group_by(user_pseudo_id) %>% 
  arrange(timestamp) %>%
  distinct()

## use labels for long-format event name
activity_reminder$event <- factor(activity_reminder$event, labels = c("refuse_comfirm", "set_reminder"))


## users reaction to whether to accpet reminder are stored in 'agreedForReminders' 
## reformat the data with 'key' and 'string_value' by spread
df <- activity_reminder %>% 
  rename(
    reminder_timestamp = timestamp
  ) %>%
  spread(
    key, string_value
  ) %>%
  select(
    -event_date,
    -first_touch_timestamp,
    -firebase_event_origin,
    -firebase_screen_id,
    -firebase_screen_class,
    -governance,
    -user,
    -user_id,
    -timestamp
  ) %>% 
  group_by(user_pseudo_id) %>%
  arrange(reminder_timestamp)


## how many times each user agrees/refuses to accept reminder
freq <- df %>% 
  filter( event == "set_reminder" ) %>%
  group_by(user_pseudo_id, agreedForReminders) %>%
  summarise(n = n()) %>% 
  spread(agreedForReminders, n) %>% 
  replace_na(list(no = 0, yes = 0))


## after user chooses to refuse reminder they will need to confirm it on a pop-up window
## the refuse and confirm are usually paired
refuse_confirm <- df %>% 
  filter( is.na(agreedForReminders) |
            agreedForReminders == "no") %>%
  group_by(user_pseudo_id) %>%
  arrange(reminder_timestamp)


## filter only refuse data with timestamp
refuse <- refuse_confirm %>% 
  filter(agreedForReminders == "no") %>%
  select( - refused ) %>%
  group_by(user_pseudo_id) %>%
  arrange(reminder_timestamp)



## filter only reminder-related events
remove_app <- activity_may2019 %>%
  filter( str_detect(event_name, "app_remove") )  %>%
  filter( str_detect(key, "firebase") ) %>%
  group_by(user_pseudo_id) %>% 
  rename( remove_timestamp = timestamp ) %>%
  arrange(remove_timestamp) 


# str_value for "ga_session" are all NAs
# remove_app %>% filter( str_detect(key, "ga_session") )

### there are 35 users have remoed the app more than once
#remove_app %>% group_by( user_pseudo_id ) %>% summarise(n = n()) %>% filter(n != 1) %>% nrow()
#
#remove_app_rep <- remove_app %>%
#  inner_join( remove_app %>% group_by( user_pseudo_id ) %>% summarise(n = n()) %>% filter(n != 1) ) #select(-key, -string_value, -first_touch_timestamp, -event_date)

remove_app %>% distinct(user_pseudo_id) %>% nrow()
# 10112

refuse %>% distinct(user_pseudo_id) %>% nrow()
# 1790


## people who refused reminder and removed the app
remove_and_refuse <- remove_app %>% 
  inner_join( 
    refuse, 
    by = "user_pseudo_id" 
    ) %>%
  select(
    -event_date, 
    -key, 
    -string_value, 
    -agreedForReminders,
    -event_name,
    -event
    ) %>%
  rename(
    remove_app      = remove_timestamp,
    refuse_reminder = reminder_timestamp,
    first_touch     = first_touch_timestamp
  ) 

remove_and_refuse %>% distinct(user_pseudo_id) %>% nrow()
# 762


## explore remove_refuse_reminder
remove_and_refuse <- remove_and_refuse %>% 
  mutate(
    overall_usage     = difftime(remove_app, first_touch,     units = "hours"),
    refuse_to_remove  = difftime(remove_app, refuse_reminder, units = "hours")
  )


remove_and_refuse %>% 
  ungroup() %>% 
  summarise(
    median = median(overall_usage)
  )
# the median overall usage for refuse_and_remove user is 2.12 hours

remove_and_refuse$overall_usage %>% quantile(c(0, .5, .8, .86, 1))
# 86% users less than 10 hours

remove_and_refuse %>%
  mutate(
    overall_usage = as.numeric(overall_usage)
  ) %>%
  filter( overall_usage <= 10 ) %>%
  ggplot( aes(x = overall_usage) ) +
  geom_histogram( bins = 25 ) +
  theme_bw() +
  labs(title = 'Histogram of Overall Usage Time',
       subtitle = 'For Users who removed app within 10 hours')

remove_and_refuse %>% 
  filter( overall_usage <= 10 ) %>%
  ungroup() %>% 
  summarise(
    median = median(overall_usage)
  )
# with median 0.12 hours


## accept reminder
accept_reminder <- df %>%
  filter( event == "set_reminder") %>%
  anti_join(refuse, by = "user_pseudo_id") %>%
  select( -refused )

accept_reminder %>% distinct(user_pseudo_id) %>% nrow()
# 4523

accept_and_remove <- remove_app %>% 
  inner_join( 
    accept_reminder, 
    by = "user_pseudo_id" 
  ) %>%
  select(
    -event_date, 
    -key, 
    -string_value, 
    -agreedForReminders,
    -event_name,
    -event
  ) %>%
  rename(
    remove_app      = remove_timestamp,
    accept_reminder = reminder_timestamp,
    first_touch     = first_touch_timestamp
  ) 

accept_and_remove <- accept_and_remove %>% 
  mutate(
    overall_usage     = difftime(remove_app, first_touch,     units = "hours"),
    refuse_to_remove  = difftime(remove_app, accept_reminder, units = "hours")
  )

accept_and_remove %>% distinct( user_pseudo_id ) %>% nrow()
# 1519


## 1790 users refused the reminder and 762 of them removed the app, 
## ratio 762/1790 = 0.4256
## 4523 users have never refused reminder and 1519 among them removed the app, 
## ratio 1519/4523 = 0.3358


