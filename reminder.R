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



## accept reminder
accept_reminder <- df %>%
  filter( event == "set_reminder") %>%
  anti_join(refuse, by = "user_pseudo_id") %>%
  select( -refused )

accept_reminder %>% distinct(user_pseudo_id) %>% nrow()
# 4523

accept_reminder <- accept_reminder %>%
  #slice(1:10) %>%
  mutate(
    timeslot = case_when(
      hour(reminder_timestamp) <= 6  ~ 'Night',
      hour(reminder_timestamp) <= 12 ~ 'Morning',
      hour(reminder_timestamp) <= 18 ~ 'Afternoon',
      TRUE                           ~ 'Evening'
    )
  )

action <- df %>%
  filter( ! is.na(agreedForReminders) ) %>%
  select( - refused ) %>%
  mutate(
    timeslot = case_when(
      hour(reminder_timestamp) <= 6  ~ 'Night',
      hour(reminder_timestamp) <= 12 ~ 'Morning',
      hour(reminder_timestamp) <= 18 ~ 'Afternoon',
      TRUE                           ~ 'Evening'
    )
  )




df %>%
  filter( ! is.na(agreedForReminders) ) %>%
  select( - refused ) %>%
  mutate(
    timeslot = case_when(
      hour(reminder_timestamp) <= 6  ~ 'Night',
      hour(reminder_timestamp) <= 12 ~ 'Morning',
      hour(reminder_timestamp) <= 18 ~ 'Afternoon',
      TRUE                           ~ 'Evening'
    )
  ) %>% 
  ggplot() +
  geom_bar(aes(x = timeslot, 
               fill = agreedForReminders),
           position = position_dodge()) + 
  theme_bw() +
  labs(x = '', y = '') +
  scale_fill_discrete(name = '', labels = c('Refuse', 'Accept')) +
  scale_x_discrete( limits = c('Morning', 'Afternoon', 'Evening', 'Night')) +
  ggtitle("When Did Users Accept/Refuse Reminders")








