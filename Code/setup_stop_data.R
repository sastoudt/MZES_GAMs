library(tidyverse)

## subset of code from SCarolinaTrafficStops repo needed to get our sample datasets

## https://github.com/corinne-riddell/SCarolinaTrafficStops/

#### number of stops ####

load("Enhanced-data-05-06.Rdata") ## from Data folder in SCarolinaTrafficStops repo

# Apply exclusion criteria (highway patrol only, race constraints, either radar triggered or violation observed)
SC_after_exclusions <- SC_05_06 %>%
  filter(
    police_department == "SCHP",
    driver_race %in% c("Black", "Hispanic", "White")
  ) %>%
  filter(stop_purpose %in% c("Radar Triggered", "Violation Observed"))

# Add variables needed for the analysis
SC_after_exclusions <- SC_after_exclusions %>%
  mutate(dangerous_days = case_when(
    day == 31 & month == 12 ~ "New Year's",
    day == 1 & month == 1 ~ "New Year's",
    day == 4 & month == 7 ~ "Independence day",
    day == 18 & month == 3 ~ "Day after St. Patrick's",
    day %in% c(24, 25, 26) & month == 11 & year == 2005 ~ "Thanksgiving weekend", # Thanksgiving Thurs + Friday + Saturday
    day %in% c(23, 24, 25) & month == 11 & year == 2006 ~ "Thanksgiving weekend",
    day %in% c(22, 23, 24) & month == 11 & year == 2007 ~ "Thanksgiving weekend",
    day %in% c(28, 29, 30) & month == 11 & year == 2008 ~ "Thanksgiving weekend",
    day %in% c(26, 27, 28) & month == 11 & year == 2009 ~ "Thanksgiving weekend",
    day %in% c(25, 26, 27) & month == 11 & year == 2010 ~ "Thanksgiving weekend",
    day %in% c(24, 25, 26) & month == 11 & year == 2011 ~ "Thanksgiving weekend",
    day %in% c(22, 23, 24) & month == 11 & year == 2012 ~ "Thanksgiving weekend",
    day %in% c(2, 3, 4, 5) & month == 9 & year == 2005 ~ "Labor Day weekend", # Monday + preceding Friday, Saturday, Sunday
    day %in% c(1, 2, 3, 4) & month == 9 & year == 2006 ~ "Labor Day weekend",
    day %in% c(1, 2, 3) & month == 9 & year == 2007 ~ "Labor Day weekend",
    day == 31 & month == 8 & year == 2007 ~ "Labor Day weekend",
    day == 1 & month == 9 & year == 2008 ~ "Labor Day weekend",
    day %in% c(29, 30, 31) & month == 8 & year == 2008 ~ "Labor Day weekend",
    day %in% c(4, 5, 6, 7) & month == 9 & year == 2009 ~ "Labor Day weekend",
    day %in% c(3, 4, 5, 6) & month == 9 & year == 2010 ~ "Labor Day weekend",
    day %in% c(2, 3, 4, 5) & month == 9 & year == 2011 ~ "Labor Day weekend",
    day %in% c(1, 2, 3) & month == 9 & year == 2012 ~ "Labor Day weekend",
    day == 31 & month == 8 & year == 2012 ~ "Labor Day weekend"
  )) %>%
  mutate(post_policy = case_when(
    year > 2005 ~ 1,
    year == 2005 & month < 12 ~ 0,
    year == 2005 & month == 12 & day >= 9 ~ 1,
    year == 2005 & month == 12 & day < 9 ~ 0
  ))

# table(SC_after_exclusions$dangerous_days, useNA = "always")
SC_after_exclusions$dangerous_days[is.na(SC_after_exclusions$dangerous_days) == T] <- "Other day"
SC_after_exclusions$dangerous_days <- fct_relevel(SC_after_exclusions$dangerous_days, "Other day")
# levels(SC_after_exclusions$dangerous_days)
# table(SC_after_exclusions$dangerous_days, useNA = "always")

SC_daily2 <- SC_after_exclusions %>%
  filter(stop_purpose %in% c("Radar Triggered", "Violation Observed")) %>% # only excludes <1% stops
  group_by(driver_race, stop_date, stop_purpose) %>%
  summarise(
    daily_num_stops = n(),
    month = first(month),
    day = first(day),
    day_index = first(day_index),
    day_of_week = first(day_of_week),
    fri_sat = first(fri_sat),
    post_policy = first(post_policy),
    dangerous_days = first(dangerous_days)
  )

SC_daily2$race <- as.factor(SC_daily2$driver_race)
SC_daily2$month <- as.factor(SC_daily2$month)
SC_daily2$day_of_week <- as.factor(SC_daily2$day_of_week)

SC_daily2$race <- fct_relevel(SC_daily2$race, "White")
SC_daily2$day_of_week <- fct_relevel(SC_daily2$day_of_week, "Sunday")
SC_daily2$dangerous_days <- fct_relevel(SC_daily2$dangerous_days, "Other day")

SC_daily2 <- SC_daily2 %>% mutate(day_counter = difftime(ymd(stop_date), ymd("2005-01-01"), units = "days"))

SC_daily2$dangerous_days2 <- SC_daily2$dangerous_days
SC_daily2$dangerous_days2[SC_daily2$dangerous_days == "Other day"] <- NA

SC_daily2$date2 <- as.Date(SC_daily2$stop_date, "%Y-%m-%d")

SC_daily2$stop_purpose2 <- fct_relevel(SC_daily2$stop_purpose, "Violation Observed", "Radar Triggered")

write.csv(SC_daily2, "dailyStops.csv", row.names = F) ## we'll use this nice dataset
