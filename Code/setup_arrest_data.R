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

# add variables needed for the analysis of arrest and search rates
SC_for_analysis <- SC_after_exclusions %>%
  mutate(
    arrest = case_when(
      stop_outcome %in% c("Arrest", "Felony Arrest") ~ 1,
      stop_outcome %in% c("Citation", "Warning") ~ 0
    ),
    felony_arrest = 1 * (stop_outcome == "Felony Arrest") + 0 * (stop_outcome != "Felony Arrest"),
    regular_arrest = 1 * (stop_outcome == "Arrest") + 0 * (stop_outcome != "Arrest"),
    race = forcats::fct_relevel(driver_race, "White", "Black", "Hispanic"),
    day_of_week = forcats::fct_relevel(day_of_week, "Sunday"),
    driver_gender = forcats::fct_relevel(driver_gender, "F"),
    month_i = factor(month)
  )

# Add a second variable for dangerous days that leaves out independence day
SC_for_analysis$dang_days2 <- SC_for_analysis$dangerous_days
SC_for_analysis$dang_days2[SC_for_analysis$dang_days2 == "Independence day"] <- "Other day"

write.csv(SC_for_analysis, "arrests.csv", row.names = F) ## we'll use this nice dataset
