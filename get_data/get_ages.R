###############################################
#   The file "unparsed_birthdates.csv" used here is the raw manipulated data that
#   came from our manual data collection process
###############################################

library(dplyr)
library(lubridate)

birth_dates <- read.csv("data/unparsed_birthdates.csv")
mTimes_skip <- read.csv("data/mTimes_skip.csv")
wTimes_skip <- read.csv("data/wTimes_skip.csv")

# Process Male Times
mTimes <- mTimes_skip |>
  mutate(full_name = paste(fname, lname), 
         gender = "Male")

# Process Female Times
wTimes <- wTimes_skip |>
  mutate(full_name = paste(fname, lname), 
         gender = "Female")

# Join with birth dates
mTimes <- mTimes |>
  left_join(birth_dates |> select(full_name, dob, gender), by = c("full_name", "gender"))

wTimes <- wTimes |>
  left_join(birth_dates |> select(full_name, dob, gender), by = c("full_name", "gender"))

# Remove unnecessary columns
mTimes <- mTimes |>
  select(-full_name, -gender)

wTimes <- wTimes |>
  select(-full_name, -gender)

# Combine start dates
combined_start_dates <- bind_rows(
  mTimes |> select(start_date),
  wTimes |> select(start_date)
)

# Calculate the earliest start date
earliest_start_date <- min(as.Date(combined_start_dates$start_date, format = "%Y-%m-%d"))

# Calculate ages and time progression for Male Times
mTimes_with_ages <- mTimes |>
  mutate(
    time_progression = as.numeric(difftime(as.Date(start_date, format = "%Y-%m-%d"), earliest_start_date, units = "days")),
    age = floor(as.numeric(difftime(as.Date(start_date, format = "%Y-%m-%d"), as.Date(dob, format = "%Y-%m-%d"), units = "days")) / 365)
  )

# Calculate ages and time progression for Female Times
wTimes_with_ages <- wTimes |>
  mutate(
    time_progression = as.numeric(difftime(as.Date(start_date, format = "%Y-%m-%d"), earliest_start_date, units = "days")),
    age = floor(as.numeric(difftime(as.Date(start_date, format = "%Y-%m-%d"), as.Date(dob, format = "%Y-%m-%d"), units = "days")) / 365)
  )

# If ages are NA, set to average age in an event for Male Times
mTimes_with_ages <- mTimes_with_ages |>
  group_by(event_id) |>
  mutate(
    avg_age = mean(age, na.rm = TRUE),
    age = ifelse(is.na(age), avg_age, age)
  ) |>
  ungroup() |>
  select(-avg_age)

# If ages are NA, set to average age in an event for Female Times
wTimes_with_ages <- wTimes_with_ages |>
  group_by(event_id) |>
  mutate(
    avg_age = mean(age, na.rm = TRUE),
    age = ifelse(is.na(age), avg_age, age)
  ) |>
  ungroup() |>
  select(-avg_age)

write.csv(mTimes_with_ages, file = "data/mTimesAges.csv", row.names = FALSE)
write.csv(wTimes_with_ages, file = "data/wTimesAges.csv", row.names = FALSE)
