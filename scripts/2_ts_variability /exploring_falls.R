library(ggplot2)
library(dplyr)
library(tidyr)

load("data/mTimes_skip.Rdata")
load("data/wTimes_skip.Rdata")

# Setup
m_times_update$full_name <- paste(m_times_update$fname, m_times_update$lname, sep = " ")
w_times_update$full_name <- paste(w_times_update$fname, w_times_update$lname, sep = " ")

m_times_update |> mutate(sex = "m") |>
  bind_rows(w_times_update |> mutate(sex = "w")) -> all_climbers 

# Get falls per date/event
falls_per_date <- all_climbers |>
  group_by(start_date, tomoa_skip, fname, lname) |>
  summarise(total_falls_fs = sum(fall_final + fall_qual + false_start_qual + false_start_final)) |>
  filter(!is.na(total_falls_fs) & !is.na(total_falls_fs))

# plot falls per event colored by tomoa skip
falls_per_date |>
  filter(!is.na(tomoa_skip)) |>
  ggplot(aes(x=start_date, y=total_falls_fs, color=tomoa_skip)) +
  geom_jitter(alpha = .4) +
  xlab("Start Date") +
  ylab("Total Falls")

## maybe a rate?
falls_per_date |>
  filter(!is.na(tomoa_skip)) |>
  filter(start_date < "2020-01-01" & start_date > "2019-01-01") |>
  group_by(start_date, tomoa_skip) |>
  summarise(n = n(), falls_fs = sum(total_falls_fs))
