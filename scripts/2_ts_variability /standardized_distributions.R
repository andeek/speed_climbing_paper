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

all_climbers <- all_climbers |>
  rowwise() |>
  mutate(final = ifelse(!is.na(final) & (!is.na(small_final) | !is.na(first_round) | !is.na(quarter) | 
                                           !is.na(semi) | !is.na(small_final) | !is.na(big_final)), NA, final)) |>
  mutate(best_qual = ifelse(!is.na(best_qual) & (!is.na(lane_a) | !is.na(lane_b)), NA, best_qual)) |>
  pivot_longer(cols = c("best_qual", "final", "lane_a", "lane_b", "first_round",
                        "quarter", "semi", "small_final", "big_final"), names_to = "round", values_to = "time")

# Calculate each climbers mean time and standard deviation
mean_sd_times <- all_climbers |>
  group_by(full_name) |>
  filter(!is.na(time)) |>
  summarise(mean_time = mean(time, na.rm = TRUE), sd_time = sd(time, na.rm = TRUE)) |>
  filter(!is.na(sd_time) & (sd_time != 0))

# Merge mean times back into climber data and calculate standardized times
all_climbers <- all_climbers |>
  left_join(mean_sd_times, by = "full_name") |>
  filter(!is.na(time) & !is.na(mean_time) & !is.na(tomoa_skip)) |>
  mutate(std_time = (time - mean_time) / sd_time)

# All climber's standardized times combined
ggplot(all_climbers, aes(x = std_time, color = tomoa_skip)) +
  geom_density(alpha = 0.5) +
  # geom_violin(alpha = 0.5) +
  xlab("Standardized Time") +
  ylab("Density") +
  theme_minimal()

# All climbers distributions on one plot
# drop climbers with less than 2 events to avoid warnings
df_drop_2 <- all_climbers |>
  group_by(full_name) |>
  filter(n() >= 2) |>
  ungroup()

#FIXME
ggplot(df_drop_2) + 
  geom_line(aes(x = std_time, group = paste(full_name, tomoa_skip), color = factor(tomoa_skip)), stat="density", alpha=0.1) +
  geom_line(aes(x = std_time, group = paste(tomoa_skip), color = factor(tomoa_skip)), stat="density", size = 2) +
  xlab("Standardized Time") +
  ylab("Density") +
  theme_minimal()

