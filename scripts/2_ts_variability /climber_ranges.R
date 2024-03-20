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

# Calculate ranges and only keep events with at least one occurrence of both TS usage 
# and no ts usage
all_climbers <- all_climbers |>
  rowwise() |>
  pivot_longer(cols = c("best_qual", "final", "lane_a", "lane_b", "first_round",
                        "quarter", "semi", "small_final", "big_final"), names_to = "round", values_to = "time") |>
  group_by(event, start_date) |> # Only keep events in which there was both tomoa skip and no tomoa skip used
  filter(any(!is.na(time) & tomoa_skip == TRUE) &
           any(!is.na(time) & tomoa_skip == FALSE)) |>
  filter(!is.na(time) & !is.na(tomoa_skip)) |>
  ungroup() |>
  group_by(full_name, event, start_date, tomoa_skip) |>
  summarise(min_time = min(time, na.rm = TRUE),
            max_time = max(time, na.rm = TRUE)) |>
  mutate(range_time = max_time - min_time) |>
  mutate(event = ifelse(start_date == "2022-05-20", "IFSC - Climbing World Cup (B,S) - Salt Lake City (USA) 2022 I", 
                        ifelse(start_date == "2022-05-27", "IFSC - Climbing World Cup (B,S) - Salt Lake City (USA) 2022 II", event)))

# Function to create individual violin plots for each event
create_violin_plot <- function(event_name) {
    event_data <- all_climbers |>
    filter(event == event_name) 
    ggplot(event_data, aes(x = factor(tomoa_skip), y = range_time, fill = factor(tomoa_skip))) +
        geom_violin(trim = FALSE) +
        ggtitle(paste("Range of Times for", event_name)) +
        xlab("Tomoa Skip") +
        ylab("Time") +
        theme_minimal()
}

# Loop through each event and create violin plot
for (event_name in unique(all_climbers$event)) {
    plot <- create_violin_plot(event_name)
    print(plot)
}

# All plots together (a bit hard to see)
ggplot(all_climbers, aes(x=factor(tomoa_skip), y = range_time, fill = factor(tomoa_skip))) +
    geom_violin(trim = TRUE) +
    xlab("Tomoa Skip") +
    ylab("Time") +
    facet_wrap(~event) +
    theme_minimal() 


## Go with this one for now.
all_climbers |>
  mutate(event = factor(event, levels = rev(unique(all_climbers[, c("event", "start_date")]) |> arrange(start_date) %>% .$event))) |>
  ggplot(aes(x=event, y = range_time, group = paste(event, tomoa_skip), fill = factor(tomoa_skip))) +
  geom_boxplot(trim = TRUE) +
  xlab("") +
  ylab("Time (s)") +
  theme_minimal() +
  coord_flip() +
  scale_fill_discrete("Tomoa Skip")


    
# Climber ranges with / without Tomoa Skip
# FIXME -- Not sure if this is the best way to look at ranges
# Filter out climbers who have the same min time as max time
all_climbers <- all_climbers |>
  filter(max_time != min_time)

ggplot(all_climbers, aes(x = min_time, y = max_time)) +
  geom_point(aes(color = factor(tomoa_skip))) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  xlab("Minimum time in event") +
  ylab("Maximum time in event") +
  facet_wrap(~tomoa_skip) +
  theme_minimal()




