library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
library(knitr)
library(stringr)
library(scales)

# Load data
load("data/mTimes_skip.Rdata")
load("data/wTimes_skip.Rdata")
load("data/events.Rdata")


# Number of Tomoa Skip users per start date plot
label_names <- c('F' = 'Womens', 'M' = 'Mens')
custom_colors <- c("Tomoa Skip" = "#d95f02", "No Tomoa Skip" = "#7570b3")

m_times_update |>
  bind_rows(w_times_update) |>
  group_by(sex, start_date) |>
  filter(!is.na(tomoa_skip)) |>
  summarise(num_ts = sum(tomoa_skip), num_no_ts = sum(!tomoa_skip)) |>
  ggplot() +
  geom_point(aes(start_date, num_ts, color = "Tomoa Skip")) + 
  geom_point(aes(start_date, num_no_ts, color = "No Tomoa Skip")) + 
  geom_smooth(aes(start_date, num_ts, color = "Tomoa Skip"), method = "auto") +
  geom_smooth(aes(start_date, num_no_ts, color = "No Tomoa Skip"), method = "auto") +
  facet_wrap(.~sex, labeller = as_labeller(label_names)) +
  labs(x="Start Date", y = "Count Tomoa Skip Usage") + 
  theme(legend.title = element_blank()) +
  scale_color_manual(values = custom_colors) +
  theme(legend.position = "bottom")


# Events per year plot
events |> 
  group_by(year) |> 
  summarise(
    num_events = n()
  ) |> 
  ggplot(aes(x = year, y = num_events)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Number of Events") +
  scale_x_continuous(breaks = seq(min(events$year), max(events$year), by = 1)) +
  ylim(0, NA) 


# Tomoa Skip Progression plot
test_df <- m_times_update |>
  bind_rows(w_times_update)
test_df$full_name <- paste(test_df$fname, test_df$lname, sep = " ")
true_ts_df <- subset(test_df, tomoa_skip == TRUE)
earliest_dates <- aggregate(start_date ~ full_name, data = true_ts_df, FUN = min)
test_df$days_since_first_skip <- as.numeric(test_df$start_date - earliest_dates$start_date[match(test_df$full_name, earliest_dates$full_name)])

test_df |>
  pivot_longer(cols = c("best_qual", "final", "lane_a", "lane_b", "first_round", "quarter", "semi", "small_final", "big_final"), names_to = "round", values_to = "time") |>
  group_by(fname, lname, tomoa_skip, sex, event_id, year, start_date, days_since_first_skip) |>
  mutate(best_time = if (all(is.na(time))) NA else min(time, na.rm = TRUE)) |>
  group_by(fname, lname) |> 
  mutate(count = length(unique(event_id))) |>  
  filter(!is.na(best_time) & !is.na(days_since_first_skip) & count > 10 & time < 50) |>
  filter(lname == "brosler" | fname == "veddriq" | lname == "miroslaw" | lname == "brozek") |>
  ggplot(aes(days_since_first_skip, time)) +
  geom_rect(aes(xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "grey", alpha = .1) +
  geom_point(aes(group = paste(fname, lname)), alpha = 0.4) +
  geom_point(aes(days_since_first_skip, best_time, group = paste(fname, lname)), color="#d95f02") +
  geom_smooth(aes(days_since_first_skip, best_time, group = paste(fname, lname)), color="#d95f02", se = FALSE) +
  facet_wrap(~str_to_title(paste(fname, lname)), scales = "free_x") +
  labs(x = "Days since first Tomoa Skip", y = "Best Time") +
  theme(legend.position = "none") 


# Tomoa Skip ranges plot
m_times_update$full_name <- paste(m_times_update$fname, m_times_update$lname, sep = " ")
w_times_update$full_name <- paste(w_times_update$fname, w_times_update$lname, sep = " ")
m_times_update |> mutate(sex = "m") |>
  bind_rows(w_times_update |> mutate(sex = "w")) -> all_climbers 

all_climbers <- all_climbers |>
  rowwise() |>
  pivot_longer(cols = c("best_qual", "final", "lane_a", "lane_b", "first_round",
                        "quarter", "semi", "small_final", "big_final"), names_to = "round", values_to = "time") |>
  group_by(event, start_date) |> # Only keep events in which there was both Tomoa Skip and no Tomoa Skip used
  filter(any(!is.na(time) & tomoa_skip == TRUE) &
           any(!is.na(time) & tomoa_skip == FALSE)) |>
  filter(!is.na(time) & !is.na(tomoa_skip)) |>
  ungroup() |>
  group_by(full_name, event, start_date, tomoa_skip) |>
  summarise(min_time = min(time, na.rm = TRUE),
            max_time = max(time, na.rm = TRUE)) |>
  mutate(range_time = max_time - min_time) |>
  mutate(event = ifelse(start_date == "2022-05-20", "IFSC - Climbing World Cup (B,S) - Salt Lake City (USA) 2022 I", 
                        ifelse(start_date == "2022-05-27", "IFSC - Climbing World Cup (B,S) - Salt Lake City (USA) 2022 II", event))) |>
  mutate(event = str_split(event, "-") |> sapply(function(x) trimws(x[length(x)]))) 

custom_colors <- c("TRUE" = "#d95f02", "FALSE" = "#7570b3")

all_climbers |>
  mutate(event = factor(event, levels = rev(unique(all_climbers[, c("event", "start_date")]) |> arrange(start_date) %>% .$event))) |>
  ggplot(aes(x=event, y = range_time, group = paste(event, tomoa_skip), fill = factor(tomoa_skip))) +
  geom_boxplot(trim = TRUE) +
  xlab("") +
  ylab("Worst - Best Times (s)") +
  coord_flip() +
  scale_fill_manual(name = "Tomoa Skip", values = custom_colors)


# World record plot
wr_data <- data.frame(
  year = as.Date(c("2011-08-27", "2012-10-13", "2012-10-13", "2014-08-31", "2014-09-12", "2017-04-30", "2021-05-28", "2021-05-28",
                   "2022-05-06", "2022-05-21", "2022-06-30", "2022-06-30", "2022-07-08", "2023-04-08", "2023-04-28", 
                   "2017-04-23", "2017-04-30", "2017-07-22", "2019-04-26", "2019-10-19", "2020-11-21", "2021-08-06",
                   "2022-05-06", "2022-05-27", "2023-04-28", "2023-04-28", "2023-04-28", "2023-04-28", "2023-09-15", "2018-04-22", "2017-07-22", "2017-04-30", "2017-04-23", "2015-07-11", "2015-06-21", "2015-04-17", "2014-10-19")),
  sex = c("M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "F", "F", "F", "F", "F", "F",
          "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F"),
  record = c(6.26 ,6.07, 5.88, 5.73, 5.60, 5.48, 5.25, 5.20, 5.17, 5.10, 5.09, 5.04, 5.00, 4.98, 4.90, 7.46, 7.38,   7.32, 7.10, 6.99, 6.96, 6.84, 6.64, 6.53, 6.46, 6.37, 6.35, 6.25, 6.24, 7.32, 7.32, 7.38, 7.46, 7.53, 7.56, 7.74, 7.85)
) 

wr_data <- wr_data |>
  arrange(sex, year) |>
  group_by(sex) |>
  mutate(duration = if_else(is.na(lead(year)), 0, as.numeric(lead(year) - year)))


label_names <- c('F' = "Womens", 'M' = "Mens")

ggplot(wr_data, aes(x = year, y = record)) +
  geom_rect(aes(xmin = as.Date("2018-08-18"), xmax = as.Date(Inf), ymin = as.Date(-Inf), ymax = as.Date(Inf)), fill = "grey", alpha = .1) +
  geom_point(size = 0.76) +
  labs(x = "Date", y = "Record", color = "Gender") +
  facet_wrap(~sex, labeller = as_labeller(label_names)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") + 
  geom_segment(aes(x = year, xend = year + duration, y = record, yend = record))

