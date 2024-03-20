library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
library(knitr)
library(stringr)

load("data/mTimes_skip.Rdata")
load("data/wTimes_skip.Rdata")

## Start here -----
# Best times per event
m_times_update |>
  bind_rows(w_times_update) |>
  pivot_longer(cols = c("best_qual", "final", "lane_a", "lane_b", "first_round", "quarter", "semi", "small_final", "big_final"), names_to = "round", values_to = "time") |>
  group_by(fname, lname, tomoa_skip, sex, event_id, year, start_date) |>
  # Handle case where all values in time are NA 
  summarise(best_time = if (all(is.na(time))) NA else min(time, na.rm = TRUE)) |>
  filter(!is.na(best_time)) |>
  ggplot() +
  geom_jitter(aes(start_date, best_time, color = tomoa_skip)) +
  facet_wrap(.~sex) + 
  ggtitle("Best Time Per Event") + 
  labs(x = "Start Date", y = "Best Time")



# Qualifying times for men and women
m_times_update |>
  bind_rows(w_times_update) |>
  pivot_longer(cols = c("best_qual", "lane_a", "lane_b"), names_to = "round", values_to = "qual_time") |>
  group_by(fname, lname, tomoa_skip, sex, event_id, year, start_date) |>
  filter(!is.na(qual_time)) |>
  ggplot() +
  geom_jitter(aes(start_date, qual_time, color = tomoa_skip)) + 
  facet_wrap(.~sex) + 
  coord_cartesian(ylim = c(0, 40)) + 
  ggtitle("Qualifying Times for Men and Women") +
  labs(x = "Start Date", y = "Qualifier Time") 
  



# Final times for men and women
m_times_update |>
  bind_rows(w_times_update) |>
  pivot_longer(cols = c("final", "first_round", "quarter", "semi", "small_final", "big_final"), names_to = "round", values_to = "final_time") |>
  group_by(fname, lname, sex, tomoa_skip, event_id, year, start_date) |>
  filter(!is.na(final_time)) |>
  ggplot() +
  geom_jitter(aes(start_date, final_time, color = tomoa_skip)) + 
  facet_wrap(.~sex) + 
  coord_cartesian(ylim = c(0, 40)) + 
  ggtitle("Final Times for Men and Women") + 
  labs(x = "Start Date", y = "Final Time")


# Womens qualifier times 
qual_plot <-w_times_update |>
  pivot_longer(cols = c("best_qual", "lane_a", "lane_b"), names_to = "round", values_to = "qual_time") |>
  group_by(fname, lname, tomoa_skip, sex, event_id, year, start_date) |>
  filter(!is.na(qual_time))  |>
  ggplot() +
  # Hiding legends to prevent compressing
  geom_jitter(aes(start_date, qual_time, color = tomoa_skip), show.legend = FALSE) + 
  coord_cartesian(ylim = c(0, 40)) + 
  ggtitle("Qualifying Times for Women") + 
  theme(legend.position = "none") + 
  labs(x = "Start Date", y = "Qualifier Time")

# Womens final times
final_plot <- w_times_update |>
  pivot_longer(cols = c("final", "first_round", "quarter", "semi", "small_final", "big_final"), names_to = "round", values_to = "final_time") |>
  group_by(fname, lname, sex, tomoa_skip, event_id, year, start_date) |>
  filter(!is.na(final_time))  |>
  ggplot() +
  geom_jitter(aes(start_date, final_time, color = tomoa_skip), show.legend = FALSE) + 
  coord_cartesian(ylim = c(0, 40)) + 
  ggtitle("Final Times for Women") + 
  labs(x = "Start Date", y = "Final Time")

plot_grid(qual_plot, final_plot, ncol = 2)


# Mens qualifier times 
qual_plot <-m_times_update |>
  pivot_longer(cols = c("best_qual", "lane_a", "lane_b"), names_to = "round", values_to = "qual_time") |>
  group_by(fname, lname, tomoa_skip, sex, event_id, year, start_date) |>
  filter(!is.na(qual_time))  |>
  ggplot() +
  geom_jitter(aes(start_date, qual_time, color = tomoa_skip), show.legend = FALSE) + 
  coord_cartesian(ylim = c(0, 40)) + 
  ggtitle("Qualifying Times for Men") + 
  labs(x = "Start Date", y = "Qualifier Time") + 
  theme(legend.position = "none")

# Mens final times
final_plot <- m_times_update |>
  pivot_longer(cols = c("final", "first_round", "quarter", "semi", "small_final", "big_final"), names_to = "round", values_to = "final_time") |>
  group_by(fname, lname, sex, tomoa_skip, event_id, year, start_date) |>
  # There are 3 outliers of ~100 seconds
  filter(!is.na(final_time))  |>
  ggplot() +
  geom_jitter(aes(start_date, final_time, color = tomoa_skip), SHOW.LEGEND = FALSE) + 
  coord_cartesian(ylim = c(0, 40)) + 
  ggtitle("Final Times for Men") + 
  labs(x = "Start Date", y = "Final Time") 

#Combine two plots
plot_grid(qual_plot, final_plot, ncol = 2) 



## Number of tomoa skip users per start date
#Label Names:
label_names <- c('F' = 'Womens', 'M' = 'Mens')

TS_Users <- m_times_update |>
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
  labs(x="Start Date", y = "Count") + 
  theme_bw() + 
  theme(
    legend.title = element_blank(),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 11),
    strip.text = element_text(size = 13),
    legend.text = element_text(size = 13)
    ) 




#Best Final and Best Qualifier
label_names <- c('FALSE' = "No Tomoa Skip", 'TRUE' = "Tomoa Skip")
m_times_update |>
  # These pivot_longers are producing duplicates
  filter(rank <= 16) |>
  pivot_longer(cols = c("best_qual", "lane_a", "lane_b"), names_to = "qual_round", values_to = "qual_time") |>
  pivot_longer(cols = c("final", "first_round", "quarter", "semi", "small_final", "big_final"), names_to = "final_round", values_to = "final_time") |>
  group_by(fname, lname, sex, event_id, tomoa_skip) |>
  summarise(
    best_qual = ifelse(all(is.na(qual_time)), NA, min(qual_time, na.rm = TRUE)),    
    best_final = ifelse(all(is.na(final_time)), NA, min(final_time, na.rm = TRUE))
  ) |>
  filter(!is.na(best_qual) & !is.na(best_final)) |>
  ggplot() +
  geom_jitter(aes(best_qual, best_final, color = tomoa_skip), alpha=0.7, show.legend = FALSE) + 
  geom_abline() +
  facet_wrap(.~tomoa_skip, labeller = as_labeller(label_names)) + 
  labs(x = "Best Qualifier Time", y = "Best Final Time") + 
  coord_cartesian(ylim = c(0, 30), xlim = c(0, 30)) + 
  scale_colour_discrete(name  ="",
                        breaks=c(TRUE, FALSE),
                        labels=c("Tomoa Skip", "No Tomoa Skip"))




  
#FIXME -- How to make this readable?
## Days since first tomoa skip

# Find first Tomoa Skip dates and days since first skip
test_df <-  m_times_update
test_df$full_name <- paste(m_times_update$fname, m_times_update$lname, sep = " ")
test_df <- subset(test_df, tomoa_skip == TRUE)
earliest_dates <- aggregate(start_date ~ full_name, data = test_df, FUN = min)
test_df$days_since_first_skip <- as.numeric(test_df$start_date - earliest_dates$start_date[match(test_df$full_name, earliest_dates$full_name)])

test_df |>
  pivot_longer(cols = c("best_qual", "final", "lane_a", "lane_b", "first_round", "quarter", "semi", "small_final", "big_final"), names_to = "round", values_to = "time") |>
  group_by(fname, lname, tomoa_skip, sex, event_id, year, start_date, days_since_first_skip) |>
  summarise(best_time = if (all(is.na(time))) NA else min(time, na.rm = TRUE)) |>
  filter(!is.na(best_time)) |>
  ggplot() +
  #geom_jitter(aes(days_since_first_skip, best_time, color = tomoa_skip)) +
  geom_line(aes(days_since_first_skip, best_time, group = paste(fname, lname))) +
  labs(x = "Days since first tomoa skip", y = "Best Time")




## Progression of Best Time Per Event Since First Tomoa Skip

days_since_labels <- c(
                       'john brosler' = "John Brosler",
                       'reza alipour shenazandifard' = "Reza Alipour Shenazandifard",
                       'veddriq leonardo' = "Veddriq Leonardo"
                      )

# Find first Tomoa Skip dates and days since first skip
test_df <- m_times_update
test_df$full_name <- paste(test_df$fname, test_df$lname, sep = " ")
true_ts_df <- subset(test_df, tomoa_skip == TRUE)
earliest_dates <- aggregate(start_date ~ full_name, data = true_ts_df, FUN = min)
test_df$days_since_first_skip <- as.numeric(test_df$start_date - earliest_dates$start_date[match(test_df$full_name, earliest_dates$full_name)])

days_since_skip_plot <- test_df |>
  pivot_longer(cols = c("best_qual", "final", "lane_a", "lane_b", "first_round", "quarter", "semi", "small_final", "big_final"), names_to = "round", values_to = "time") |>
  group_by(fname, lname, tomoa_skip, sex, event_id, year, start_date, days_since_first_skip) |>
  mutate(best_time = if (all(is.na(time))) NA else min(time, na.rm = TRUE)) |>
  group_by(fname, lname) |> 
  mutate(count = length(unique(event_id))) |>  
  filter(!is.na(best_time) & !is.na(days_since_first_skip) & count > 10 & time < 50) |>
  # filter(!is.na(days_since_first_skip) & count > 10 & time < 50) |> 
  filter(lname == "brosler" | fname == "reza" | fname == "veddriq") |>
  ggplot(labeller = days_since_labels) +
  geom_vline(aes(xintercept = 0), lty = 2, color = "red") +
  geom_point(aes(days_since_first_skip, time, group = paste(fname, lname)), alpha = 0.4) +
  geom_line(aes(days_since_first_skip, best_time, group = paste(fname, lname))) +
  
  facet_wrap(.~paste(fname, lname), labeller = as_labeller(days_since_labels)) +
  ggtitle("Best Time Per Event") +
  labs(x = "Days since first tomoa skip", y = "Best Time", title=NULL) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 11),
    strip.text = element_text(size = 13),
    legend.text = element_text(size = 13)
    ) +
  ylim(c(4.99, 8))




# Fall rate table
fall_rate_table <- m_times_update |> 
  filter(rank <= 16) |> 
  group_by(year) |> 
  summarise(
    num_falls_final_no_ts = sum(!tomoa_skip & fall_final, na.rm = TRUE), 
    num_falls_final_ts = sum(tomoa_skip & fall_final, na.rm = TRUE), 
    total_ts = sum(tomoa_skip),
    total_no_ts = sum(!tomoa_skip),
    num_events = length(unique(event_id))
  ) |> 
  mutate(fall_rate_no_ts = round(num_falls_final_no_ts / (total_no_ts * num_events), 3),
         fall_rate_ts = round(num_falls_final_ts / (total_ts * num_events), 3))

events |>
  group_by(year) |>
  summarise(
    num_events = n()
  )

# Save plots
ggsave("scripts/1_calculate_statistics/saved_plots/TS_users.png", plot = TS_Users, width = 12, height = 6, dpi = 400)
ggsave("scripts/1_calculate_statistics/saved_plots/days_since_skip.png", plot = days_since_skip_plot, width = 12, height = 6, dpi = 400)


  






