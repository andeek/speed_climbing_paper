library(dplyr)
library(lme4)
library(ggplot2)
library(visreg)
library(lattice)

## SETUP ##
m_times_update <- read.csv("data/mTimesAges.csv")
w_times_update <- read.csv("data/wTimesAges.csv")

# Get data into shape
falls <- mTimes_with_ages |> 
  mutate(full_name = paste(fname, lname, sep = " "),
         best_time = pmin(final, semi, best_qual, first_round, quarter, small_final, big_final, na.rm = TRUE), 
         sex = "m") |> 
  bind_rows(wTimes_with_ages |> 
              mutate(full_name = paste(fname, lname, sep = " "),
                     best_time = pmin(final, semi, best_qual, first_round, quarter, small_final, big_final, na.rm = TRUE), 
                     sex = "w")) |> 
  select(tomoa_skip, full_name, event, sex, best_time, age, time_progression, start_date, fall_final, fall_qual) |> 
  filter(complete.cases(tomoa_skip, full_name, event, sex, best_time, age, time_progression)) |> 
  mutate(time_progression = scale(time_progression)) |> 
  group_by(start_date, full_name, tomoa_skip, sex, event, age, time_progression) |> 
  summarise(total_falls = sum(fall_final + fall_qual, na.rm = TRUE), .groups = "drop") |> 
  filter(!is.na(total_falls) & !is.na(tomoa_skip))

# Distribution of falls (very unbalanced)
ggplot(falls, aes(x = factor(total_falls))) +
  geom_bar() +
  labs(x = "Total Falls", y = "Frequency") +
  ggtitle("Distribution of Total Falls")

# Simple logistic regression model
glm1 <- glm(total_falls ~ tomoa_skip, data = falls, family = binomial)
summary(glm1)

# Fit mixed effects models
m1 <- glmer(total_falls ~ tomoa_skip + sex + (1 + tomoa_skip | full_name) +
              (1 | event), data = falls, family = binomial)
m2 <- glmer(total_falls ~ tomoa_skip + sex + (1 + tomoa_skip | full_name) +
              (1 + tomoa_skip | event), data = falls, family = binomial)
m3 <- glmer(total_falls ~ tomoa_skip + sex + time_progression + age + (1 + tomoa_skip | full_name) + (1 + tomoa_skip | event),
            data=falls, family=binomial)


