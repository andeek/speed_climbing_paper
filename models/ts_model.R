library(lme4)
library(dplyr)
library(lattice)
library(tidyr)
library(broom.mixed)
library(ggplot2)
library(performance)
library(sjPlot)

mTimes_with_ages <- read.csv("data/mTimesAges.csv")
wTimes_with_ages <- read.csv("data/wTimesAges.csv")

# Add full name
mTimes_with_ages$full_name <- paste(mTimes_with_ages$fname, mTimes_with_ages$lname, sep = " ")
wTimes_with_ages$full_name <- paste(wTimes_with_ages$fname, wTimes_with_ages$lname, sep = " ")

# Add best time column
mTimes_with_ages <- mTimes_with_ages |>
  mutate(best_time = pmin(final, semi, best_qual, first_round, quarter, small_final, big_final, na.rm = TRUE))

wTimes_with_ages <- wTimes_with_ages |>
  mutate(best_time = pmin(final, semi, best_qual, first_round, quarter, small_final, big_final, na.rm = TRUE))

mTimes_with_ages |> mutate(sex = "m") |>
  bind_rows(wTimes_with_ages |> mutate(sex = "w")) -> times

times |> select(tomoa_skip, full_name, event, sex, best_time, age, time_progression) -> reg_times
reg_times <- reg_times[complete.cases(reg_times), ]

times |> select(tomoa_skip, full_name, event, sex, best_time, age, time_progression) -> reg_dat
reg_dat[complete.cases(reg_dat),] -> reg_dat

reg_dat <- reg_dat |>
  mutate(
    time_progression = scale(time_progression)
  )


m00 <- lmer(log(best_time) ~ (1 | full_name), data = reg_dat, REML = FALSE)
m0 <- lmer(log(best_time) ~ (1 + tomoa_skip | full_name) + (1 + tomoa_skip | event), data = reg_dat, REML = FALSE)
m1 <- lmer(log(best_time) ~ sex + (1 + tomoa_skip | full_name) + (1 + tomoa_skip | event), data = reg_dat, REML = FALSE)
m2 <- lmer(log(best_time) ~ tomoa_skip + sex + time_progression + age + (1 + tomoa_skip | full_name) + (1 + tomoa_skip | event), data = reg_dat, REML = FALSE)
m3 <- lmer(log(best_time) ~ tomoa_skip*age + sex + time_progression + (1 + tomoa_skip | full_name) + (1 + tomoa_skip | event), data = reg_dat, REML = FALSE)

anova(m00, m0, m1, m2, m3) |>
  tidy()


