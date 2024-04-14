library(lme4)
library(dplyr)
library(lattice)
library(tidyr)
library(broom.mixed)
library(ggplot2)
library(performance)
library(sjPlot)
library(rstanarm)
library(foreach)
library(parallel)


## SETUP ##
m_times_update <- read.csv("data/mTimes_skip.csv")
w_times_update <- read.csv("data/wTimes_skip.csv")

# Add full name
m_times_update$full_name <- paste(m_times_update$fname, m_times_update$lname, sep = " ")
w_times_update$full_name <- paste(w_times_update$fname, w_times_update$lname, sep = " ")

# Add best time column
m_times_update <- m_times_update |>
  mutate(best_time = pmin(final, semi, best_qual, first_round, quarter, small_final, big_final, na.rm = TRUE))

w_times_update <- w_times_update |>
  mutate(best_time = pmin(final, semi, best_qual, first_round, quarter, small_final, big_final, na.rm = TRUE))

m_times_update |> mutate(sex = "m") |>
  bind_rows(w_times_update |> mutate(sex = "w")) -> times

times |> select(tomoa_skip, full_name, event, sex, best_time) -> reg_times
reg_times <- reg_times[complete.cases(reg_times), ]

times |> select(sex, best_time, tomoa_skip, full_name, event) -> reg_dat
reg_dat[complete.cases(reg_dat),] -> reg_dat

times |>
  mutate(log_best_time = log(best_time),
         sqrt_best_time = sqrt(best_time)) |>
  select(tomoa_skip, full_name, event, sex, log_best_time, sqrt_best_time) |> 
  filter(!is.na(tomoa_skip)) |>
  pivot_longer(cols = ends_with("best_time"), names_to = "transform", values_to = "best_time") |>
  ggplot() +
  geom_histogram(aes(best_time, fill = tomoa_skip), position = "dodge", binwidth = .1) +
  facet_grid(sex ~ transform)

MASS::boxcox(lm(best_time ~ 1, data = reg_dat)) ## lambda = -1 or -2 => transformation = 1/x or 1/x^2

## no transformation
times |>
  filter(!is.na(tomoa_skip)) |>
  ggplot() +
  geom_histogram(aes(best_time, fill = tomoa_skip), position = "dodge", binwidth = .1) +
  facet_grid(sex ~ .)


# Model
model <- lmer(log(best_time) ~ tomoa_skip + sex + (1 + tomoa_skip | full_name) + (1 + tomoa_skip | event), data = reg_dat)

tidy(model)
glance(model)
tab_model(model)

ggplot() +
  geom_point(aes(fitted(model), resid(model)))

qqnorm(resid(model))
qqline(resid(model))


data.frame(true = reg_dat$best_time, fitted = exp(fitted(model))) |>
  ggplot() +
  geom_point(aes(true, fitted)) +
  geom_abline(aes(intercept = 0, slope = 1), colour = "red")


## MSE
mean((reg_dat$best_time - exp(fitted(model)))^2)
