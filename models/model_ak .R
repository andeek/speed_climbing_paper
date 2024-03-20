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
load("data/mTimes_skip.Rdata")
load("data/wTimes_skip.Rdata")

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

# models
m0 <- lmer(log(best_time) ~ (1 | full_name), data = reg_dat)
m1 <- lmer(log(best_time) ~ tomoa_skip + (1 | full_name), data = reg_dat)
m2 <- lmer(log(best_time) ~ tomoa_skip + (1 + tomoa_skip | full_name), data = reg_dat)

# compare models
anova(m0, m1, m2) ## models without sex

m3 <- lmer(log(best_time) ~ tomoa_skip + sex + (1 | full_name), data = reg_dat)
m4 <- lmer(log(best_time) ~ tomoa_skip + sex + (1 + tomoa_skip | full_name), data = reg_dat)

# compare models
anova(m0, m3, m4) ## models with sex

m0 <- lmer(log(best_time) ~ (1 | full_name) + (1 | event), data = reg_dat)
m5 <- lmer(log(best_time) ~ tomoa_skip + sex + (1 + tomoa_skip | full_name) + (1 | event), data = reg_dat)
m6 <- lmer(log(best_time) ~ tomoa_skip + sex + (1 + tomoa_skip | full_name) + (1 + tomoa_skip | event), data = reg_dat)

# compare models
anova(m0, m5, m6) ## models with sex and event
anova(m2, m4, m6) ## best of abovs, still nested. choose m6

## gamma glm with log link?
m7 <- glmer(best_time ~ tomoa_skip + sex + (1 + tomoa_skip | full_name) + (1 | event), family = Gamma(link = "log"), data = reg_dat)
m8 <- glmer(best_time ~ tomoa_skip + sex + (1 + tomoa_skip | full_name) + (1 + tomoa_skip | event), family = Gamma(link = "log"), data = reg_dat)

anova(m7, m8)

## box cox
# models
m0 <- lmer(1/best_time ~ (1 | full_name), data = reg_dat)
m9 <- lmer(1/best_time ~ tomoa_skip + (1 | full_name), data = reg_dat)
m10 <- lmer(1/best_time ~ tomoa_skip + (1 + tomoa_skip | full_name), data = reg_dat)

# compare models
anova(m0, m9, m10) ## models without sex

m11 <- lmer(1/best_time  ~ tomoa_skip + sex + (1 | full_name), data = reg_dat)
m12 <- lmer(1/best_time  ~ tomoa_skip + sex + (1 + tomoa_skip | full_name), data = reg_dat)

# compare models
anova(m0, m11, m12) ## models with sex

m0 <- lmer(1/best_time  ~ (1 | full_name) + (1 | event), data = reg_dat)
m13 <- lmer(1/best_time  ~ tomoa_skip + sex + (1 + tomoa_skip | full_name) + (1 | event), data = reg_dat)
m14 <- lmer(1/best_time  ~ tomoa_skip + sex + (1 + tomoa_skip | full_name) + (1 + tomoa_skip | event), data = reg_dat)

anova(m12, m13, m14)

## box cox 2
# models
m0 <- lmer(1/best_time^(3/2) ~ (1 | full_name), data = reg_dat)
m15 <- lmer(1/best_time^(3/2) ~ tomoa_skip + (1 | full_name), data = reg_dat)
m16 <- lmer(1/best_time^(3/2) ~ tomoa_skip + (1 + tomoa_skip | full_name), data = reg_dat)

# compare models
anova(m0, m15, m16) ## models without sex

m17 <- lmer(1/best_time^(3/2)  ~ tomoa_skip + sex + (1 | full_name), data = reg_dat)
m18 <- lmer(1/best_time^(3/2)  ~ tomoa_skip + sex + (1 + tomoa_skip | full_name), data = reg_dat)

# compare models
anova(m0, m17, m18) ## models with sex

m0 <- lmer(1/best_time^(3/2)  ~ (1 | full_name) + (1 | event), data = reg_dat)
m19 <- lmer(1/best_time^(3/2)  ~ tomoa_skip + sex + (1 + tomoa_skip | full_name) + (1 | event), data = reg_dat)
m20 <- lmer(1/best_time^(3/2)  ~ tomoa_skip + sex + (1 + tomoa_skip | full_name) + (1 + tomoa_skip | event), data = reg_dat)

anova(m18, m19, m20)

tidy(m6)
glance(m6)
tab_model(m6)

ggplot() +
  geom_point(aes(fitted(m6), resid(m6)))

qqnorm(resid(m6))
qqline(resid(m6))


data.frame(true = reg_dat$best_time, fitted = fitted(m8)) |>
  ggplot() +
  geom_point(aes(true, fitted)) +
  geom_abline(aes(intercept = 0, slope = 1), colour = "red")

data.frame(true = reg_dat$best_time, fitted = exp(fitted(m6))) |>
  ggplot() +
  geom_point(aes(true, fitted)) +
  geom_abline(aes(intercept = 0, slope = 1), colour = "red")


data.frame(true = reg_dat$best_time, fitted = 1/fitted(m14)) |>
  ggplot() +
  geom_point(aes(true, fitted)) +
  geom_abline(aes(intercept = 0, slope = 1), colour = "red")


data.frame(true = reg_dat$best_time, fitted = 1/fitted(m19)^(2/3)) |>
  ggplot() +
  geom_point(aes(true, fitted)) +
  geom_abline(aes(intercept = 0, slope = 1), colour = "red")

## MSE?
mean((reg_dat$best_time - exp(fitted(m6)))^2)
mean((reg_dat$best_time - fitted(m8))^2)
mean((reg_dat$best_time - 1/fitted(m14))^2)
mean((reg_dat$best_time - 1/fitted(m20)^(2/3))^2, na.rm = TRUE)

## LOOCV
## this doesn't work because there is a climber who is only in the data set once
## let's remove?
reg_dat |>
  group_by(full_name) |>
  summarise(count = n()) -> num_occur

cv_dat <- reg_dat |>
  filter(!full_name %in% num_occur[num_occur$count == 1, ]$full_name)

cl <- parallel::makeCluster(10)
doParallel::registerDoParallel(cl)

MSE <- foreach(i = seq_len(nrow(cv_dat)), .combine = 'rbind', .packages = c("lme4")) %dopar% {
  train <- cv_dat[-i,]
  test <- cv_dat[i,]
  
  m5 <- lmer(log(best_time) ~ tomoa_skip + sex + (1 + tomoa_skip | full_name) + (1 | event), data = train)
  m6 <- lmer(log(best_time) ~ tomoa_skip + sex + (1 + tomoa_skip | full_name) + (1 + tomoa_skip | event), data = train)
  m7 <- glmer(best_time ~ tomoa_skip + sex + (1 + tomoa_skip | full_name) + (1 | event), family = Gamma(link = "log"), data = train)
  m8 <- glmer(best_time ~ tomoa_skip + sex + (1 + tomoa_skip | full_name) + (1 + tomoa_skip | event), family = Gamma(link = "log"), data = train)
  m13 <- lmer(1/best_time  ~ tomoa_skip + sex + (1 + tomoa_skip | full_name) + (1 | event), data = train)
  m14 <- lmer(1/best_time  ~ tomoa_skip + sex + (1 + tomoa_skip | full_name) + (1 + tomoa_skip | event), data = train)
  
  pred5 <- predict(m5, test)
  pred6 <- predict(m6, test)
  pred7 <- predict(m7, test)
  pred8 <- predict(m8, test)
  pred13 <- predict(m13, test)
  pred14 <- predict(m14, test)
  
  MSE_linear <- cbind(mean((test$best_time - exp(fitted(m5)))^2), mean((test$best_time - exp(fitted(m6)))^2))
  MSE_gamma <- cbind(mean((test$best_time - fitted(m7))^2), mean((test$best_time - fitted(m8))^2))
  MSE_boxcox <- cbind(mean((test$best_time - 1/fitted(m13))^2), mean((test$best_time - 1/fitted(m14))^2))
  
  cbind(MSE_linear, MSE_gamma, MSE_boxcox)
}
parallel::stopCluster(cl)

MSE_bar <- colMeans(MSE, na.rm = TRUE)

## se
apply(MSE, 2, sd, na.rm = TRUE)

apply(MSE, 2, max, na.rm = TRUE)
apply(MSE, 2, function(x) which(x == max(x, na.rm = TRUE)))
cv_dat[1958,]git


sum(is.na(MSE[,1]))
nrow(MSE)
