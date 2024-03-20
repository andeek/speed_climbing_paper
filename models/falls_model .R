library(dplyr)
library(lme4)
library(ggplot2)
library(visreg)
library(lattice)

## SETUP ##
load("data/mTimes_skip.Rdata")
load("data/wTimes_skip.Rdata")

# Add full name
m_times_update$full_name <- paste(m_times_update$fname, m_times_update$lname, sep = " ")
w_times_update$full_name <- paste(w_times_update$fname, w_times_update$lname, sep = " ")

m_times_update |> mutate(sex = "m") |>
  bind_rows(w_times_update |> mutate(sex = "w")) -> falls 

# Get falls per date/event
falls <- falls |>
  group_by(start_date, full_name, tomoa_skip, sex, event) |>
  summarise(total_falls = sum(fall_final + fall_qual)) |>
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
# This model is very similar to the one used to examine our question of the Tomoa skip's effect
# on speed climbing times
m2 <- glmer(total_falls ~ tomoa_skip + sex + (1 + tomoa_skip | full_name) +
              (1 + tomoa_skip | event), data = falls, family = binomial)

summary(m1)
summary(m2)

# Examine assumptions
# Partial residual plot
#visreg(m2, "tomoa_skip", scale = "response")
# QQ plot of random effects
qqmath(ranef(m2))


