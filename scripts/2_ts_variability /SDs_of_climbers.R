library(ggplot2)
library(dplyr)
library(tidyr)

load("data/mTimes_skip.Rdata")
load("data/wTimes_skip.Rdata")

# Calculate SDs of climbers best round times before and after using the skip

# Setup
m_times_update$full_name <- paste(m_times_update$fname, m_times_update$lname, sep = " ")
w_times_update$full_name <- paste(w_times_update$fname, w_times_update$lname, sep = " ")

m_times_update |> mutate(sex = "m") |>
  bind_rows(w_times_update |> mutate(sex = "w")) -> all_climbers


all_climbers <- all_climbers |>
  mutate(final = ifelse(!is.na(final) & (!is.na(small_final) | !is.na(first_round) | !is.na(quarter) | 
                                           !is.na(semi) | !is.na(small_final) | !is.na(big_final)), NA, final)) |>
  mutate(best_qual = ifelse(!is.na(best_qual) & (!is.na(lane_a) | !is.na(lane_b)), NA, best_qual)) |>
  pivot_longer(cols = c("best_qual", "final", "lane_a", "lane_b", "first_round",
                      "quarter", "semi", "small_final", "big_final"), names_to = "round", values_to = "time") |>
  filter(!is.na(time))

# Calculate standard deviations for climbers before and after using TS
sds <- aggregate(time ~ full_name + tomoa_skip + sex, data = all_climbers, FUN = sd)
names(sds)[names(sds) == "time"] <- "sds"
sds <- na.omit(sds)

# Reshape data and get differences
sds_wide <- reshape(sds, idvar = c("full_name", "sex"),
                    time = "sds", v.names = "sds", timevar = "tomoa_skip", direction = "wide")
sds_wide <- na.omit(sds_wide)
sds_wide$sd_dif <- sds_wide$sds.TRUE - sds_wide$sds.FALSE


# Plot results
ggplot(sds_wide, aes(x = sds.FALSE, y = sds.TRUE)) +
  geom_abline(aes(intercept = 0, slope = 1), lty = 2) +
  geom_point(alpha = 0.8) +
  labs(x = "Standard Deviation (No Tomoa Skip)", y = "Standard Deviation (Tomoa Skip)") +
  theme_minimal() + 
  # coord_fixed()
  coord_cartesian(xlim = c(min(sds_wide$sds.FALSE), max(sds_wide$sds.FALSE)),
                  ylim = c(min(sds_wide$sds.TRUE), max(sds_wide$sds.TRUE)))

# Same plot with limited axis
ggplot(sds_wide, aes(x = sds.FALSE, y = sds.TRUE)) +
  geom_abline(aes(intercept = 0, slope = 1), lty = 2) +
  geom_point(alpha = 0.8) +
  labs(x = "Standard Deviation (No Tomoa Skip)", y = "Standard Deviation (Tomoa Skip)") +
  theme_minimal() + 
  coord_cartesian(xlim = c(0, 5), ylim = c(0, 2))
