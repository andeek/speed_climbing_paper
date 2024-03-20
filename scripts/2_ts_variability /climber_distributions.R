library(ggplot2)
library(dplyr)
library(tidyr)

load("data/mTimes_skip.Rdata")
load("data/wTimes_skip.Rdata")


# Look at distributions of specific climber before and after skip

names_to_search = c("john brosler", "veddriq leonardo", "reza alipour shenazandifard", 
            "danyil boldyrev", "aspar aspar", "aleksandra miroslaw",  "natalia kalucka",
            "anna brozek")

m_times_update$full_name <- paste(m_times_update$fname, m_times_update$lname, sep = " ")
w_times_update$full_name <- paste(w_times_update$fname, w_times_update$lname, sep = " ")

m_times_update <- m_times_update |>
  mutate(best_time = pmin(final, semi, best_qual, first_round, quarter, small_final, big_final, na.rm = TRUE))

w_times_update <- w_times_update |>
  mutate(best_time = pmin(final, semi, best_qual, first_round, quarter, small_final, big_final, na.rm = TRUE))

m_times_update |> mutate(sex = "m") |>
  bind_rows(w_times_update |> mutate(sex = "w")) -> all_climbers

climbers <- all_climbers[all_climbers$full_name %in% names_to_search, ]

create_distribution_plots <- function(climber_data, climber_name) {
  climber_data <- climber_data[!is.na(climber_data$best_time),]
  ggplot(climber_data, aes(x = best_time, fill = tomoa_skip)) +
    geom_density(alpha = 0.5) +
    ggtitle(paste("Distribution of Times for", climber_name)) +
    xlab("Best Round Time") +
    ylab("Density") +
    theme_minimal() + 
    xlim(min(climber_data$best_time), max(climber_data$best_time))
}

for (climber_name in names_to_search) {
  climber <- climbers[climbers$full_name == climber_name, ]
  
  if (nrow(climber) > 0) {
    plot_m <- create_distribution_plots(climber, climber_name)
    print(plot_m)
  }
  
}
