###############################################
#   The IFSC website has changed significantly since the use of these web scrapers. 
#   As of this date, these scripts are not compatible with the current iteration of the IFSC website,
#   however, they are included in this repository for posterity. The current iteration of the IFSC website's 
#   event pages can be found at the IFSC Climbing Calendar (https://www.ifsc-climbing.org/calendar/index) and 
#   the Event Result Service Page (https://ifsc.results.info/#/). The data that we collected from these scripts 
#   can be found in the Datadirectory.
###############################################


# Required libraries
library(dplyr)
library(googlesheets4)
library(r2r)

# Import data from Google Drive
m_tomoa_skip_data <- read_sheet('https://docs.google.com/spreadsheets/d/15vR-ZX_4U7oRExClp_SWz88g3hWKRGjETQssIACAgSk/edit?usp=sharing', col_names = TRUE, col_types = "dcTTddcccl")
w_tomoa_skip_data <- read_sheet('https://docs.google.com/spreadsheets/d/19m6dNDvdVuqtaQsVS0mt8r2iElL7Pe95vkm6cWiPq_E/edit?usp=sharing', col_names = TRUE, col_types = "dcTTddcccl")

# load
load("data/mTimes.Rdata")
load("data/wTimes.Rdata")
load("data/events.Rdata")

#make copies to update to (easier for testing)
m_times_update <- m_times
w_times_update <- w_times

# Join imported data into overall times for men and women
m_times_update <- m_times |>
  mutate(event_id = as.numeric(event_id)) |>
  left_join(
            m_tomoa_skip_data |> select(fname, lname, event_id, tomoa_skip),
            by = c("fname", "lname", "event_id"),
            ) |>
  mutate(tomoa_skip = ifelse(tomoa_skip == "NULL", NA, tomoa_skip))

w_times_update <- w_times |>
  mutate(event_id = as.numeric(event_id)) |>
  left_join(
    w_tomoa_skip_data |>
      select(fname, lname, event_id, tomoa_skip),
    by = c("fname", "lname", "event_id"),
  ) |>
  mutate(tomoa_skip = ifelse(tomoa_skip == "NULL", NA, tomoa_skip))

#Join with events to get years and dates
m_times_update <- m_times_update |>
  left_join(events |> mutate(event_id = as.numeric(event_id)), by="event_id")
w_times_update <- w_times_update |>
  left_join(events |> mutate(event_id = as.numeric(event_id)), by = "event_id")

# Create empty hashmaps for mens and womens
m_hm <- hashmap()
w_hm <- hashmap()

# The earliest date we have data for is 2018-04-21, so can assume all earlier events as false
m_times_update$tomoa_skip[as.double(m_times_update$start_date) < as.Date("2018-04-21")] <- FALSE
w_times_update$tomoa_skip[as.double(w_times_update$start_date) < as.Date("2018-04-21")] <- FALSE

# Sort the merged data set by event id so we can properly compare event IDs
m_times_update <-  m_times_update[order((m_times_update$start_date)), ]
w_times_update <- w_times_update[order((w_times_update$start_date)), ]

# For mens data
# Current type is string, so convert to bool
m_times_update$tomoa_skip <- as.logical(m_times_update$tomoa_skip)
for(i in 1:nrow(m_times_update)){
  
  row <- m_times_update[i, ]
  full_name <- paste(row$fname, row$lname, sep = " ")
  
  # Check if tomoa skip is true and querying the climber's name in the hashmap is not null.
  # If so, this is the first time we know the climber performs the Tomoa skip so add to hashmap
  if (!is.na(row$tomoa_skip) && row$tomoa_skip && is.null(query(m_hm, full_name))) {
    insert(m_hm, full_name, row$start_date)
  }
  
  # Checks if tomoa skip is not NA and FALSE. Additionally, checks if the query to the hm is NOT null.
  # This is for the rare case that a climber uses the skip and then later switches to not using the skip.
  # This conditional will "reset" their value in the hm so we can fill in further dates up to their 
  # next recorded skip as FALSE.
  else if(!is.na(row$tomoa_skip) && !row$tomoa_skip && !is.null(query(m_hm, full_name))) {
    insert(m_hm, full_name, NULL)
    m_times_update[i, "tomoa_skip"] <- FALSE
    print("Happened")
  }
  
  # Checks if value is NA and the query returns null. Then check if we have gathered data on this climber.
  # If so, set skip val to false.
  else if(is.na(row$tomoa_skip) && is.null(query(m_hm, full_name))
          && (row$fname %in% m_tomoa_skip_data$fname) && (row$lname %in% m_tomoa_skip_data$lname)){
    m_times_update[i, "tomoa_skip"] <- FALSE
  }
  
  # If query is not null, compare start date values
  else if (!is.null(query(m_hm, full_name))) {
    # Compare current start_date to query start_date
    if (as.double(row$start_date) >= as.double(query(m_hm, full_name))) {
      m_times_update[i, "tomoa_skip"] <- TRUE
    }
  }
}


# For womens data (works the same)
w_times_update$tomoa_skip <- as.logical(w_times_update$tomoa_skip)
for(i in 1:nrow(w_times_update)){
  
  row <- w_times_update[i, ]
  full_name <- paste(row$fname, row$lname, sep = " ")

  if (!is.na(row$tomoa_skip) && row$tomoa_skip && is.null(query(w_hm, full_name))) {
    insert(w_hm, full_name, row$start_date)
  }
  else if(!is.na(row$tomoa_skip) && !row$tomoa_skip && !is.null(query(w_hm, full_name))) {
    insert(w_hm, full_name, NULL)
    w_times_update[i, "tomoa_skip"] <- FALSE
  }
  else if(is.na(row$tomoa_skip) && is.null(query(w_hm, full_name))
          && (row$fname %in% w_tomoa_skip_data$fname) && (row$lname %in% w_tomoa_skip_data$lname)){
    w_times_update[i, "tomoa_skip"] <- FALSE
  }

  else if (!is.null(query(w_hm, full_name))) {
    if (as.double(row$start_date) >= as.double(query(w_hm, full_name))) {
      w_times_update[i, "tomoa_skip"] <- TRUE
    }
  }
}

## Mark 999 final values as NA
m_times_update$final <- ifelse(m_times_update$final == 999, NA, m_times_update$final)

## save objects
save(m_times_update, file = "data/mTimes_skip.Rdata")
save(w_times_update, file = "data/wTimes_skip.Rdata")





