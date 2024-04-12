library(RSelenium) ## connect to websites via selenium server
library(dplyr) ## data munge
library(tidyr) ## data tidy
library(lubridate) ## dates

## remote driver
driver <- rsDriver(browser = "chrome", chromever = "106.0.5249.21", verbose = TRUE, iedrver = NULL, geckover = NULL, phantomver = NULL)
##driver <- rsDriver(browser = "firefox", chromever = NULL, iedrver = NULL, verbose = TRUE)
remote_driver <- driver$client

## get events for each year
years <- 2012:2022
events <- data.frame()
for(y in years) {
  ## navigate to results
  remote_driver$navigate("https://components.ifsc-climbing.org/calendar")
  element_year <- remote_driver$findElement(using = "css", value = "select#yearSelect")
  Sys.sleep(2)
  
  ## choose year
  option_year <- remote_driver$findElement(using = 'xpath', paste0("//select[@id='yearSelect']/option[normalize-space(text())='", y, "']"))
  option_year$clickElement()
  Sys.sleep(2)
  
  ## choose league
  option_league <- remote_driver$findElement(using = 'xpath', "//select[@id='leaguesSelect']/option[normalize-space(text())='World Cups and World Championships']")
  option_league$clickElement()
  Sys.sleep(2)
  
  ## get event data for the chosen year
  comps <- remote_driver$findElements(using = "css", value = ".competition")
  comps_data <- data.frame()
  
  for(j in seq_along(comps)) {
    title <- comps[[j]]$findChildElement(using = "css", value = ".title")$findChildElement(using = "css", "a")
    date <- comps[[j]]$findChildElement(using = "css", value = ".date")
    links <- comps[[j]]$findChildElements(using = "css", value = ".tag")
    
    ## get mens and women's speed links
    links_data <- data.frame()
    for(k in seq_along(links)) {
      a_link <- links[[k]]$findChildElements(using = "css", "a")
      if(length(a_link) > 0) {
        a_txt <- a_link[[1]]$getElementText()[[1]]
        a_href <- a_link[[1]]$getElementAttribute("href")[[1]]
      } else {
        a_txt <- "missing_link"
        a_href <- NA
      }
      links_data <- bind_rows(links_data, data.frame(event = gsub(" " , "_", tolower(a_txt)), link = a_href))
    }
    if(nrow(links_data) == 0) {
      links_data <- data.frame(event_id = NA)
    } else {
      links_data |> 
        filter(grepl("speed", event)) |>
        separate(link, into = c("junk", "not_junk"), sep = "event=") |> 
        separate(not_junk, into = c("event_id", "junk2"), sep = "&") |>
        select(event_id) |>
        unique() -> links_data
    }
    if(nrow(links_data) == 0) links_data <- data.frame(event_id = NA)
    
    comps_data <- rbind(comps_data, data.frame(event = title$getElementText()[[1]], date = date$getElementText()[[1]], links_data))
  }
  
  ## clean up results into table
  comps_data |>
    separate(date, into = c("start_date", "end_date"), sep = " - ") |>
    mutate(end_date = gsub(paste0(" ", y), "", end_date)) |>
    mutate(year = y) |>
    bind_rows(events) -> events
}

## add event id
events |>
  mutate(start_date = lubridate::dmy(paste(start_date, year)),
         end_date = lubridate::dmy(paste(end_date, year))) |>
  select(event_id, year, start_date, end_date, event) -> events

## save events object
save(events, file = "data/events.Rdata")

## cleanup
remote_driver$close()
driver$server$stop()


