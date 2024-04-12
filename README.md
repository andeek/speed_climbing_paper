# About
This is the code compendium for "The Fast and the Furious: Tracking the Effect of the Tomoa Skip on Speed Climbing" by Caleb Chou and Andee Kaplan. The following headers correspond to the different directories included in this repository.

# get_data
This directory contains all the web scrapers that we developed to scrape the IFSC website for data. The IFSC website has changed significantly since the use of these web scrapers. As of this date, these scripts are not compatible with the current iteration of the IFSC website, however, they are included in this repository for posterity. The current iteration of the IFSC website's event pages can be found at the [IFSC Climbing Calendar](https://www.ifsc-climbing.org/calendar/index) and the [Event Result Service Page](https://ifsc.results.info/#/). The data that we collected from these scripts can be found in the **Data** directory.

**get_events.R:** This file contains the webscraper used to scrape information about speed climbing events.

**get_times.R:** This file contains the webscraper used to navigate to each event page and store it's results.

**save_climbers.R:** This file contains a script that creates csv files of mens and womans speed climbers. It then uploads these CSV's to a Google drive file that we used to manually input which climbers use the Tomoa Skip.

**get_skip_data.R:** This file contains a script that iterates through our datasets and marks climbers as using the Tomoa Skip or not using the Tomoa Skip.

# data 
This directory contains the various data files used. **ts_data_[gender].csv** contains our data regarding which climbers use the Tomoa Skip. The **events.Rdata** file contains the scraped events from the IFSC website. The rest of the files in this directory contain the result data for mens and womens speed climbing compeitions. The Rdata files with the **_skip** postfix contain this same result data with the Tomoa Skip information for each climber added.

# visualizations
This directory contains all plots and tables used in our paper. **plots.r** contains the R code used to create each plot, and **tables.rmd** contains the R code and markdown used to render each table used in the paper. 

# models
This directory contains the models discussed in the paper.

**falls_model.R:** This file contains the model used to assess the variability of the Tomoa Skip.

**ts_model.R:** This file contains the model used to assess the effect of the Tomoa Skip on speed climbing times.

# paper
This directory contains the markdown file of the paper.

