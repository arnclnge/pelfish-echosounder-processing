library(tidyverse)

###### create a date and time (HMS) sequence that will serve as input to extract tidal data using Clea P.'s bpns package (python)
##### echosounders ping every first 10 minutes of every hour
#### six weeks = 4M seconds and this is too large, we can do batch processing for every week...
### AP_Grafton_test: 7/13/2021 10:00 to 8/29/2021 23:10
## output: weekly csv of deployment (by the second)

setwd("~/PelFish/echogram processing/csv/tidal input/")

#------INPUTS
start_date <- as.POSIXct("7/13/2021", format = "%m/%d/%Y", tz="UTC")
end_date <- as.POSIXct("8/29/2021", format = "%m/%d/%Y", tz="UTC")
name <- "AP_Grafton_"
lat <- 51.4068
long <- 2.8182
interval <- "7 days" #how much data do you want in one csv file?

#------LOOP
week_interval <- seq(from= start_date, 
                 to= end_date,
                 by = interval) %>% append(end_date) #check that date cut-offs are correct

timestamp_list <- list()
timestamp_df <- data.frame(timestamp = as.POSIXct(NA, tz=UTC))

for (n in 1:(length(week_interval)-1)){

    timestamp_hr <- seq(from= week_interval[n], 
                        to=week_interval[n+1],
                        by = "hours")
    
    for (x in 1:length(timestamp_hr)){
      timestamp_min <- seq(from= timestamp_hr[x], 
                           length.out = 10, #we only want the first 10 mins of every hour
                           by = "mins")
      for (s in 1:length(timestamp_min)){
        timestamp_sec <- seq(from= timestamp_min[s], 
                             length.out = 60,
                             by = "secs")
        timestamp_list <- append(timestamp_list, timestamp_sec)
      }
    }
    
    #bind and export to csv  
    timestamp_df <- as.data.frame.POSIXct(timestamp_list)
    timestamp_df$Latitude <- lat
    timestamp_df$Longitude <- long
    out_file <- paste0(name,week_interval[n],"_",week_interval[n+1],".csv")
    write_csv(timestamp_df, out_file)
    print(paste0("Exported ", out_file))
    
    #empty lists & frames
    timestamp_list <- list()
    timestamp_df <- data.frame(timestamp = as.POSIXct(NA))
    
}

  


