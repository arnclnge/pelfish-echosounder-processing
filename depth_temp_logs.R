setwd("~/PelFish/echogram processing/")

#CHANGE INPUTS
raw <- read.delim("AP_Grafton2021-receiver_logs2.txt", header = F)
date_start = as.Date("2021-07-13")
date_end = as.Date("2021-07-16")
datetime_start = as.POSIXct("2021-07-13 00:00:00", tz = "UTC")
datetime_end = as.POSIXct("2021-07-17 00:00:00", tz="UTC")
average_depth = 19.140766 #in meters

######################################

###---EXTRACTING DEPTH VALUES FROM TXT FILE

#separate string into multiple columns
df <- raw %>% extract(V1, into = str_c("Split", 1:4), "(..........)(.)(........)(.*?)$")
df <- df %>% separate_wider_regex(Split4, c(data_class = ".*?", "-", data = ".*")) %>% rename(Depth_date= Split1, Depth_time = Split3)

#extract depth data
df <- df %>% filter(data_class == " DEPTH ")
df <- df %>% extract(data, into = str_c("data", 1:2), "(.....................)(.*?)$") %>% 
              separate_wider_regex(data2, c(value = ".*?", ",", rest = ".*")) %>% 
              select (-data1,-rest, -data_class, -rest, -Split2) %>% rename (Depth_meters = value)

#select dates to export
df <- df %>% filter(Depth_date >= date_start & Depth_date <= date_end)

#format depth to heave values, add 1 m which is the ~ height of the mooring
df$Depth_meters <- as.numeric(df$Depth_meters) 
df <- df %>% mutate(Depth_meters = Depth_meters +1 - average_depth) %>% mutate(Depth_meters = 0-Depth_meters)

###---CREATE DATAFRAME BY THE SECOND

hour_interval <- seq(from= datetime_start, to= datetime_end, by="hour")  #check that date cut-offs are correct

timestamp_df <- data.frame(timestamp = as.POSIXct(NA, tz=UTC))

for (n in 1:(length(hour_interval))){
  
  timestamp_min <- seq(from= hour_interval[n], 
                       length.out = 11, #we only want the first 10 mins of every hour
                       by = "mins") 
  
  for (s in 1:length(timestamp_min)){
    timestamp_sec <- seq(from= timestamp_min[s], 
                         length.out = 60,
                         by = "secs") %>% as.data.frame() 
    colnames(timestamp_sec)[1]="timestamp"
    timestamp_df <- dplyr::bind_rows(timestamp_df, timestamp_sec)
  }
}

timestamp_df <- timestamp_df %>% na.omit()

###---ADD DEPTH VALUES TO THE DATAFRAME CREATED

timestamp_df$Depth_date <- as.Date(timestamp_df$timestamp)
timestamp_df$Depth_time <- format(as.POSIXct(timestamp_df$timestamp), format = "%H:%M:%S", tz="UTC") 
timestamp_df$Depth_hour <- format(as.POSIXct(timestamp_df$timestamp), format = "%H", tz="UTC") 

df$Depth_hour <- substr(df$Depth_time, start = 1, stop = 2)

timestamp_df$Depth_meters <- df$Depth_meters[match(timestamp_df$Depth_hour,df$Depth_hour)]

#remove unnecessary columns
timestamp_df <- timestamp_df %>% mutate(Depth_status = 0) %>% select(-timestamp, -Depth_hour)

write_csv(timestamp_df,"csv/heave correction/AP_Grafton_2021_depth_July13-July16.depth.csv")
