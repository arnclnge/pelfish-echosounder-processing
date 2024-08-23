library(tidyverse)

setwd("C:/Users/arienne.calonge/OneDrive - VLIZ/Ari/PelFish/R/")
files_dir = "C:/Users/arienne.calonge/OneDrive - VLIZ/Ari/PelFish/R/csv/AP_Grafton/"

##Biomass calculation

# 1. Calculate TS = 20log10(mean fish length) + b20 and mean acoustic backscattering cross-section 

catch_summary = "csv/APELAFICO Pelagic Survey  (8-9 August 2023).csv"
catch_raw_xls = "csv/Biotic_NLAPELAG_2023.csv"
HaulNo = "5000975"

catch_sum <- read_csv(catch_summary) %>% filter(HaulNumber == HaulNo, CatchSpeciesWeightProportion > 5)
species = as.vector(catch_sum[['CatchSpeciesScientificName']]) #clupeids 
speciesEng = as.vector(catch_sum[['EngName']])

catch_raw <- read_csv(catch_raw_xls)
row_start = 6
catch_raw = catch_raw[row_start:nrow(catch_raw),1:22] 
names(catch_raw) <- catch_raw[1,]
catch_raw <- catch_raw[-1,]
catch_raw <- catch_raw %>% mutate_at(c('CatchSpeciesCategoryNumber', 'CatchSpeciesCategoryWeight', 'CatchSubsamplingFactor', 'CatchSubsampledNumber', 'CatchSubsampleWeight', 'CatchLengthClass', 'CatchNumberAtLength'), as.numeric) %>%
                            filter(HaulNumber == HaulNo, CatchSpeciesScientificName %in% species) %>% 
                              mutate(CatchLengthClass = ifelse(CatchLengthCode=="halfcm",CatchLengthClass/10, CatchLengthClass)) 

catch_raw$EngName = recode(catch_raw$CatchSpeciesScientificName, "Clupea harengus"="Atlantic herring", "Merlangius merlangus"="Whiting",
                               "Scomber scombrus"="Atlantic mackerel", "Sardina pilchardus"="Pilchard",
                               "Sprattus sprattus" = "Sprat", "Mustelus asterias" = "Starry smooth-hound",
                               "Alloteuthis subulata" = "Common squid", "Trachurus trachurus" = "Atlantic horse mackerel",
                               "Engraulis encrasicolus"="Anchovy")

#---Assign variable to species---#

for (i in 1:nrow(catch_sum)){
  var = paste0('sp',i) 
  assign(var,catch_sum$EngName[i])
}

#####################################################################################################

# 1.a. Get mean weight based on Bayesian length-weight relationship (a & b constants from Fishbase.se)

catch_raw <- catch_raw  %>% mutate(a = case_when(EngName == "Sprat" ~ 0.00575,
                                                  EngName == "Atlantic herring" ~ 0.00562,
                                                  EngName == "Pilchard" ~ 0.00676,
                                                  EngName == "Anchovy" ~ 0.00468),
                                    b = case_when(EngName == "Sprat" ~ 3.09,
                                                  EngName == "Atlantic herring" ~ 3.09,
                                                  EngName == "Pilchard" ~ 3.06,
                                                  EngName == "Anchovy" ~ 3.1),
                                    ind_weight = a*(CatchLengthClass)^b,
                                    weightCat = ind_weight*CatchNumberAtLength)

df_values = catch_raw %>% group_by(EngName) %>% summarize(mean_weight = sum(weightCat)/sum(CatchNumberAtLength)) #grams
weight_list = with(df_values, setNames(mean_weight, EngName))

# 1.b. Get mean lengths of each species

df_values$mean_length = catch_raw %>% group_by(EngName) %>% summarize(mean(CatchLengthClass)) %>% pull() #cm
length_list = with(df_values, setNames(mean_length, EngName))

# 1.c. Get proportions of species 

df_values$proportion = catch_sum %>% group_by(EngName) %>% pull(CatchSpeciesWeightProportion)
proportion_list = with(catch_sum, setNames(CatchSpeciesWeightProportion, EngName))

# 1.d. Calculate TS & Sbs

b20 <- -71.2 #b20 of clupeids, ICES survey standard, https://ices-library.figshare.com/articles/report/SISP_9_-_Manual_for_International_Pelagic_Surveys_IPS_/19050794

#TS and Sigma backscattering values of each species
df_values$TS = NA
df_values$Sbs = NA

for (i in 0:nrow(df_values)){
  df_values$TS[i] = 20*log10(df_values$mean_length[i])+b20
  df_values$Sbs = 10^(df_values$TS/10)
}

# 2. Calculate NASC percentage according to catch data. Calculate abundance per square kilometer

files_cells = list.files(path=files_dir, pattern = "(cells)", full.names=TRUE)
files_intervals = list.files(path=files_dir, pattern = "(intervals)", full.names=TRUE)

#merge cells and intervals

df_station = data.frame()

for (i in 1:length(files_cells)){
  cells = read.csv(files_cells[i])
  intervals = read.csv(files_intervals[i])
  df_fileset = merge(cells, intervals, all=T)
  df_station = rbind(df_station, df_fileset)
}

df_station[df_station$PRC_NASC== -9999,]$PRC_NASC=0
df_station = df_station %>% filter(Layer == 1)

df2_station <- df_station %>%  group_by(Interval) %>% summarize(PRC_NASC = mean(PRC_NASC), PRC_ABC = PRC_NASC / (4*pi*1852^2)) 

#create columns in df for all variables needed to calculate the biomass
for (i in 1:nrow(catch_sum)){
    df2_station[paste0('PRC_ABC_',speciesEng[i])] <- NA
    df2_station[paste0('Abundance_',speciesEng[i])] <- NA
    df2_station[paste0('Biomass_',speciesEng[i])] <- NA
}
                                                                
for (i in 1:length(species)){
  #assign variables
  PRC_ABC_sp = paste0('PRC_ABC_',speciesEng[i])
  Abundance_sp = paste0('Abundance_',speciesEng[i])
  Biomass_sp = paste0('Biomass_',speciesEng[i])
  
  Proportion_sp = df_values$proportion[i]/100 #convert percent to decimal
  Sbs_sp = df_values$Sbs[i]
  Weight_sp = df_values$mean_weight[i]
  
  #calculate values
  df2_station[PRC_ABC_sp] = df2_station$PRC_ABC*Proportion_sp
  df2_station[Abundance_sp] =  (df2_station[PRC_ABC_sp]/Sbs_sp) #to convert square meters to square kilometers, multiply (10^6)
  df2_station[Biomass_sp] = df2_station[Abundance_sp]*Weight_sp #to convert grams to tonnes *0.001*0.001
}                  

#calculate total biomass

df2_station = df2_station %>% mutate(Total_biomass = select(., starts_with("Biomass")) %>% rowSums())


# 3. Plot time series

#prepare date and hour
df_station$Date <- as.Date(as.character(df_station$Date_M), format= "%Y%m%d")
df_station$Week <- round_date(df_station$Week, "7 days")
df_station_dates <- df_station %>% select(Interval, Date) #Week or Date

df_station$Time_M = str_sub(df_station$Time_M, 2, -6)
df_station$Hour <- format(strptime(df_station$Time_M,"%H:%M:%S"),'%H')

df2_station <- merge(df2_station, df_station_dates, by = "Interval", all.x = TRUE)
df2_station_filtered <- df2_station %>% filter(!Interval == 452649& !Interval ==451788 & !Interval ==452647 & !Interval ==452213 & !Interval ==452389 & !Interval ==451715)


#Plot total biomass, 700 x 400, multiply by 1000 tonnes/sqkm
df2_station_filtered %>% group_by(Date) %>% summarize(meanBiomass = mean(Total_biomass)) %>% #default units of Total biomass is grams per sqm or tons per sqkm. 
  ggplot(aes(x=Date)) + geom_line(aes(y=meanBiomass, color = "red", size = ))+
  #scale_y_log10()+
  labs(y="Average acoustic biomass density (tons/sqkm)")+theme_minimal()+theme(legend.position="none")+
  ggtitle("Station: APELAFICO-Grafton 2021 (WBAT 70 kHz) \nCase: 85% Sprat, 9% Atlantic Herring")


###################

#---SCHOOLS CHARACTERIZATION

#Plot number of schools
df_station_filtered = df_station %>% filter(!Interval == 452649& !Interval ==451788 & !Interval ==452647 & !Interval ==452213 & !Interval ==452389 & !Interval ==451715)

df_station_filtered %>% group_by(Date) %>% filter(!Region_ID== -9999) %>% summarize(NumberSchools=n_distinct(Region_ID)) %>% 
  ggplot(aes(x=Date)) + geom_line(aes(y=NumberSchools))+
  labs(y="Number of schools detected")+theme_minimal()+
  ggtitle("Station: APELAFICO-Grafton 2021 (WBAT 70 kHz)")

#remove outliers?

#Plot schools over 24 hours
df_station_filtered %>% group_by(Hour) %>% filter(!Region_ID= -9999) %>% summarize(NumberSchools=n_distinct(Region_ID)) %>% 
  ggplot(aes(x=Hour)) + geom_col(aes(y=NumberSchools))+
  labs(y="Number of schools detected")+theme_minimal()+theme(legend.position="none")

#boxplot of schools
NumberSchools <- Grafton_raw %>% group_by(Date,Hour) %>% summarize(NumberSchools=n_distinct(Region_ID))
boxplot(Grafton_raw$Region ~ Grafton_raw$Sport, las = 2, xlab = "")

#School size and school depth over 24 hours

#Schools depth
Grafton_raw %>% ggplot(aes(x=Hour)) + geom_point(aes(y=Range_mean))+
  theme_minimal()+labs(y="Distance from the transducer (m)")
par(cex.lab=0.9)
boxplot(Grafton_raw$Range_mean ~ Grafton_raw$Hour, xlab = "Hour", ylab = "Vertical distance from the transducer (m)")

#Depth profiles: size, aggregation, schools
Grafton_raw$Range_mean_round = round(Grafton_raw$Range_mean, digits=0)

Grafton_raw %>% group_by(Range_mean_round) %>% summarize(Height_mean_by_range = mean(Height_mean)) %>% 
  ggplot(aes(x=Height_mean_by_range))+ geom_point(aes(y=Range_mean_round))

x <- Grafton_raw %>% group_by(Range_mean_round) %>% summarize(Height_mean_by_range = mean(Height_mean)) 
y <- Grafton_raw %>% group_by(Range_mean_round) %>% summarize(Number_schools = n_distinct(Region_ID)) 
z <- Grafton_raw %>% group_by(Range_mean_round) %>% summarize(Mean_Aggregation_index = mean(Aggregation_index)) 

#DEPTH vs. MEAN SCHOOL HEIGHT
ggplot()+ geom_point(data=Grafton_raw, aes(x=Range_mean_round, y=Height_mean), color="lightgrey", size=0.5)+
  #set up the asthetics, the line must be broken because this is technically discrete data
  geom_line(data=x, aes(x=Range_mean_round,y = Height_mean_by_range),linewidth=0.5, linetype=2)+
  geom_point(data=x, aes(x=Range_mean_round,y = Height_mean_by_range))+
  #put the y axis labes on the opposite side so when its flipped it will appear at top
  scale_y_continuous(position="right")+
  labs(y="Mean acoustic school height (m)", x="Vertical distance from the transducer (m)")+
  coord_flip(ylim = c(0,6))+
  theme_minimal()+
  theme(axis.title=element_text(size=9))

#DEPTH vs. NUMBER OF SCHOOLS
ggplot()+ geom_point(data=y, aes(x=Range_mean_round, y=Number_schools))+
  #set up the asthetics, the line must be broken because this is technically discrete data
  geom_line(data=y, aes(x=Range_mean_round, y=Number_schools),linewidth=0.5, linetype=2)+
  #put the y axis labes on the opposite side so when its flipped it will appear at top
  scale_y_continuous(position="right")+
  labs(y="Number of schools detected", x="Vertical distance from the transducer (m)")+
  coord_flip(ylim = c(0,max(y$Number_schools)))+
  theme_minimal()+
  theme(axis.title=element_text(size=9))

#DEPTH vs. AGGREGATION INDEX
ggplot()+ geom_point(data=Grafton_raw, aes(x=Range_mean_round, y=Aggregation_index), color="lightgrey", size=0.5)+
  #set up the asthetics, the line must be broken because this is technically discrete data
  geom_line(data=z, aes(x=Range_mean_round,y = Mean_Aggregation_index),linewidth=0.5, linetype=2)+
  geom_point(data=z, aes(x=Range_mean_round,y = Mean_Aggregation_index))+
  #put the y axis labes on the opposite side so when its flipped it will appear at top
  scale_y_continuous(position="right")+
  labs(y="Aggregation index", x="Vertical distance from the transducer (m)")+
  coord_flip(ylim = c(min(Grafton_raw$Aggregation_index),22))+
  theme_minimal()+
  theme(axis.title=element_text(size=9))


#Schools height
Grafton_raw %>% ggplot(aes(x=Height_mean)) + geom_point(aes(y=Range_mean), color = "lightgray")+
  geom_line()
theme_minimal()+labs(y="Vertical distance from the transducer (m)", x="Acoustic school height (m)")

#characterization of schools detected -> Sv_mean, Height_mean, Length of time, Range_mean, Aggregation_index, PRC_ABC

# 5. Values

n_uniqueSchools = n_distinct(Grafton_raw$Region_ID)
n_uniqueDates = n_distinct(Grafton_raw$Date)
mean_NumberSchools = n_uniqueSchools/n_uniqueDates
sd_NumberSchools = Grafton_raw %>% group_by(Date) %>% summarize(NumberSchools=n_distinct(Region_ID)) %>% pull(NumberSchools) %>%  sd()
mean_biomass = Grafton_intervals %>% group_by(Date) %>% summarize(meanBiomass = mean(Total_biomass)) %>% pull(meanBiomass) %>% mean()
sd_Biomass = Grafton_intervals %>% group_by(Date) %>% summarize(meanBiomass = mean(Total_biomass)) %>% pull(meanBiomass) %>%  sd()
mean_SchoolHeight = mean(Grafton_raw$Height_mean)

daily_estimates <- Grafton_intervals %>% group_by(Date) %>% summarize(total_PRC= sum(PRC_ABC), totalBiomass = sum(Total_biomass), meanBiomass = mean(Total_biomass))
nSchoolsdaily <- Grafton_raw %>% group_by(Date) %>% summarize(NumberSchools=n_distinct(Region_ID))

daily_estimates <- merge(daily_estimates, nSchoolsdaily, by = "Date")


