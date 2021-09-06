#Neutron Probe processing script
#Tracey May,Hiz Jamali, Chris Nunn #update 24/06/21
#combines and redesigns scripts to process Neutron Probe Data from raw to calibrated readings. 

library(tidyverse)
library(lubridate)
library(purrr)
library(readxl)

#set directory paths
NMM_data_location <- "//nexus.csiro.au/csiro/Agriculture/Operations Agriculture/Myall Vale/Groups/COTPhys_Current/2020-2021/Trials/PlantBasedSensing_OD212857/LimitedWater_A2/Raw/NMM"
#read in the raw data file

#neutron probe raw data
#1920_read_delim("//fsact.nexus.csiro.au/ANF-Share1/MyallVale/Groups/COTPhys/2019-2020/Trials/1920_Limited_Water_A2/raw_data/NMM/2021_06_02.txt", delim = " ", trim_ws = TRUE, col_names = FALSE) 
#nmm_raw <- read_delim("//nexus.csiro.au/csiro/Agriculture/Operations Agriculture/Myall Vale/Groups/COTPhys_Current/2020-2021/Trials/PlantBasedSensing_OD212857/LimitedWater_A2/Raw/NMM/2020_12_07.txt", delim = " ", trim_ws = TRUE, col_names = FALSE) 

nmm_recursive <- list.files(path = NMM_data_location,
                            recursive = T,
                            full.names = T,
                            pattern = "*.txt")


df <- data_frame(filename = nmm_recursive)%>% 
  mutate(file_contents = map(filename, ~ read_delim(file.path(.),
                                                   delim = " ",
                                                   trim_ws = TRUE,
                                                   col_names = FALSE,
                                                   col_types = "cddccddddddddd")))


#limited water plot description 
nmm_raw <- unnest(df, cols = c(file_contents))
#name columns
nmm_raw_label <- rename(nmm_raw, plot = X2, 
                 index = X1, 
                 date = X4, 
                 time = X5,
                 d_120cm = X6, 
                 d_100cm = X7, 
                 d_80cm = X8, 
                 d_60cm = X9, 
                 d_50cm = X10, 
                 d_40cm = X11,
                 d_30cm= X12,
                 d_20cm = X13) %>% 
  select(plot,index,date,time,d_120cm:d_20cm)

#apply date conversion to raw data.  
nmm_raw_df <- nmm_raw_label %>% 
  mutate(date = mdy(date)) %>%
  mutate(time = parse_time(time,format = "%H:%M"))%>% 
  mutate(across(c(d_120cm:d_20cm), as.numeric))


#Filter out drum reads to form field reads nmm data frame. 
nmm_df <- filter(nmm_raw_df, plot != 0) %>% 
  group_by(plot,date) %>%
  mutate(row = if_else(time > mean(time), "plant", "skip")) %>% 
  ungroup()

mutate(date = floor_date(date)) %>%
  gather()
group_by(date) %>% 
  summarise(ave = mean())
  
##plot 0 is the drum reads, or 100% saturation, filter all other reads for drum df
drum_df <- nmm_raw_df %>% filter(plot == 0) %>% 
  mutate(across(cols = -c(index:time, date), na_if(.,0))) %>% 
  group_by(drum2,date) %>% 
  summarise(ave = mean(counts))

drum2 <-gather(drum, key = "depth", value = counts, "r20":"r120")

drum2[drum2$counts == 0] <- NA
  
drum3 <- group_by(drum2,date) %>% 
  summarise(ave = mean(counts))
  
  


#gather the data
gather_nmm <- gather(nmm_df3, key = "depth", value = count, "r20":"r120")

#sort the data 
nmm <- gather_nmm %>% 
  group_by(date, plot) %>% 
  mutate(skip = ifelse(index == min(index), "skip", "plant"))



write_csv(join_meta, file.path("//fsact.nexus.csiro.au/ANF-Share1/MyallVale/Groups/COTPhys/2019-2020/Trials/1920_Limited_Water_A2/raw_data/NMM", paste0("nmm_", Sys.Date(), ".csv")))

                    




