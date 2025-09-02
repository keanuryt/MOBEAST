### MOBEAST Metadata clean-up script
### By: Keanu Rochette
### Created: 2025-09-01
### Last Modified: 2025-09-01
###############

## Libraries #####
library(tidyverse)
library(lubridate)
library(here)
library(janitor)

## Data ######
meta <- read_csv(here("data", "MOBEAST_metadata.csv"))

## Creating metadata file
meta <-meta %>% clean_names()
table_id <- read_csv(here("data", "Laurel", "TableID.csv"))

table_id <- clean_names(table_id)

table_id <- table_id %>% rename(tank_number=tankid) %>% select(-treatment) %>% 
  mutate(tank_number = paste0("T", tank_number)) %>% 
  mutate(inflow_table = paste("Table", inflow_table))

inflow_id <- tibble(tank_number = c("F1", "F2"),
                    inflow_table = c("Table 1", "Table 2"))
table_id <- rbind(table_id, inflow_id)

meta <- left_join(meta, table_id)

meta %>% relocate(inflow_table, .before = tank_number)

treat_code <- tibble(
  treatment = c("A", "K", "C", "R", "Inflow", "Endmember", "Rain"),
  full_name = c("Algae", "Coral", "Control", "CCA", "Inflow", "Endmember", "Rain")
)
treat_code

meta<- left_join(meta, treat_code)

meta <- meta %>% relocate(full_name, .before = treatment) %>% 
  select(-treatment) %>% 
  rename(treatment = full_name) %>% 
  filter(id_number != is.na(id_number),
         id_number == ifelse(str_detect(meta$id_number,"[ABC]"), id_number, "NA")) 


############


#Reformating the date, time and date-time in the metadata file 

meta<- meta %>% mutate(time = str_pad(time, width = 4, side = "left", pad = "0"),
                       time = str_replace(time, "(\\d{2})(\\d{2})", "\\1:\\2")) %>% 
  # "(\\d{2})(\\d{2})" separates the string in 2 groups of 2 characters
  # "\\1:\\2", group1 + ":" + group2
  mutate(time = paste0(time, ":00"),
         date_time = paste(date, time),
         date = mdy(date), 
         date_time = mdy_hms(date_time),
         time = factor(time))


### Preparing the metadata to annotate the heatmap

# renaming some of treatment so that environmental data have a label: endmember or rain
# Same for inflow table, environmental samples now have a label
## Convert tibble to df for later
meta <- meta %>% mutate(treatment = as.character(treatment)) %>% 
  mutate(treatment = ifelse(id_number == "MOBEAST115", "Rain", 
                            ifelse(is.na(treatment), "Endmember", treatment))) %>% 
  mutate(inflow_table = ifelse(is.na(inflow_table), "Environmental data", inflow_table)) %>% 
  mutate(date_string = as.character(date),
         time_string = as.character(time),
         date_time_string = paste(date_string, time_string)) %>% 
  as.data.frame()


#write_csv(meta, here("data","MOBEAST_metadata_V2.csv"))



