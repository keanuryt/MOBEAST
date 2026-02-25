# Title: Merging all data
# Date: November 18, 2025
# Edited: 23 February 2026
# Author: Keanu Rochette-Yu Tsuen 
#################


# Load libraries 
library(tidyverse)
library(here)
library(janitor)
library(hms)


# Load the data
meta <- read_csv(here("data", "MOBEAST_metadata_V2.csv"))
carbonate <- read_csv(here("data", "carbonate.csv"))
DOC <- read_csv(here("data", "DOC", "DOC_clean.csv"))
fDOM <- read_csv(here("data", "fDOM", "fDOM_sorted.csv"))
FCM <- read_csv(here("data", "MOBEAST_FCM.csv"))
nutrients <- read_delim(here("data", "Nutrients", "MOBEAST_nutrients.csv"))
meta2 <- read_csv(here("data", "MOBEAST_metadata.csv"))
res_time <- read_csv(here("data", "Laurel", "Full_Carb_Chem_Data.csv"),locale=locale(encoding="latin1"))
chla <- read_csv(here("data", "chlorophyll_a.csv"))


## Data Clean up 

### DOC 
DOC<- DOC %>%  select(id_number, date, time, npoc_u_m, tn_u_m)
DOC_list <- colnames(DOC[,c(4,5)])

### fDOM 
fDOM<- fDOM %>% select(id_number, date_time, coble_a:lignin) %>% 
  # error in data treatment script, M:C needs to be recalculated manually 
  mutate(m_to_c = coble_m/coble_c) 

fDOM_list <- colnames(fDOM[,c(3:13)])

### Nutrients
# nutrients are in umol/L 
nutrients <- nutrients %>% clean_names() %>% 
  rename(N_N = no3_no2_umol_l,
         NO2 = no2_umol_l,
         PO4 = po4_umol_l,
         SiO2 = si_o2_umol_l,
         NH3 = nh3_umol_l) %>% 
  mutate(across(N_N:NH3, ~ str_remove_all(.x, "[<>]"))) %>% 
  mutate(across(N_N:NH3, ~ as.numeric(.x))) %>% 
  mutate(NO3 = N_N - NO2) %>% select(-run_number)

nutrient_list <- colnames(nutrients[,c(2:7)])

### FCM 

# can't use the good metadata V2 bcs we sampled FCM more frequently than the other variable
# we are missing some id numbers for the additional samples...

FCM <- FCM %>% clean_names()
meta2 <-meta2 %>% clean_names()

## FCM data doesn't have a id number so we'll join the data with other information 
### Treatment code changed below
treat_code <- tibble(
  treatment = c("A", "K", "C", "R", "Inflow", "Endmember", "Rain"),
  full_name = c("Algae", "Coral", "Control", "CCA", "Inflow", "Endmember", "Rain"))

### Associate the data with an inflow table 
table_id <- read_csv(here("data", "Laurel", "TableID.csv"))
table_id <- clean_names(table_id)

table_id <- table_id %>% rename(tank_number=tank_num) %>% select(-treatment) %>% 
  mutate(tank_number = paste0("T", tank_number)) %>% 
  mutate(inflow_table = paste("Table", inflow_table))

## remove samples that are problematic from the dataset 
### contamination and mislabeling
FCM_pre <- FCM %>% filter(str_detect(sample, pattern = "MOBEAST")) %>% 
  mutate(sample = str_remove(sample, pattern = "X_")) %>% 
  filter(sample!= "2_MOBEAST_T4_A_2024-06-02_18:00", 
         sample!= "3_MOBEAST_T4_A_2024-06-02_18:00") %>% 
  separate_wider_delim(sample, names= c("project", "tank", "treatment", "date","time"), 
                       delim = "_") %>% 
  ## Misaligned data made it difficult to analyze and compare
  ### 18:00 is duplicated because one batch is actually 21:00
  ### there is also multiple samples for 1 tank (T4?)
  ### 3PM and 6PM data were close enough so we used the 3PM as our 6PM data 
  ### to have a complete data set. (Feb 23 2026)
  ## Removed 34 samples: 18 samples (batch 1) + 16 samples (batch 2, missing T4 and T6 values)
  filter(time != "18:00") %>% 
  mutate(time = str_replace_all(time,"15:00", "18:00")) 

## Merging preliminatry meta data to add with the bigger data file later
FCM_pre <- left_join(FCM_pre, treat_code, by= "treatment")

FCM_pre <- FCM_pre %>% relocate(full_name,.before = treatment) %>%
  select(-treatment) %>% 
  rename(treatment = full_name) %>% 
  rename(tank_number = tank)

FCM_pre <- FCM_pre %>% left_join(table_id, by = "tank_number") %>% 
  mutate(inflow_table = ifelse(tank_number == "F1", "Table 1",
                               ifelse(tank_number == "F2", "Table 2", inflow_table))) %>% 
  mutate(inflow_table = ifelse(is.na(inflow_table), "Endmember", inflow_table))

FCM_pre <- FCM_pre %>% 
  mutate(date_time = ymd_hms(paste0(date," ", time,":00"))) %>% 
  select(tank_number, treatment, inflow_table, date_time, date, time, 
         het_bact = het_bact_events_u_l,
         pico_euk = pico_euk_events_u_l,
         syn = syn_events_u_l,
         pro = pro_events_u_l,
         tot_bact = bacteria_events_u_l) %>% 
  group_by(date_time, treatment, tank_number) %>% 
  mutate(cyano = sum(syn, pro, het_bact) - het_bact) %>%  
  ungroup() %>% 
  mutate(date =  ymd(date),
         time = paste0(time,":00"),
         time = as.character(time)) 

fcm_list <- colnames(FCM_pre[,c(7:12)])

## carbonate 

carbonate <- carbonate %>% select(-ph_inflow, -ta_inflow) 
carbonate_list <- colnames(carbonate[,c(7:10)])

## Chlorophyll 
chla <- chla %>% clean_names() %>% 
  rename(id_number = sample_id)

## Residence time and flowrate 

res_time <- res_time %>% clean_names() %>% 
  mutate(date = ymd(date),
         date_string= as.character(date),
         time_string =  paste0(time), 
         tank_num = paste0("T", tank_num), 
         treatment = str_remove(treatment, "_Dom"), 
         treatment= ifelse(treatment =="Rubble", "CCA", treatment)) %>% 
  rename(tank_number = tank_num) %>% 
  select(date_string, time_string, tank_number, treatment, do_mg_l, ta, p_h,
         dic_mmol_kg, residence_time, flowrate) 


res_time_list <- colnames(res_time[,c(2:10)])

## Merging the data 
### Merging FCM data with meta data
merged <- meta %>% full_join(FCM_pre, by = c("tank_number", "treatment", "date_time", 
                                "date")) %>% 
  mutate(inflow_table.y = ifelse(is.na(inflow_table.y), inflow_table.x, inflow_table.y)) %>% 
  select(-inflow_table.x) %>% 
  rename(inflow_table = inflow_table.y) %>% 
  mutate(time.y = as.character(time.y)) %>% 
  mutate(time.y = ifelse(is.na(time.y), as.character(time.x), time.y)) %>% 
  select(-time.x) %>% 
  rename(time = time.y) %>% select(-long_name) 


### Merging chla data with previous data 
merged <- merged %>% left_join(chla, by = "id_number") 

### Merging previous data with DOC data 
merged <- merged %>% left_join(DOC, by = "id_number") %>% 
  select(-time.y, -date.y) %>% 
  rename(time = time.x, date = date.x) %>%  
  relocate(time, .after = "date")%>% 
  relocate(inflow_table, .after = "treatment") 

### Merging previous data with fDOM data 
merged <- merged %>% left_join(fDOM) 

### Merging previous data with nutrient data 
merged<- merged %>% left_join(nutrients) 

### Merging previous data with carbonate data 
merged <- merged %>% mutate(time = as_hms(time)) %>% 
  full_join(carbonate) 

### merge residence time with meta data
#res_time <- res_time %>% clean_names() %>% 
#  rename(tank_number = tank_num) %>% 
#  mutate(tank_number = paste0("T",tank_number)) 

merged<- merged %>% mutate(date_string = as.character(date_string),
                    time_string = as.character(time_string)) %>% 
    left_join(res_time, by = c("date_string", "time_string", "treatment", "tank_number")) %>% 
  mutate(ta.y= ifelse(is.na(ta.y), ta.x, ta.y)) %>% 
  select(-ta.x) %>%  rename(ta = ta.y) 
         
## Data clean up
merged_data <- merged %>% 
  mutate(time_string = as.character(time),
         date_string = as.character(date),
         date_time_string = as.character(date_time)) 

#write_csv(merged_data, here("data", "MOBEAST_full_merged_data.csv"))

## Creating long format data 
merged_long <- merged_data %>% 
  pivot_longer(cols = het_bact:dic_mmol_kg,
               names_to = "variable",
              values_to = "value") %>% 
  mutate(variable_cat =
           ifelse(variable %in% DOC_list, "DOC",
                  ifelse(variable %in% fDOM_list, "fDOM", 
                         ifelse(variable %in% carbonate_list, "carbonate", 
                                ifelse(variable %in% fcm_list, "FCM", 
                                       ifelse(variable %in% nutrient_list, "nutrients", 
                                              ifelse(variable %in% res_time_list, "residence time",
                                                     "chla")))))))

#write_csv(merged_long, here("data", "MOBEAST_full_merged_data_long.csv"))




