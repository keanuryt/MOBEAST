## Title: Chlorophyll data clean up 
## Created: Jan 20, 2025 
## By: Keanu Rochette
########


## Load Libraries 
library(tidyverse)
library(janitor)
library(here)

## Load Data 
chla_raw <- clean_names(read_delim(here("data", "raw_data", "chlorophyll_raw.tsv")))

## Context 
### Chlorophyll was collected by filtering seawater for the MOBEAST project on cellulose filters
### We need to know the volume that was filtered in order to calculate the chlorophyll concentration in the water. 
### We measured the water filtered using 50mL falcon tubes for a target volume of 125mL, i.e. 2.5falcon tubes, measured in 3 installments.
### However, sometimes, the tubes would spill or volume was not recorded, so volumes were back calculated the best I could be information available.

## Data clean up 

### Caluculating filtered volume
chla_raw <- chla_raw %>% 
  # if all 3 volumes measurements were recorded, we summed them up
  mutate(total_vol = select(., c(vol_1,vol_2,vol_3_half)) %>% apply(1, sum, na.rm=TRUE)) %>% 
  # if volume 1 was missing, we double volume 2 and add vol 3
  mutate(total_vol = ifelse(is.na(vol_1), vol_2*2 + vol_3_half, total_vol)) %>% 
  # if volume 2 was missing, we double volume 1 and add vol 3
  mutate(total_vol = ifelse(is.na(vol_2), vol_1*2 + vol_3_half, total_vol)) %>% 
  # if vol 1 and vol 2 are missing, then we multiple vol 3 by 5. 
  ## that's because vol 3 is only half of vol1 and vol2. 
  mutate(total_vol = ifelse(is.na(vol_1) & is.na(vol_2), vol_3_half*5, total_vol)) %>% 
  # if vol 1 and vol 3 are missing, then we doubled vol 2 and add half of vol 2. 
  mutate(total_vol = ifelse(is.na(vol_1) & is.na(vol_3_half), vol_2*2 +vol_2/2, total_vol)) %>% 
  # some samples were filtered by hand with a syringe (vol =125ml)
  ## other samples had no volume filtered recorded and were assumed to be 125mL as preliminary results
  mutate(total_vol = ifelse(is.na(total_vol), 125, total_vol)) 

## Back calculating chla concentrations
chla <- chla_raw %>% 
  # the concentration in the extractant (ug/L) is equal to the fluorometer reading / dilution correlation factor
  mutate(conc_extractant = fluorometer/dilution_factor,
  # the mass of chla (ug) on the filter is equal to the concentration of chla in the extractant x the extration volume /1000
         chla_on_filter = conc_extractant*extraction_vol/1000,
  #the final chla concentration is the mass of chla on the filter / volume filtered x1000
         chla_conc = chla_on_filter/total_vol*1000) 


## removing columns that are not useful for analysis
chla <- chla %>% select(sample_id, chla_conc)
#write_csv(chla, here("data", "chlorophyll_a.csv"))



