## MOBEAST: Nutrient Data Clean Up 

## Created : 2025-08-30
## Created by: Keanu Rochette-Yu Tsuen 
##############

## Libraries ########
library(tidyverse) 
library(here) 
library(janitor)

## Load Data ########
nutrient <- read_delim(here("data", "Nutrients", "MOBEAST_nutrients.csv"))
meta <- read_csv(here("data", "MOBEAST_metadata_V2.csv"))
treat_code <- tibble(
  symbole = c("A", "K", "C", "R", "Inflow", "Endmember", "Rain"),
  treatment = c("Algae", "Coral", "Control", "CCA", "Inflow", "Endmember", "Rain")
)

## Data clean-up
nutrient <- nutrient %>% tibble() %>% clean_names()

meta <- meta %>%  left_join(treat_code) %>% relocate(symbole,.after = treatment)

nutrient_full <- full_join(meta,nutrient)

nutrient_full <- nutrient_full %>% 
  mutate(time = str_replace(time, "(\\d{1,2})(\\d{2})$", "\\1:\\2")) %>% 
  mutate(time = paste0(time, ":00")) %>%  
  mutate(date = mdy(date), 
         time = hms::as.hms(time)) 


#write_csv(nutrient_full,here("data", "Nutrients", "nutrients_clean.csv"))
colnames(nutrient_full)
nutrient_full<- nutrient_full %>% rename(N_N = no3_no2_umol_l,
                         NO2 = no2_umol_l,
                         PO4 = po4_umol_l,
                         Si02 = si_o2_umol_l,
                         NH3 = nh3_umol_l) %>% 
  mutate(N_N= if_else(str_detect(N_N, "^<"), 0, as.numeric(N_N)),
         NO2= if_else(str_detect(NO2, "^<"), 0, as.numeric(NO2)),
         PO4= if_else(str_detect(PO4, "^<"), 0, as.numeric(PO4)),
         Si02= if_else(str_detect(Si02, "^<"), 0, as.numeric(Si02)),
         NH3= if_else(str_detect(NH3, "^<"), 0, as.numeric(NH3))
         ) %>% 
  pivot_longer(cols = N_N:NH3, 
               names_to = "nutrients",
               values_to = "conc") %>% view()
  

ggplot(nutrient_full)+
  geom_boxplot(aes(treatment, conc))+
  facet_wrap(~nutrients+date)










