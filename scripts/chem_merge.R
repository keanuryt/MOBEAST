# Title: Merging Chem data
# Date: July 25, 2025
# Author: Keanu Rochette-Yu Tsuen 
#################

# Load libraries 
library(tidyverse)


# Load the data
carbonate <- read_csv(here("data", "carbonate.csv"))
DOC <- read_csv(here("data", "DOC", "DOC_norm.csv"))
fDOM <-  read_csv(here("data", "fDOM", "fDOM_norm.csv"))

# Clean Data 

fDOM_wide <- fDOM %>% select(-fdom_alt_name, -fdom_cat, -fdom_conc, -inflow) %>% 
  pivot_wider(names_from = fdom, 
              values_from = fdom_conc_norm) 

# Merging datasets
chem <- left_join(DOC, fDOM_wide)

chem <- chem %>%  select(-sample, -npoc_mg_l,-tn_mg_l,-tn_u_m, -inflow) 

chem <- left_join(chem, carbonate)

chem %>% view()

write_csv(chem, here("data", "chem.csv"))









