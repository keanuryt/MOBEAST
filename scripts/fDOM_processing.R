#####
# This script processed fDOM data for MOBEAST and ROTUI projects
# Author: Keanu Rochette-Yu Tsuen
# Created: Jan 16, 2025
# Last Edited: Jan 25, 2025
# Functions written by Sean Swift, Nelson Lab
#####

# Notes:
# Need to rerun run2 (edit: run 2 was re-run on Jan 17, 2025)
# There is no run1, contaminated samples with rust due to poor storage
# MOBEAST fDOM Measured using frozen water sample aimed for nutrient measurements
#####

## Libraries
library(here)
library(tidyverse)

## Set the working directory
## if directory not set, code won't work due to written function 
setwd("/Users/keanurochette/Desktop/Git Hub Repository/MOBEAST/data/raw_data/fDOM")

## file path
path <- here("data", "raw_data", "fDOM")

## Load functions
source(file = here("scripts","aqualog", "process_aqualog_functions.R"))


## Data processing 

### MOBEAST files
run3 = process_aqualog(data_directory = paste0(path,"/run3"),
                       run_name = "run3",
                       sample_key_file = "run3_log.tsv")

run4 = process_aqualog(data_directory = paste0(path,"/run4"),
                       run_name = "run4",
                       sample_key_file = "run4_log.tsv")

run7 = process_aqualog(data_directory = paste0(path,"/run7"),
                       run_name = "run7",
                       sample_key_file = "run7_log.tsv")

### ROTUI files

run5 = process_aqualog(data_directory = paste0(path,"/run5"),
                       run_name = "run5",
                       sample_key_file = "run5_log.tsv")

run6 = process_aqualog(data_directory = paste0(path,"/run6"),
                       run_name = "run6",
                       sample_key_file = "run6_log.tsv")

## Sample ID list MOBEAST
sample_sheet= read.csv(paste0(path,"/UniqueID_MOBEAST.csv"))


## compile all the files in one folders
all_runs = compile_runs(run_dirs = c("run3", "run4", "run7"), paste0(path,"/fDOM_clean_MOBEAST"))


## Sample ID list ROTUI
sample_sheet= read.csv(paste0(path,"/UniqueID_ROTUI.csv"))

## compile all the files in one folders
all_runs = compile_runs(run_dirs = c("run5", "run6"), out_dir = paste0(path,"/fDOM_clean_ROTUI"))






