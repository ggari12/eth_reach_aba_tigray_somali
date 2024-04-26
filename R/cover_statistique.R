###############################################################################
# checks for data collection
# read packages
rm(list = ls())

library(tidyverse)
library(lubridate)
library(glue)
library(supporteR)
data_path <- "inputs/ETH2306b_ABA_Tigray_Somali_data_2024-04-24-07-49-01.xlsx"


df_tool_data <- readxl::read_excel(data_path) |>  
  select(region,zone, woreda, hh_situation)|>
  filter(!is.na(region))|>
  dplyr::group_by(region,zone,woreda, hh_situation)|>
  dplyr::summarise(number= n())

view(df_tool_data)
