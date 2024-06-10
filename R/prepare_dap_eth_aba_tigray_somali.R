###############################################################################
# read packages
rm(list = ls())
library(tidyverse)

df_tool_survey <- readxl::read_excel(path = "inputs/REACH_ETH2306b_ABA_tool_FINAL.xlsx", sheet = "survey")

vars_to_remove <- c("consent",
                    "team_leader_name",
                    "enumerator_id",
                    "enum_comment",
                    "person_name",
                    "health_ind_name",
                    "hh_kebele",
                    "hh_telephone",
                    "hh_phone_num",
                    "enum_gender",
                    "date_dc")

df_dap_file_data_composites <- df_tool_survey |> 
  filter(str_detect(string = type, pattern = "integer|date|select_one|select_multiple"),
         !name %in% vars_to_remove) |> 
  select(variable = name) |>
  bind_rows(tibble::tribble(~variable,
                            "i.hh_age",
                            "i.hh_size",
                            "i.hoh_no")) |> 
  mutate(split = "all",
         subset_1 = "region",
         subset_2 = "hh_situation",
         subset_4 = "i.hh_age",
         subset_5 = "hoh_gender") |> 
  pivot_longer(cols = starts_with("subset"), names_to = "subset_no", values_to = "subset_1") |> 
  filter(!is.na(subset_1), !subset_1 %in% c("NA")) |> 
  select(-subset_no)

# output r_dap
write_csv(x = df_dap_file_data_composites, file = paste0("outputs/", butteR::date_file_prefix(), "_r_dap_eth_aba_tigray_somali.csv"), na = "NA") 
write_csv(x = df_dap_file_data_composites, file = "inputs/r_dap_eth_aba_tigray_somali.csv", na = "NA")

###############################################################################
