###############################################################################
rm(list = ls())
library(tidyverse)
library(srvyr)
library(supporteR) 


source("R/composite_indicators.R")

# packages to install incase
# devtools::install_github("zackarno/butteR")
# devtools::install_github("twesigye10/supporteR")

# clean data
data_path <- "outputs/20240520_clean_data_eth_aba_somali_tigray.xlsx"

data_nms <- names(readxl::read_excel(path = data_path, n_max = 3000, sheet = "cleaned_main_data"))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_main_cleand_data <- readxl::read_excel(path = data_path, sheet = "cleaned_main_data", col_types = c_types, na = "NA")|> 
  # dplyr::select(-starts_with("i.")) |> 
  create_composite_indicators() |> 
  dplyr::mutate(strata = woreda,
                region = case_when(region =="ET05"~"Somali",
                                   region == "ET01"~	"Tigray"))|>
  select_if(~ any(!is.na(.)))

numeric_cols <- c("hh_age",
                  "hoh_no",
                  "pregnant_lac_women_yes",
                  "hh_water_payment_amount",
                  "healthcare_payment_amount",
                  "num_jerrycan_allowed")

df_main_cleand_data[, numeric_cols] <- lapply(df_main_cleand_data[, numeric_cols], as.numeric)



# add weights to data
# df_main_clean_data_with_weights <- df_main_clean_data |>
#   dplyr::group_by(hh_zone, pop_group)|>
#   left_join(weight_table, by = c("hh_zone", "pop_group"))|>
#   dplyr::rename(zone1="hh_zone")
# 
# writexl::write_xlsx(df_main_clean_data_with_weights, "inputs/clean_data_eth_lcsa_somali_weighted.xlsx")



loop_support_data <- df_main_cleand_data|>
  dplyr::select(uuid,region,hh_situation,hoh_gender,i.hh_age, strata)


#Load data clean loop education

educ_loop <- readxl::read_excel(path = data_path, sheet = "cleaned_educ_loop", na = "NA")
df_cleaned_educ_data <- loop_support_data |> 
  inner_join(educ_loop, by = c("uuid" = "_submission__uuid") ) 
  
#Load data clean loop2 education

educ_loop2<- readxl::read_excel(path = data_path, sheet = "cleaned_educ_loop2", na = "NA")
df_cleaned_educ2_data <- loop_support_data |> 
  inner_join(educ_loop2, by = c("uuid" = "_submission__uuid") ) 

#Load data clean roster loop
roster_loop  <- readxl::read_excel(path = data_path, sheet = "cleaned_roster_loop", na = "NA")


# tool
loc_tool <- "inputs/REACH_ETH2306b_ABA_tool_FINAL.xlsx"
df_survey <- readxl::read_excel(loc_tool, sheet = "survey")
df_choices <- readxl::read_excel(loc_tool, sheet = "choices")

df_tool_data_support <- df_survey |> 
  select(type, name, label = `label::English`) |> 
  filter(str_detect(string = type, pattern = "integer|date|select_one|select_multiple")) |> 
  separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop" )

# dap
dap <- read_csv("inputs/r_dap_eth_aba_tigray_somali.csv")
# main dataset ------------------------------------------------------------

# set up design object
ref_svy <- as_survey(.data = df_main_cleand_data, strata = strata)

# analysis

df_main_analysis <- analysis_after_survey_creation(input_svy_obj = ref_svy,
                                                   input_dap = dap) |> 
  dplyr::mutate(level = "Household")


# education ------------------------------------------------------------------

df_dap_education <- bind_rows(tibble::tribble(~variable,
                                           "hh_education_level_attend",
                                           "education_facility_location",
                                           "education_facility_owner",
                                           "education_functionality_status",
                                           "education_functionality_status_no")) |> 
  mutate(split = "all",
         subset_1 = "region",
         subset_2 = "hh_situation",
         subset_3 = "hoh_gender",
         subset_4 = "i.hh_age") |> 
  pivot_longer(cols = starts_with("subset"), names_to = "subset_no", values_to = "subset_1") |> 
  filter(!is.na(subset_1), !subset_1 %in% c("NA")) |> 
  select(-subset_no)

# set up design object
   ref_svy_education_loop <- as_survey(.data = df_cleaned_educ_data, strata = strata)

# # analysis
df_nalaysis_education_loop <- analysis_after_survey_creation(input_svy_obj = ref_svy_education_loop,
                                                         input_dap = df_dap_education)|>


 mutate(level = "Individual")



# education2 ------------------------------------------------------------------

df_dap_education2 <- bind_rows(tibble::tribble(~variable,
                                           "not_attending_reasons",
                                           "dropout_reason")) |> 
  mutate(split = "all",
         subset_1 = "region",
         subset_2 = "hh_situation",
         subset_3 = "hoh_gender",
         subset_4 = "i.hh_age") |> 
  pivot_longer(cols = starts_with("subset"), names_to = "subset_no", values_to = "subset_1") |> 
  filter(!is.na(subset_1), !subset_1 %in% c("NA")) |> 
  select(-subset_no)

# set up design object
ref_svy_education2_loop <- as_survey(.data = df_cleaned_educ2_data, strata = strata)

# # analysis
df_analysis_education2_loop <- analysis_after_survey_creation(input_svy_obj = ref_svy_education2_loop,
                                                          input_dap = df_dap_education2)|>
  
  
  mutate(level = "Individual")

# roster ------------------------------------------------------------------

# df_dap_roster <- bind_rows(tibble::tribble(~variable,
#                                            "i.individual_age_cat",
#                                            "i.individual_genre_cat",
#                                            "i.individual_age_school_cat",
#                                            "i.individual_genre_age_cat")) |> 
#   mutate(split = "all",
#          subset_2 = "ind_gender"
#   ) |> 
#   pivot_longer(cols = starts_with("subset"), names_to = "subset_no", values_to = "subset_1") |> 
#   filter(!is.na(subset_1), !subset_1 %in% c("NA")) |> 
#   select(-subset_no)



# df_main_pivot <- df_main_clean_data_with_weights |> 
#   pivot_longer(cols = num_males_0to6:num_females_66plusyrs, names_to = "i.num_gender_age", values_to = "i.hh_size_based_on_gender_age")
# 
# df_roster_extract <- df_main_pivot |> 
#   filter(i.hh_size_based_on_gender_age > 0) |> 
#   uncount(i.hh_size_based_on_gender_age) |> 
#   mutate(i.individual_gender = ifelse(str_detect(string = i.num_gender_age, pattern = "females"), "Female", "Male"),
#          i.individual_age_cat = case_when(str_detect(string = i.num_gender_age, pattern = "0to6|7to3yrs|4to6") ~ "cat_0_6",
#                                           str_detect(string = i.num_gender_age, pattern = "7to13|14to17") ~ "cat_7_17",
#                                           str_detect(string = i.num_gender_age, pattern = "18to49|50to65") ~ "cat_18_65",
#                                           str_detect(string = i.num_gender_age, pattern = "66plusyrs") ~ "cat_66+" ))

# set up design object
# ref_svy_roster <- as_survey(.data = df_roster_clean_data, strata = strata, weights = weights)
# # analysis
# df_analysis_roster <- analysis_after_survey_creation(input_svy_obj = ref_svy_roster,
#                                                      input_dap = df_dap_roster ) |> 
#   mutate(level = "Individual")
# 

# merge and format analysis ----------------------------------------------------------

combined_analysis <- bind_rows(df_main_analysis, df_nalaysis_education_loop, df_analysis_education2_loop)


integer_cols_i <- c("i.hh_age",
                    "i.hh_size"
                    )

# formatting the analysis, adding question labels
full_analysis_long <- combined_analysis |> 
  mutate(variable = ifelse(is.na(variable) | variable %in% c(""), variable_val, variable),
         int.variable = ifelse(str_detect(string = variable, pattern = "^i\\."), str_replace(string = variable, pattern = "^i\\.", replacement = ""), variable)) |> 
  left_join(df_tool_data_support, by = c("int.variable" = "name")) |> 
  relocate(label, .after = variable) |> 
  mutate(variable = ifelse(variable %in% integer_cols_i, str_replace(string = variable, pattern = "i.", replacement = "int."), variable),
         # select_type = ifelse(variable %in% integer_cols_int, "integer", select_type),
         label = ifelse(is.na(label), variable, label),
         # `mean/pct` = ifelse(select_type %in% c("integer") & !variable %in% integer_cols_i & !str_detect(string = variable, pattern = "^i\\."), `mean/pct`, `mean/pct`*100),
         `mean/pct` = round(`mean/pct`, digits = 2))|>
  select(`Question`= label,
         variable,
         `choices/options` = variable_val,
         `Results(mean/percentage)` = `mean/pct`,
         n_unweighted,
         population,
         subset_1_name,
         subset_1_val,
         select_type,
         level)

# output analysis

write_csv(full_analysis_long, paste0("outputs/", butteR::date_file_prefix(), "_full_analysis_aba_tigray_somali.csv"), na="")
write_csv(full_analysis_long, paste0("outputs/full_analysis_aba_tigray_somali.csv"), na="")
write_csv(df_main_analysis, paste0("outputs/", butteR::date_file_prefix(), "_combined_analysis_aba_tigray_somali.csv"), na="")
###############################################################################
