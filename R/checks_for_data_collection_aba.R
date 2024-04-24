###############################################################################
# checks for data collection
# read packages
rm(list = ls())

library(tidyverse)
library(lubridate)
library(glue)
library(supporteR)
remotes::install_github("ellieallien/cleaninginspectoR")

# read data and tool ----------------------------------------------------------
# data
data_path <- "inputs/ETH2306b_ABA_Tigray_Somali_data_2024-04-23-06-38-03.xlsx"


df_tool_data <- readxl::read_excel(data_path) |>  
  mutate(start = as_datetime(start),
         end = as_datetime(end),
         enumerator_id = ifelse(is.na(enumerator_id), enum_id, enumerator_id)) |> 
  checks_add_extra_cols(input_enumerator_id_col = "enumerator_id",
                        input_location_col = "region")|>
  select(-c("health_post_location2_other",
            "health_centre_non_functionality_reasons_other",
            "health_centre_location2_other",
            "hospital_non_functionality_reasons_other",
            "hopsital_location2_other",
            "pharmacy_non_functionality_reasons_other",
            "pharmacy_location2_other",
            "dispensary_non_functionality_reasons_other",
            "dispensary_location2_other",
            "mobile_clinics_non_functionality_reasons_other",
            "mobile_clinics_location2_other",
            "hh_healthcare_non_functionality_reasons_other",
            "hh_healthcare_location2_other"))


# loops -----------------------------------------------------------------------
# loop_hh_roster
loop_hh_roster <- readxl::read_excel(path = data_path, sheet = "hh_roster")

df_raw_data_loop_hh_roster <- df_tool_data |> 
  select(-`_index`) |> 
  inner_join(loop_hh_roster, by = c("_uuid" = "_submission__uuid"))


# loop_hh_education
loop_hh_education <- readxl::read_excel(path = data_path, sheet = "education_loop")

df_raw_data_loop_hh_education <- df_tool_data |> 
  select(-`_index`) |> 
  inner_join(loop_hh_education, by = c("_uuid" = "_submission__uuid"))


# loop2_hh_health
loop2_hh_education <- readxl::read_excel(path = data_path, sheet = "education_loop2")

df_raw_data_loop2_hh_education <- df_tool_data |> 
  select(-`_index`) |> 
  inner_join(loop2_hh_education, by = c("_uuid" = "_submission__uuid"))



# tool
loc_tool <- "inputs/REACH_ETH2306b_ABA_tool_FINAL.xlsx"

df_survey <- readxl::read_excel(loc_tool, sheet = "survey")
df_choices <- readxl::read_excel(loc_tool, sheet = "choices")

# checks ----------------------------------------------------------------------

checks_output <- list()

# testing data ----------------------------------------------------------------

df_testing_data <- df_tool_data |> 
  filter(i.check.start_date < as_date("2024-04-11")) |> 
  mutate(i.check.type = "remove_survey",
         i.check.name = "",
         i.check.current_value = "",
         i.check.value = "",
         i.check.issue_id = "logic_c_testing_data",
         i.check.issue = "testing_data",
         i.check.other_text = "",
         i.check.checked_by = "C",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "1",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  dplyr::select(starts_with("i.check.")) |> 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))|>
  mutate(region = as.character(region))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_testing_data")


df_testing_data <- df_tool_data |> 
  filter(i.check.start_date < as_date("2024-04-13")) |> 
  mutate(i.check.type = "remove_survey",
         i.check.name = "",
         i.check.current_value = "",
         i.check.value = "",
         i.check.issue_id = "logic_c_testing_data",
         i.check.issue = "testing_data",
         i.check.other_text = "",
         i.check.checked_by = "C",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "1",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  dplyr::select(starts_with("i.check.")) |> 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))|>
  mutate(region = as.character(region))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_testing_data")

# Time checks -----------------------------------------------------------------

# Time interval for the survey
min_time_of_survey <- 20
max_time_of_survey <- 120

df_c_survey_time <-  supporteR::check_survey_time(input_tool_data = df_tool_data, 
                                                  input_enumerator_id_col = "enumerator_id",
                                                  input_location_col = "region",
                                                  input_min_time = min_time_of_survey, 
                                                  input_max_time = max_time_of_survey)|>
  
  mutate(region = as.character(region))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_c_survey_time")

# check duplicate uuids -------------------------------------------------------

df_c_duplicate_uuid <-  supporteR::checks_duplicate_uuids(input_tool_data = df_tool_data)|>
  
  mutate(region = as.character(region))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_c_duplicate_uuid")

# outliers --------------------------------------------------------------------

df_c_outliers <- supporteR::check_outliers_cleaninginspector(input_tool_data = df_tool_data,
                                                             input_enumerator_id_col = "enumerator_id",
                                                             input_location_col = "region")|>
mutate(region = as.character(region))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_c_outliers")

# other_specify ---------------------------------------------------------------

df_others_data <- supporteR::extract_other_specify_data(input_tool_data = df_tool_data, 
                                                        input_enumerator_id_col = "enumerator_id",
                                                        input_location_col = "region",
                                                        input_survey = df_survey,  
                                                        input_choices = df_choices)|>
  
  mutate(region = as.character(region))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_others_data")

# repeat_other_specify education_loop --------------------------------------------------------
  
df_repeat_others_data_educ <- supporteR::extract_other_specify_data_repeats(input_repeat_data = df_raw_data_loop_hh_education|> mutate(`_index.y` = `_index`), 
                                                                       input_enumerator_id_col = "enumerator_id",
                                                                       input_location_col = "region",
                                                                       input_survey = df_survey,
                                                                       input_choices = df_choices,
                                                                       input_sheet_name = "education_loop",
                                                                       input_repeat_cols = c("hh_education_level_attend", "education_facility_owner","education_functionality_status_no"))|>
  mutate(region = as.character(index))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_repeat_others_data_educ")

# repeat_other_specify education_loop2 --------------------------------------------------------

df_repeat_others_data_educ2 <- supporteR::extract_other_specify_data_repeats(input_repeat_data = df_raw_data_loop2_hh_education |> mutate(`_index.y` = `_index`), 
                                                                       input_enumerator_id_col = "enumerator_id",
                                                                       input_location_col = "region",
                                                                       input_survey = df_survey,
                                                                       input_choices = df_choices,
                                                                       input_sheet_name = "education_loop2",
                                                                       input_repeat_cols = c("not_attending_reasons","dropout_reason"))|>
  mutate(region = as.character(index))


add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_repeat_others_data_educ2")

# spatial checks --------------------------------------------------------------

# logical checks --------------------------------------------------------------

# Household size seems to be unusually low (below 2) or high (above 8); survey needs to be checked
df_logic_c_hh_size_seems_unusal <- df_tool_data |> 
  filter(hh_size <= 2 | hh_size > 8) |> 
  mutate(i.check.type = "remove_survey",
         i.check.name = "hh_size",
         i.check.current_value = as.character(hh_size),
         i.check.value = "",
         i.check.issue_id = "hh_size_seems_unusal",
         i.check.issue = "household size seems unusal",
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "care to be taken in deciding how to use this data", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  dplyr::select(starts_with("i.check.")) |> 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))|>
  mutate(region = as.character(region))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hh_size_seems_unusal")

# The age of the hoh seems too high (80 years old or higher), please check the age of the hoh again.
df_logic_c_hoh_age_seems_too_high <- df_tool_data |> 
  filter(hoh %in% c("no"), hoh_no >= 80) |>
  mutate(i.check.type = "change_response",
         i.check.name = "hoh",
         i.check.current_value = as.character(hoh_no),
         i.check.value = "",
         i.check.issue_id = "hoh_age_seems_too_high",
         i.check.issue = glue("hoh: {hoh} but hoh_no:{hoh_no}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename() |> 
  mutate(region = as.character(region))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hoh_age_seems_too_high")

#water.... The amount you spend on water throughout the month seems very low
df_logic_c_hoh_water_month_seems_too_low <- df_tool_data |> 
  filter(hh_water_payment_amount < 200) |>
  mutate(i.check.type = "change_response",
         i.check.name = "hh_water_payment_amount",
         i.check.current_value = as.character(hh_water_payment_amount),
         i.check.value = "",
         i.check.issue_id = "hoh_water_month_seems_too_low",
         i.check.issue = glue("hoh_water_month_seems_too_low"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename() |> 
  mutate(region = as.character(region))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hoh_water_month_seems_too_low")

#water.... The amount you spend on water throughout the month seems very high
df_logic_c_hoh_water_month_seems_too_high <- df_tool_data |> 
  filter(hh_water_payment_amount > 100000) |>
  mutate(i.check.type = "change_response",
         i.check.name = "hh_water_payment_amount",
         i.check.current_value = as.character(hh_water_payment_amount),
         i.check.value = "",
         i.check.issue_id = "hoh_water_month_seems_too_high",
         i.check.issue = glue("hoh_water_month_seems_too_high"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename() |> 
  mutate(region = as.character(region))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hoh_water_month_seems_too_high")

#healthcare.... The amount you spend to your healthcare seems very high
df_logic_c_hoh_healthcare_seems_too_high <- df_tool_data |> 
  filter(healthcare_payment_amount  > 2000000) |>
  mutate(i.check.type = "change_response",
         i.check.name = "hh_payment_modality",
         i.check.current_value = as.character(healthcare_payment_amount),
         i.check.value = "",
         i.check.issue_id = "hoh_healthcare_seems_too_high",
         i.check.issue = glue("hoh_healthcare_seems_too_high"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename() |> 
  mutate(region = as.character(region))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hoh_healthcare_seems_too_high")

#healthcare.... The amount you spend to your healthcare seems very low
df_logic_c_hoh_healthcare_seems_too_low <- df_tool_data |> 
  filter(healthcare_payment_amount < 1000) |>
  mutate(i.check.type = "change_response",
         i.check.name = "hh_payment_modality",
         i.check.current_value = as.character(healthcare_payment_amount),
         i.check.value = "",
         i.check.issue_id = "hoh_healthcare_seems_too_low",
         i.check.issue = glue("hoh_healthcare_seems_too_low"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename() |> 
  mutate(region = as.character(region))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hoh_healthcare_seems_too_low")

#Jerrycan.... number of jerrycan seems too high
df_logic_c_hoh_num_jerrycan_seems_too_high <- df_tool_data |> 
  filter(num_jerrycan_allowed > 10) |>
  mutate(i.check.type = "change_response",
         i.check.name = "num_jerrycan_allowed",
         i.check.current_value = as.character(num_jerrycan_allowed),
         i.check.value = "",
         i.check.issue_id = "hoh_num_jerrycan_seems_too_high",
         i.check.issue = glue("hoh_num_jerrycan_seems_too_high"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename() |> 
  mutate(region = as.character(region))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hoh_num_jerrycan_seems_too_high")

# log 999
cols_with_text_values <- df_survey |> filter(type %in% c("text"), name %in% colnames(df_tool_data)) |> pull(name)

df_999_data_other <- purrr::map_dfr(.x = cols_with_text_values, 
                                    .f = ~ {df_tool_data |> 
                                        dplyr::filter(str_detect(string = !!sym(.x), pattern = "^-[9]{2,4}$|^[9]{2,4}$")) |> 
                                        dplyr::mutate(i.check.type = "change_response",
                                                      i.check.name = .x,
                                                      i.check.current_value = as.character(!!sym(.x)),
                                                      i.check.value = "NA",
                                                      i.check.issue_id = "logic_c_handle_999_other",
                                                      i.check.issue = "remove 999 added during data collection",
                                                      i.check.other_text = "",
                                                      i.check.checked_by = "TKC",
                                                      i.check.checked_date = as_date(today()),
                                                      i.check.comment = "",
                                                      i.check.reviewed = "1",
                                                      i.check.adjust_log = "",
                                                      i.check.so_sm_choices = "") |>
                                        dplyr::select(starts_with("i.check."))}) |> 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = "")) |> 
  mutate(region = as.character(region))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_999_data_other")

# combined  checks ------------------------------------------------------------

df_combined_checks <- bind_rows(checks_output)

# output the log --------------------------------------------------------------

write_csv(x = df_combined_checks, file = paste0("outputs/", butteR::date_file_prefix(), 
                                                "_combined_checks_eth_aba_tigray_somali.csv"), na = "")

###############################################################################
