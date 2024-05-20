################################################################################
# read in log and data
rm(list = ls())
library(tidyverse)
library(lubridate)
library(glue)
library(supporteR)
library(openxlsx)

options("openxlsx.borderStyle" = "thin")
options("openxlsx.dateFormat" = "dd/mm/yyyy")
# Read data and checking log 
# 
# df_cleaning_log <- read_csv("inputs/combined_checks_eth_lcsa_somali_GG2.csv", col_types = cols(sheet = "c", index = "i")) |> 
#   filter(reviewed %in% c("1"))

df_cleaning_log  <- read_csv("outputs/20240507_combined_checks_eth_aba_somali_carlos.csv", col_types = cols(sheet = "c", index = "i"))|> 
  filter(reviewed %in% c("1"))

# raw data
loc_data <- "inputs/REACH_ETH2306b_ABA_data.xlsx"

cols_to_escape <- c("index", "start", "end", "today", "starttime",	"endtime", "_submission_time", "_submission__submission_time")

data_nms <- names(readxl::read_excel(path = loc_data, n_max = 3000))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_raw_data <- readxl::read_excel(path = loc_data, col_types = c_types) |> 
  mutate(across(.cols = -c(contains(cols_to_escape)), 
                .fns = ~ifelse(str_detect(string = ., 
                                          pattern = fixed(pattern = "N/A", ignore_case = TRUE)), "NA", .))) 

# import clean data
clean_data_path <- "outputs/20240517_clean_data_eth_aba_somali_tigray.xlsx"
clean_data_nms <- names(readxl::read_excel(path = clean_data_path, n_max = 3000, sheet = "cleaned_main_data"))
clean_c_types <- ifelse(str_detect(string = clean_data_nms, pattern = "_other$"), "text", "guess")
df_main_clean_data <- readxl::read_excel(path = clean_data_path, sheet = "cleaned_main_data", col_types = clean_c_types, na = "NA") 

# tool
loc_tool <- "inputs/REACH_ETH2306b_ABA_tool_FINAL.xlsx"

df_survey <- readxl::read_excel(loc_tool, sheet = "survey")
df_choices <- readxl::read_excel(loc_tool, sheet = "choices") |> 
  mutate(label = `label::English`)

# harmonize select multiple to change_response according to HQ changes --------

# we may need to handle the delete log pattern on one of the entries // (is.na(adjust_log))
# we group by uuid, name. Then we filter only the row_number 1 and 
# change the type to change_response.
df_select_multiple_log <- df_cleaning_log %>% 
    filter(type %in% c("add_option", "remove_option"), is.na(adjust_log)) %>% 
    group_by(uuid, name) %>% 
    filter(row_number() == 1) %>% 
    mutate(type = "change_response",
           int.log_id = paste0(uuid, "_", name))

# use clean data to extract updated values
df_sm_cols_oi <- df_main_clean_data %>% 
    filter(uuid %in% df_select_multiple_log$uuid) %>% 
    select(uuid, any_of(df_select_multiple_log$name)) %>% 
    pivot_longer(cols = -uuid) %>% 
    mutate(int.data_sm_id = paste0(uuid, "_", name)) %>% 
    filter(int.data_sm_id %in% df_select_multiple_log$int.log_id)

# In the value, put the new value in the clean data
df_select_multiple_log_update <- df_select_multiple_log %>% 
    filter(uuid %in% df_main_clean_data$uuid) %>% 
    mutate(value = recode(int.log_id, !!!setNames(df_sm_cols_oi$value, df_sm_cols_oi$int.data_sm_id)),
           value = ifelse(is.na(value), "NA", value)) %>% 
    select(-"int.log_id")

# update log with changed select multiple ---------------------------------

df_log_del_all <- df_cleaning_log |> 
    filter(type %in% c("remove_survey"))

df_log_del_confirmed <- df_cleaning_log |> 
    filter(!adjust_log %in% c("delete_log"), type %in% c("remove_survey"))

df_log_for_change_reponse <- df_cleaning_log %>% 
    filter(type %in% c("change_response"), !uuid %in% df_log_del_confirmed$uuid) %>% 
    mutate(value = ifelse(is.na(value) & !adjust_log %in% c("delete_log"), "NA", value))

df_cleaning_log_updated <- bind_rows(df_log_del_all, df_log_for_change_reponse, df_select_multiple_log_update) %>% 
    arrange(uuid, start_date)

# create variable summary -------------------------------------------------
# also need to add composite indicators
# need to determine new choices added and how many entries were affected
df_variable_summary <- df_survey |> 
  filter(str_detect(string = type, pattern = "integer|date|select_one|select_multiple")) |> 
  mutate(variable = name, action = "checked", description = "", observations_affected = "") |> 
  select(variable, action, description, observations_affected)

# extract data ------------------------------------------------------------
df_data_extract <- df_raw_data |> 
  select(uuid = `_uuid`, `enumerator ID` = enumerator_id)

# log ---------------------------------------------------------------------

df_formatted_log <- df_cleaning_log_updated |> 
  # mutate(int.adjust_log = ifelse(adjust_log %in% c("delete_log"), "no", "yes")) |> 
  mutate(int.adjust_log = ifelse(adjust_log %in% c("delete_log"), "no", "yes"),
         `enumerator ID` = enumerator_id, question.name = name, Issue = str_replace_all(string = issue, pattern = "\\[+.+\\]", replacement = ""), 
         `Type of Issue` = type, feedback = comment, 
         changed = int.adjust_log, old.value = current_value, new.value = value,
         new.value = ifelse(changed %in% c("no"), old.value, new.value)) |> 
  select(uuid, `enumerator ID`, question.name, Issue, `Type of Issue`, 
         feedback, changed, old.value, new.value)
  
# deletion log ------------------------------------------------------------

df_deletion_log <- df_cleaning_log_updated |> 
  filter(!adjust_log %in% c("delete_log"), type %in% c("remove_survey")) |> 
  group_by(uuid) |> 
  filter(row_number() == 1) |> 
  ungroup() |> 
  select(uuid, `enumerator ID` = enumerator_id, Issue = issue, `Type of Issue (Select from dropdown list)` = type, 
         feedback = comment)

# enumerator performance --------------------------------------------------
# Number of surveys collected by enumerators
df_surveys_by_enum <- df_raw_data |> 
  group_by(enumerator_id) |> 
  summarise(Number = n())
# Number of changes by enumerators
df_changes_by_enum <- df_cleaning_log_updated |> 
  filter(!adjust_log %in% c("delete_log")) |> 
  group_by(enumerator_id) |> 
  summarise(Number = n())
# Number of changes by enumerators filtered by issues
df_changes_by_enum_issue <- df_cleaning_log_updated |> 
  filter(!adjust_log %in% c("delete_log")) |> 
  group_by(enumerator_id, issue_id) |> 
  summarise(Number = n())
# Number of deletions by enumerators
df_deletion_by_enum <- df_cleaning_log_updated |> 
  filter(!adjust_log %in% c("delete_log"), type %in% c("remove_survey")) |> 
  group_by(uuid) |> 
  filter(row_number() == 1) |> 
  ungroup() |> 
  group_by(enumerator_id) |> 
  summarise(Number = n())
# Number of deletions due to time by enumerator
df_deletion_by_enum_time <- df_cleaning_log_updated |> 
  filter(!adjust_log %in% c("delete_log"), type %in% c("remove_survey")) |> 
  filter(issue_id %in% c("less_survey_time")) |> 
  group_by(uuid) |> 
  filter(row_number() == 1) |> 
  ungroup() |> 
  group_by(enumerator_id) |> 
  summarise(Number = n())


# export sheets -----------------------------------------------------------

# 
# openxlsx::write.xlsx(x = list(Summary = df_variable_summary,
#                              data_extract = df_data_extract,
#                              # Log_book = df_formatted_log,
#                              deletion_log = df_deletion_log,
#                              surveys_by_enum = df_surveys_by_enum,
#                              changes_by_enum = df_changes_by_enum,
#                              changes_by_issue = df_changes_by_enum_issue,
#                              del_by_enum = df_deletion_by_enum,
#                              del_by_enum_time = df_deletion_by_enum_time),
#                     file = paste0("outputs/", butteR::date_file_prefix(), "_eth_msha_data_cleaning_logbook_for_formatting.xlsx"))

## exporting log alone
# write_csv(df_formatted_log, file = paste0("outputs/", butteR::date_file_prefix(), "_log_book.csv", na = ""))


# format the logbook and export -------------------------------------------
df_variable_tracker <- tibble::tribble(
                                  ~Variable,   ~Action,                                                                                                                                                     ~Rationale,
                                    "i.hh_size","Added",                                                                                                                                      "computed with hh_size",
                                   "i.hoh_no", "Added",                                                                                                                                     "Computed based on hh_no",
                                   "i.hh_age", "Added",                                                                                                                                     "Computed based on hh_age",
                             
                                 "deviceid", "Removed",                                                                                                                "Blanked columns related to the survey and PII",
                                    "audit", "Removed",                                                                                                                "Blanked columns related to the survey and PII",
                                "audit_URL", "Removed",                                                                                                                "Blanked columns related to the survey and PII",
                            "instance_name", "Removed",                                                                                                                "Blanked columns related to the survey and PII",
                              "enum_gender", "Removed",                                                                                                                "Blanked columns related to the survey and PII",
                            "enum_phonenum", "Removed",                                                                                                                "Blanked columns related to the survey and PII",
                                "enum_name", "Removed",                                                                                                                "Blanked columns related to the survey and PII",
                                      "gps", "Removed",                                                                                                                "Blanked columns related to the survey and PII",
                            "_gps_latitude", "Removed",                                                                                                                "Blanked columns related to the survey and PII",
                           "_gps_longitude", "Removed",                                                                                                                "Blanked columns related to the survey and PII",
                            "_gps_altitude", "Removed",                                                                                                                "Blanked columns related to the survey and PII",
                           "_gps_precision", "Removed",                                                                                                                "Blanked columns related to the survey and PII",
                             "xxx_other",       "Romoved",                                                                                                                 "Answer other"
                            )


# create workbook ---------------------------------------------------------

wb_log <- createWorkbook()

hs1 <- createStyle(fgFill = "#E34443", textDecoration = "Bold", fontName = "Arial Narrow", fontColour = "white", fontSize = 12, wrapText = T)
hs2 <- createStyle(fgFill = "#C4BD97", textDecoration = "Bold", fontName = "Roboto Condensed", fontColour = "white", fontSize = 11, wrapText = T)
hs3 <- createStyle(fgFill = "#D8E4BC", textDecoration = "Bold", fontName = "Roboto Condensed", fontSize = 11, wrapText = T)
hs4 <- createStyle(fgFill = "#D9D9D9", textDecoration = "Bold", fontName = "Roboto Condensed", fontSize = 11, wrapText = T)


# deletion_log ------------------------------------------------------------

addWorksheet(wb_log, sheetName="deletion_log")
writeData(wb_log, sheet = "deletion_log", df_deletion_log, startRow = 1, startCol = 1)
addStyle(wb_log, sheet = "deletion_log", hs1, rows = 1, cols = 1:6, gridExpand = FALSE)
setColWidths(wb = wb_log, sheet = "deletion_log", cols = 1, widths = 36)
setColWidths(wb = wb_log, sheet = "deletion_log", cols = 2, widths = 20)
setColWidths(wb = wb_log, sheet = "deletion_log", cols = 3, widths = 50)
setColWidths(wb = wb_log, sheet = "deletion_log", cols = 4, widths = 40)
setColWidths(wb = wb_log, sheet = "deletion_log", cols = 5, widths = 40)


# log book ----------------------------------------------------------------

addWorksheet(wb_log, sheetName="Log book")
writeData(wb_log, sheet = "Log book", df_formatted_log, startRow = 1, startCol = 1)
addStyle(wb_log, sheet = "Log book", hs1, rows = 1, cols = 1:9, gridExpand = FALSE)
setColWidths(wb = wb_log, sheet = "Log book", cols = 1, widths = 36)
setColWidths(wb = wb_log, sheet = "Log book", cols = 2:9, widths = 20)


# data_extract ------------------------------------------------------------

addWorksheet(wb_log, sheetName="data_extract")
writeData(wb_log, sheet = "data_extract", df_data_extract, startRow = 1, startCol = 1)
addStyle(wb_log, sheet = "data_extract", hs1, rows = 1, cols = 1:2, gridExpand = FALSE)
setColWidths(wb = wb_log, sheet = "data_extract", cols = 1, widths = 36)
setColWidths(wb = wb_log, sheet = "data_extract", cols = 2, widths = 20)


# variable_tracker --------------------------------------------------------

addWorksheet(wb_log, sheetName="variable_tracker")
writeData(wb_log, sheet = "variable_tracker", df_variable_tracker, startRow = 1, startCol = 1)
addStyle(wb_log, sheet = "variable_tracker", hs1, rows = 1, cols = 1:3, gridExpand = FALSE)
setColWidths(wb = wb_log, sheet = "variable_tracker", cols = 1:3, widths = 25)

# Enumerator - performance ------------------------------------------------

addWorksheet(wb_log, sheetName="Enumerator - performance")

setColWidths(wb = wb_log, sheet = "Enumerator - performance", cols = 1, widths = 14)
setColWidths(wb = wb_log, sheet = "Enumerator - performance", cols = 5, widths = 14)
setColWidths(wb = wb_log, sheet = "Enumerator - performance", cols = 9, widths = 14)
setColWidths(wb = wb_log, sheet = "Enumerator - performance", cols = 14, widths = 14)
setColWidths(wb = wb_log, sheet = "Enumerator - performance", cols = 18, widths = 14)
setColWidths(wb = wb_log, sheet = "Enumerator - performance", cols = 22, widths = 14)

# Dataset
writeData(wb_log, sheet = "Enumerator - performance", "Dataset", startRow = 1, startCol = 1)
addStyle(wb_log, sheet = "Enumerator - performance", hs2, rows = 1, cols = 1:2, gridExpand = FALSE)

mergeCells(wb_log, sheet = "Enumerator - performance", rows = 3:4, cols = 1:2)
writeData(wb_log, sheet = "Enumerator - performance", "Number of surveys collected by enumerators", startRow = 3, startCol = 1)
addStyle(wb_log, sheet = "Enumerator - performance", hs3, rows = 3:4, cols = 1:2, gridExpand = FALSE)

writeDataTable(wb = wb_log, sheet = "Enumerator - performance", 
               x = df_surveys_by_enum , 
               startRow = 6, startCol = 1, 
               tableStyle = "TableStyleLight10", headerStyle = hs4)

# Cleaning log 1
writeData(wb_log, sheet = "Enumerator - performance", "Cleaning log", startRow = 1, startCol = 5)
addStyle(wb_log, sheet = "Enumerator - performance", hs2, rows = 1, cols = 5:6, gridExpand = FALSE)

mergeCells(wb_log, sheet = "Enumerator - performance", rows = 3:4, cols = 5:6)
writeData(wb_log, sheet = "Enumerator - performance", "Number of changes by enumerators", startRow = 3, startCol = 5)
addStyle(wb_log, sheet = "Enumerator - performance", hs3, rows = 3:4, cols = 5:6, gridExpand = FALSE)

writeDataTable(wb = wb_log, sheet = "Enumerator - performance", 
               x = df_changes_by_enum , 
               startRow = 6, startCol = 5, 
               tableStyle = "TableStyleLight10", headerStyle = hs4)

# Cleaning log 2
writeData(wb_log, sheet = "Enumerator - performance", "Cleaning log", startRow = 1, startCol = 9)
addStyle(wb_log, sheet = "Enumerator - performance", hs2, rows = 1, cols = 9:11, gridExpand = FALSE)

mergeCells(wb_log, sheet = "Enumerator - performance", rows = 3:4, cols = 9:11)
writeData(wb_log, sheet = "Enumerator - performance", "Number of changes by enumerators filtered by issues", startRow = 3, startCol = 9)
addStyle(wb_log, sheet = "Enumerator - performance", hs3, rows = 3:4, cols = 9:10, gridExpand = FALSE)
addStyle(wb_log, sheet = "Enumerator - performance", hs3, rows = 3:4, cols = 11, gridExpand = FALSE)

writeDataTable(wb = wb_log, sheet = "Enumerator - performance", 
               x = df_changes_by_enum_issue , 
               startRow = 6, startCol = 9, 
               tableStyle = "TableStyleLight10", headerStyle = hs4)

# Deletion log 1
writeData(wb_log, sheet = "Enumerator - performance", "Deletion log", startRow = 1, startCol = 14)
addStyle(wb_log, sheet = "Enumerator - performance", hs2, rows = 1, cols = 14:15, gridExpand = FALSE)

mergeCells(wb_log, sheet = "Enumerator - performance", rows = 3:4, cols = 14:15)
writeData(wb_log, sheet = "Enumerator - performance", "Number of deletions by enumerators", startRow = 3, startCol = 14)
addStyle(wb_log, sheet = "Enumerator - performance", hs3, rows = 3:4, cols = 14:15, gridExpand = FALSE)

writeDataTable(wb = wb_log, sheet = "Enumerator - performance", 
               x = df_deletion_by_enum , 
               startRow = 6, startCol = 14, 
               tableStyle = "TableStyleLight10", headerStyle = hs4)

# Deletion log 2
writeData(wb_log, sheet = "Enumerator - performance", "Deletion log", startRow = 1, startCol = 18)
addStyle(wb_log, sheet = "Enumerator - performance", hs2, rows = 1, cols = 18:19, gridExpand = FALSE)

mergeCells(wb_log, sheet = "Enumerator - performance", rows = 3:4, cols = 18:19)
writeData(wb_log, sheet = "Enumerator - performance", "Number of deletions due to time by enumerator", startRow = 3, startCol = 18)
addStyle(wb_log, sheet = "Enumerator - performance", hs3, rows = 3:4, cols = 18:19, gridExpand = FALSE)

writeDataTable(wb = wb_log, sheet = "Enumerator - performance", 
               x = df_deletion_by_enum_time , 
               startRow = 6, startCol = 18, 
               tableStyle = "TableStyleLight10", headerStyle = hs4)

# Deletion log 3
writeData(wb_log, sheet = "Enumerator - performance", "Deletion log", startRow = 1, startCol = 22)
addStyle(wb_log, sheet = "Enumerator - performance", hs2, rows = 1, cols = 22:24, gridExpand = FALSE)

mergeCells(wb_log, sheet = "Enumerator - performance", rows = 3:4, cols = 22:24)
writeData(wb_log, sheet = "Enumerator - performance", "Needs to be reviewed by Research Manager / lead AO before submission to HQ", startRow = 3, startCol = 22)
addStyle(wb_log, sheet = "Enumerator - performance", hs3, rows = 3:4, cols = 22:23, gridExpand = FALSE)
addStyle(wb_log, sheet = "Enumerator - performance", hs3, rows = 3:4, cols = 24, gridExpand = FALSE)

writeDataTable(wb = wb_log, sheet = "Enumerator - performance", 
               x = df_deletion_by_enum |> filter(Number > 20) |> 
                   mutate(`issue(s) followed up in country y/n` = "Yes",
                          `further comments` = NA_character_) |> select(-Number), 
               startRow = 6, startCol = 22, 
               tableStyle = "TableStyleLight10", headerStyle = hs4)


saveWorkbook(wb_log, paste0("outputs/", butteR::date_file_prefix(),"_eth_aba_data_cleaning_logbook.xlsx"), overwrite = TRUE)
openXL(file = paste0("outputs/", butteR::date_file_prefix(),"_eth_aba_data_cleaning_logbook.xlsx"))

################################################################################
