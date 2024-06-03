rm(list = ls())
library(tidyverse)
library(openxlsx)
library(readxl)
read_excel_allsheets <- function(filename, tibble = T) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}
source("R/export_in_reach_format_multiple_sheets.R")

# global options can be set to further simplify things
options("openxlsx.borderStyle" = "thin")
# options("openxlsx.borderColour" = "#4F81BD")
options("openxlsx.withFilter" = FALSE)

# tool
loc_tool <- "inputs/REACH_ETH2306b_ABA_tool_FINAL.xlsx"
df_survey <- readxl::read_excel(loc_tool, sheet = "survey")
df_choices <- readxl::read_excel(loc_tool, sheet = "choices") |> 
    select(list_name, choice_name = name,   choice_label =`label::English`)

# extract select types
df_tool_select_type <- df_survey |> 
    select(type, qn_name = name) |> 
    filter(str_detect(string = type, pattern = "integer|date|select_one|select_multiple")) |> 
    separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop" )

# extract choice ids and labels
df_choices_support <- df_choices |> 
    left_join(df_tool_select_type) |> 
    unite("survey_choice_id", qn_name, choice_name, sep = "_", remove = FALSE) |> 
    select(survey_choice_id, choice_label) 

# extract groups
df_tool_groups <- df_survey |> 
    mutate(int.group = ifelse(str_detect(string = name, pattern = "^grp_"), name, NA_character_),
           i.group = int.group) |> 
    fill(i.group) |> 
    filter(!str_detect(string = name, pattern = "_other$"),
           !str_detect(string = type, pattern = "group|repeat|text|geopoint|^gps$|^note$"),
           !is.na(i.group)
    ) |> 
    select(type, name, `label::English`, i.group) ### I removed "in_number" in this selection

# support composite grps and labels
df_support_composite_grps <- readxl::read_excel("support_files/support composite grps and labels.xlsx")


df_support_integer_col_labs <- readxl::read_excel("support_files/support integer column names.xlsx") |>
    filter(!is.na(integer_column_label))

# analysis
df_analysis <- read_csv("outputs/full_analysis_aba_somali.csv")|> 
    mutate(analysis_choice_id = case_when(select_type %in% c("select_multiple", "select multiple") ~ str_replace(string = `choices/options`, 
                                                                                                                 pattern = "\\/", replacement = "_"),
                                          select_type %in% c("select_one", "select one") ~ paste0(variable, "_", `choices/options`)),
           analysis_choice_id = ifelse(variable %in% c("i.hoh_age"), paste0("hoh_age_", `choices/options`), analysis_choice_id),
           analysis_choice_id = ifelse(variable %in% c("i.hoh_gender"), paste0("hoh_gender_", `choices/options`), analysis_choice_id),
          
           Question = ifelse(variable %in% df_support_composite_grps$composite_code, recode(variable, !!!setNames(df_support_composite_grps$composite_qn_label, df_support_composite_grps$composite_code)), Question),
           Question = str_replace_all(string = Question, pattern = "\\*", replacement = ""),
           Question = str_replace_all(string = Question, pattern = "was \\[name\\] enrolled", replacement = "was atleast a school aged child enrolled"),
           Question = str_replace_all(string = Question, pattern = "Has \\[woman_name\\]", replacement = "Has atleast a member of the household"),
           Question = str_replace_all(string = Question, pattern = "did \\[woman_name\\] give birth", replacement = "wdid she give birth"),
           Question = str_replace_all(string = Question, pattern = "assisted \\[woman_name\\]", replacement = "assisted her"),
           Question = str_replace_all(string = Question, pattern = "Has \\[Child Name\\]", replacement = "Has any child"),
           Question = str_replace_all(string = Question, pattern = "If \\[woman_name\\]", replacement = "If she")
           )

  # identify indicators

data_list<- readxl::read_excel("inputs/Service Capabilities ABA_HH Questionaire DAP_Final.xlsx", sheet = "Service capabilities ABA HH DAP")

df_dap_questions <-data_list|> 
    filter(!is.na(Indicator))|> 
    janitor::clean_names() |> 
    select(sector,indicator,kobo_code,`questionnaire_question`,`questionnaire_responses`)|>
  rename(name="kobo_code")|>
  mutate(sector = str_replace_all(string = sector, pattern = "\\/|\\?", replacement = "_"))

df_tool_dap_info <- df_tool_groups |> 
    left_join(df_dap_questions) |> 
    mutate(qn_number = row_number())


# format the data ---------------------------------------------------------

df_analysis_dap_info <- df_analysis |> 
    left_join(df_tool_dap_info |> 
                  select(name,sector,indicator,questionnaire_question,qn_number), by = c("variable" = "name"))|>
    mutate(response_lable = recode(analysis_choice_id, !!!setNames(df_choices_support$choice_label, df_choices_support$survey_choice_id)),
           choices = ifelse(is.na(response_lable),`choices/options`,response_lable),
           subset_1_val = ifelse(is.na(subset_1_val), "All",
                                 ifelse(subset_1_val == "none", "none",
                                       ifelse(subset_1_val == "hh_host_idp", "host",
                                             ifelse(subset_1_val == "hh_host_returnee", "host", subset_1_val)))),
           # subset_1_val_label = recode(subset_1_val, !!!setNames(df_choices$choice_label, df_choices$choice_name)),
           # subset_1_val_label =  ifelse(is.na(subset_1_val_label), "Zone", subset_1_val_label)
           sector = ifelse(is.na(sector) & variable %in% df_support_composite_grps$composite_code, recode(variable, !!!setNames(df_support_composite_grps$grp_label, df_support_composite_grps$composite_code)), sector),
           select_type = ifelse((is.na(select_type)) & variable %in% df_support_composite_grps$composite_code, recode(variable, !!!setNames(df_support_composite_grps$composite_type, df_support_composite_grps$composite_code)), select_type),
           choices = ifelse(variable %in% df_support_integer_col_labs$variable, recode(variable, !!!setNames(df_support_integer_col_labs$integer_column_label, df_support_integer_col_labs$variable)), choices)
 
           ) |> 
    select(-c(n_unweighted, subset_1_name)) |>
    filter(!is.na(sector))|>
    filter(!is.na(subset_1_val), !choices%in% c("Other (specify)", "Other (please specify)", "other"), variable != "unmet_needs_reason")
  
  
# filter quantitave analysis

df_analysis_dap_info_quantitave <- df_analysis_dap_info|>
  filter(select_type == "integer")

#Filter qualitative analysis
df_analysis_dap_info_qualitative <- df_analysis_dap_info|>
  filter(select_type %in% c("select_one","select_multiple") )



# output full_analysis

#export
export_in_reach_format_multiple_sheets(  
  file_R_1 = df_analysis_dap_info_qualitative, sheet_name_1 = "full_df_input_perc",
  file_R_2 = df_analysis_dap_info_quantitave, sheet_name_2 = "full_df_input_moy",
  file_R_3 = df_analysis_dap_info ,  sheet_name_3 = "full_df_analysis_info",
  export_file_name =   paste0( butteR::date_file_prefix(), "_ETH Analysis ABA Somali", na=""),
  export_directory = "outputs/")


# split data based on groups or sectors
output <- split(df_analysis_dap_info, df_analysis_dap_info$sector)
names(output)
wb <- createWorkbook()

hs1 <- createStyle(fgFill = "#EE5859", halign = "CENTER", textDecoration = "Bold", fontColour = "white", fontSize = 16, wrapText = T)
hs2 <- createStyle(fgFill = "#808080", halign = "CENTER", textDecoration = "Bold", fontColour = "white", wrapText = T)
hs3 <- createStyle(fgFill = "#EE5859", halign = "CENTER", textDecoration = "Bold", border = "Bottom", fontColour = "white")
# numbers
number_2digit_style <- openxlsx::createStyle(numFmt = "0.")
number_1digit_style <- openxlsx::createStyle(numFmt = "0.")
number_style <- openxlsx::createStyle(numFmt = "0")

for (i in 1:length(output)) {
    addWorksheet(wb, sheetName=names(output[i]))

    # add header to sheet
    mergeCells(wb, sheet = names(output[i]), rows = 1, cols = 1:8)
    writeData(wb, sheet = names(output[i]), names(output[i]), startCol = 1, startRow = 1, headerStyle = hs1)
    addStyle(wb, sheet = names(output[i]), hs1, rows = 1, cols = 1:8, gridExpand = TRUE)

    setColWidths(wb = wb, sheet = names(output[i]), cols = 2, widths = 90)

    # get current data for the group or sector
    # get current data for the group or sector
    # get current data for the group or sector
    current_sheet_data <- output[[i]] |>
      pivot_wider(names_from = subset_1_val, values_from = `Results(mean/percentage)`, values_fn = mean) |>
      arrange(qn_number) |>
      mutate(row_id = row_number())


    # split variables to be written in different tables with in a sheet
    sheet_variables_data <- split(current_sheet_data, factor(current_sheet_data$variable, levels = unique(current_sheet_data$variable)))

    previous_row_end <- 2

    for (j in 1:length(sheet_variables_data)) {

        current_variable_data <- sheet_variables_data[[j]]

        get_question <- current_variable_data |> select(Question) |> unique() |> pull()
        get_qn_type <- current_variable_data |> select(select_type) |> unique() |> pull()

        if(get_qn_type %in% c("select_one", "select one", "select_multiple", "select multiple")){

          class(current_variable_data$All) <- "percentage"
            # class(current_variable_data$host_idp) <- "percentage"
            class(current_variable_data$host) <- "percentage"
            # class(current_variable_data$idp) <- "percentage"
            # class(current_variable_data$host_returnee) <- "percentage"
            class(current_variable_data$female) <- "percentage"
            class(current_variable_data$male) <- "percentage"
            # class(current_variable_data$none) <- "percentage"
            class(current_variable_data$age_18_24) <- "percentage"
            class(current_variable_data$age_25_39) <- "percentage"
            class(current_variable_data$age_40_59) <- "percentage"
            class(current_variable_data$`age_60+`) <- "percentage"
            # class(current_variable_data$Somali) <- "percentage"
            class(current_variable_data$Somali) <- "percentage"

        }else{

            class(current_variable_data$All) <- "numeric"
            # class(current_variable_data$host_idp) <- "numeric"
            class(current_variable_data$host) <- "numeric"
            # class(current_variable_data$idp) <- "numeric"
            # class(current_variable_data$host_returnee) <- "numeric"
            class(current_variable_data$female) <- "numeric"
            class(current_variable_data$male) <- "numeric"
            # class(current_variable_data$none) <- "numeric"
            class(current_variable_data$age_18_24) <- "numeric"
            class(current_variable_data$age_25_39) <- "numeric"
            class(current_variable_data$age_40_59) <- "numeric"
            class(current_variable_data$`age_60+`) <- "numeric"
            # class(current_variable_data$Somali) <- "numeric"
            class(current_variable_data$Somali) <- "numeric"

        }

        current_row_start <- previous_row_end + 3

        print(current_row_start)

        # add header for variable
        mergeCells(wb, sheet = names(output[i]), rows = previous_row_end + 2, cols = 1:8)
        writeData(wb, sheet = names(output[i]), get_question, startCol = 1, startRow = previous_row_end + 2)
        addStyle(wb, sheet = names(output[i]), hs2, rows = previous_row_end + 2, cols = 1:8, gridExpand = TRUE)

        current_data_length <- max(current_variable_data$row_id) - min(current_variable_data$row_id)

        addStyle(wb, sheet = names(output[i]), number_1digit_style, rows = current_row_start + 1 : current_row_start + 1 + current_data_length, cols = 1:8, gridExpand = TRUE)

        writeDataTable(wb = wb,
                       sheet = names(output[i]),
                       x = current_variable_data |>
                           select(-c(Question,
                                     questionnaire_question,
                                     `choices/options`,
                                     population,
                                      analysis_choice_id,
                                     sector,
                                     response_lable,
                                     row_id, variable,
                                     select_type,
                                     indicator,
                                     qn_number)
                           ),
                       startRow = current_row_start,
                       startCol = 1,
                       tableStyle = "TableStyleLight9",
                       headerStyle = hs3)

        previous_row_end <- current_row_start + 1 + current_data_length
    }
    # hide grid lines
    showGridLines(wb,  names(output[i]), showGridLines = FALSE)
}

# worksheets order

# worksheetOrder(wb) <- c(3,4,1,2,5,6,7,8,9)
#
 activeSheet(wb) <- 1

saveWorkbook(wb, paste0("outputs/", butteR::date_file_prefix(),"_formatted_analysis_eth_somali.xlsx"), overwrite = TRUE)
openXL(file = paste0("outputs/", butteR::date_file_prefix(),"_formatted_analysis_eth_somali.xlsx"))

