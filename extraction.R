################################################################################
rm(list = ls())
# Applying the cleaning log to clean the data
library(tidyverse)
library(lubridate)
library(glue)
library(magrittr)
library(stringi)
library(stringr)
library(kobold)
library(supporteR)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#data
getwd()
df<- readxl::read_excel("review_log.xlsx")

data <- df|>
  filter(!df.new_value %in%c("TRUE",
                              "FALSE"))|>
  filter(!df.old_value %in% c("other", " otehr", "other "))|>
  filter(!cl.old_value %in% c("other", " other", "other "))

openxlsx::write.xlsx(x = data,
                     file = paste0("outputs/", butteR::date_file_prefix(), 
                                   "_clean.xlsx"), 
                     overwrite = TRUE, keepNA = TRUE, na.string = "NA")
