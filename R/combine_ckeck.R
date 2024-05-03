###############################################################################
# checks for data collection
# read packages
rm(list = ls())

library(tidyverse)
library(lubridate)
library(glue)
library(readxl)
library(supporteR)
getwd()

# Spécifier le chemin du répertoire contenant les fichiers Excel
data_path <- "Feedback daily checkin"


# Obtenir une liste de tous les fichiers Excel dans le répertoire
excel_file <- list.files(path = data_path, pattern = "\\.xlsx$", full.names = TRUE)

# Créer une liste pour stocker les données de chaque fichier Excel
data_list <- list()

# Parcourir tous les fichiers Excel dans la liste et les lire
for (file in excel_file) {
  # Lire le fichier Excel et stocker les données dans la liste
  data <- read_excel(file)
  
  # Stocker les données dans la liste sous le nom du fichier
  my_file <- basename(file)
  data_list[[my_file]] <- data
}

# accéder aux données des fichiers

df_combine_data <-bind_rows(data_list[[1]], data_list[[2]], data_list[[3]]) 

# output the log --------------------------------------------------------------

write_csv(x = df_combine_data, file = paste0("outputs/", butteR::date_file_prefix(), 
                                                "_combined_checks_eth_aba_somali_carlos.csv"), na = "")

