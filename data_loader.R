#####Load data
library(dplyr)
library(readr)
library(lubridate)
library(readxl)
#library(sf)
#### Install des packages
#install.packages("readxl", dependencies=TRUE)
install.packages("odbc", dependencies = TRUE)
install.packages("pool",dependencies = TRUE)
install.packages("RMySQL", dependencies = TRUE)
install.packages("janitor", dependencies = TRUE)

####install.packages(c("package1", "package2", "package3", "package4", "package5"),dependencies = TRUE)

load_lab_data <- function(file_path = "App_suivi_LabMap_indicator/data/Data_LabMab_2025_merge_final_LabMab_29_09_2025_for_R_.xlsx") {
  # Charger les données
  #df <- read_csv(file_path, col_types = cols(.default = "c"))
  df<- read_excel(file_path, col_types = cols(.default = "c"))
  
  # Nettoyer et standardiser les données
  library(dplyr)
  library(readxl)
  
  clean_data <- function(file_path) {
    # Charger les données
    df <- read_excel(file_path, col_types = cols(.default = "c"))
    
    # Nettoyer et standardiser les données
    df_clean <- df %>%
      dplyr::mutate(  # Spécifier explicitement le package
        Date_de_l_enquête = as.Date(Date_de_l_enquête),
        
        Type_d_établissement = dplyr::case_when(
          grepl("public", tolower(Type_d_établissement)) ~ "Public",
          grepl("private", tolower(Type_d_établissement)) ~ "Privé",
          grepl("confessionnel", tolower(Type_d_établissement)) ~ "Confessionnel",
          TRUE ~ "Autre"
        ),
        
        Niveau_complexité = dplyr::case_when(
          grepl("level i", tolower(Sélectionnez_le_niveau_de_complexité_du_laboratoire)) ~ "Niveau I",
          grepl("level ii", tolower(Sélectionnez_le_niveau_de_complexité_du_laboratoire)) ~ "Niveau II",
          grepl("level iii", tolower(Sélectionnez_le_niveau_de_complexité_du_laboratoire)) ~ "Niveau III",
          grepl("level iv", tolower(Sélectionnez_le_niveau_de_complexité_du_laboratoire)) ~ "Niveau IV",
          TRUE ~ "Non spécifié"
        ),
        
        BSL3 = grepl("bsl_3", tolower(Niveau_de_biosécurité)),
        
        Surveillance_génomique = dplyr::case_when(
          grepl("yes", tolower(Surveillance_génomique)) ~ TRUE,
          TRUE ~ FALSE
        ),
        
        Tests_VIH = as.numeric(Tests_rapides_d_anticorps_contre_le_VIH)
      )
    
    return(df_clean)
  }
}  
# Fonction pour mettre à jour les indicateurs
update_indicators <- function(df) {
  list(
    total_labs = nrow(df),
    bsl3_count = sum(df$BSL3, na.rm = TRUE),
    bsl3_percentage = round(mean(df$BSL3, na.rm = TRUE) * 100, 1),
    genomic_count = sum(df$Surveillance_génomique, na.rm = TRUE),
    avg_tests = round(mean(df$Tests_VIH, na.rm = TRUE), 0)
  )
}