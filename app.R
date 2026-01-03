###### Appli suivi des indicateurs de LabMap - VERSION DEPLOIEMENT #####

library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(janitor)
library(stringr)

# === CONFIGURATION POUR SHINYAPPS.IO ===
options(
  shiny.maxRequestSize = 50*1024^2, # 50MB pour les uploads
  repos = c(CRAN = "https://cran.rstudio.com/"),
  encoding = "UTF-8"
)

# === FONCTIONS DE CHARGEMENT DES DONNÉES ===
load_lab_data <- function(file_path = NULL) {
  
  # Données d'exemple intégrées pour éviter les fichiers externes
  if(is.null(file_path) || !file.exists(file_path)) {
    return(create_sample_data())
  }
  
  tryCatch({
    df <- read_excel(file_path, .name_repair = "unique")
    
    # Afficher les noms de colonnes pour le débogage
    message("Colonnes chargées: ", paste(names(df), collapse = ", "))
    
    # Nettoyer et transformer les données
    df_clean <- df
    
    # Gestion de la date
    date_col <- names(df_clean)[grepl("date.*enquête", names(df_clean), ignore.case = TRUE)]
    if(length(date_col) > 0) {
      df_clean <- df_clean %>%
        mutate(Date_de_l_enquête = as.Date(!!sym(date_col[1])))
    } else {
      df_clean <- df_clean %>%
        mutate(Date_de_l_enquête = as.Date("2024-01-01"))
    }
    
    # Gestion du type d'établissement
    type_col <- names(df_clean)[grepl("type.*établissement", names(df_clean), ignore.case = TRUE)]
    if(length(type_col) > 0) {
      df_clean <- df_clean %>%
        mutate(
          Type_d_établissement = case_when(
            grepl("public", tolower(!!sym(type_col[1]))) ~ "Public",
            grepl("private", tolower(!!sym(type_col[1]))) ~ "Privé", 
            grepl("confessionnel", tolower(!!sym(type_col[1]))) ~ "Confessionnel",
            TRUE ~ as.character(!!sym(type_col[1]))
          )
        )
    } else {
      df_clean <- df_clean %>%
        mutate(Type_d_établissement = "Non spécifié")
    }
    
    # ANALYSE DE LA RÉPARTITION DES LABORATOIRES PAR NIVEAU DE COMPLEXITÉ
    col_complexite <- names(df_clean)[str_detect(names(df_clean), regex("complex|niveau", ignore_case = TRUE))]
    
    if(length(col_complexite) > 0){
      message("Colonne de complexité détectée: ", col_complexite[1])
      col_complexite <- col_complexite[1]
      
      df_clean <- df_clean %>%
        mutate(
          Niveau_complexité = case_when(
            tolower(str_trim(as.character(!!sym(col_complexite)))) %in% c("level i", "niveau i", "i", "1", "faible") ~ "Level I",
            tolower(str_trim(as.character(!!sym(col_complexite)))) %in% c("level ii", "niveau ii", "ii", "2", "moyen") ~ "Level II",
            tolower(str_trim(as.character(!!sym(col_complexite)))) %in% c("level iii", "niveau iii", "iii", "3", "élevé", "eleve") ~ "Level III",
            tolower(str_trim(as.character(!!sym(col_complexite)))) %in% c("level iv", "niveau iv", "iv", "4", "très élevé", "tres eleve") ~ "Level IV",
            grepl("level i|niveau i|^i$|^1$|faible", tolower(str_trim(as.character(!!sym(col_complexite))))) ~ "Level I",
            grepl("level ii|niveau ii|^ii$|^2$|moyen", tolower(str_trim(as.character(!!sym(col_complexite))))) ~ "Level II",
            grepl("level iii|niveau iii|^iii$|^3$|élevé|eleve", tolower(str_trim(as.character(!!sym(col_complexite))))) ~ "Level III",
            grepl("level iv|niveau iv|^iv$|^4$|très élevé|tres eleve", tolower(str_trim(as.character(!!sym(col_complexite))))) ~ "Level IV",
            is.na(!!sym(col_complexite)) ~ "Non spécifié",
            str_trim(as.character(!!sym(col_complexite))) == "" ~ "Non spécifié",
            TRUE ~ "Autre/Non classé"
          )
        )
    } else {
      df_clean <- df_clean %>%
        mutate(Niveau_complexité = "Non spécifié")
    }
    
    # Assurer que tous les niveaux sont présents
    all_levels <- c("Level I", "Level II", "Level III", "Level IV", "Non spécifié", "Autre/Non classé")
    df_clean <- df_clean %>%
      mutate(Niveau_complexité = factor(Niveau_complexité, levels = all_levels))
    
    # Gestion de BSL3
    biosafety_col <- names(df_clean)[grepl("niveau.*biosécurité|biosécurité", names(df_clean), ignore.case = TRUE)]
    if(length(biosafety_col) > 0) {
      df_clean <- df_clean %>%
        mutate(BSL3 = grepl("bsl_3|niveau 3", tolower(!!sym(biosafety_col[1]))))
    } else {
      df_clean <- df_clean %>%
        mutate(BSL3 = FALSE)
    }
    
    # Gestion de la surveillance génomique
    genomic_cols <- names(df_clean)[grepl("surveillance.*génomique|génomique", names(df_clean), ignore.case = TRUE)]
    
    if(length(genomic_cols) > 0) {
      df_clean <- df_clean %>%
        mutate(
          across(
            all_of(genomic_cols),
            ~ case_when(
              tolower(as.character(.)) %in% c("yes", "oui", "true", "vrai", "1") ~ TRUE,
              tolower(as.character(.)) %in% c("no", "non", "false", "faux", "0") ~ FALSE,
              TRUE ~ FALSE
            ),
            .names = "surveillance_{.col}"
          )
        ) %>%
        rowwise() %>%
        mutate(
          Surveillance_génomique = any(c_across(starts_with("surveillance_")), na.rm = TRUE)
        ) %>%
        ungroup()
    } else {
      df_clean <- df_clean %>%
        mutate(Surveillance_génomique = FALSE)
    }
    
    # Gestion des tests VIH
    tests_cols <- names(df_clean)[grepl("tests.*rapides.*anticorps.*vih|tests.*vih", names(df_clean), ignore.case = TRUE)]
    
    if(length(tests_cols) > 0) {
      df_clean <- df_clean %>%
        mutate(
          Tests_VIH = case_when(
            tolower(as.character(!!sym(tests_cols[1]))) %in% c("yes", "oui", "true", "vrai", "1") ~ 1,
            tolower(as.character(!!sym(tests_cols[1]))) %in% c("no", "non", "false", "faux", "0") ~ 0,
            TRUE ~ as.numeric(as.character(!!sym(tests_cols[1])))
          )
        )
    } else {
      df_clean <- df_clean %>%
        mutate(Tests_VIH = NA_real_)
    }
    
    # Gestion de la localisation
    location_cols <- names(df_clean)[grepl("ville|district|région|location", names(df_clean), ignore.case = TRUE)]
    if(length(location_cols) > 0) {
      df_clean <- df_clean %>%
        mutate(Ville_District = as.character(!!sym(location_cols[1])))
    } else {
      df_clean <- df_clean %>%
        mutate(Ville_District = "Non spécifié")
    }
    
    # Gestion du nom du laboratoire
    lab_name_cols <- names(df_clean)[grepl("nom.*laboratoire|laboratoire", names(df_clean), ignore.case = TRUE)]
    if(length(lab_name_cols) > 0) {
      df_clean <- df_clean %>%
        mutate(Nom_du_Laboratoire = as.character(!!sym(lab_name_cols[1])))
    } else {
      df_clean <- df_clean %>%
        mutate(Nom_du_Laboratoire = paste0("Lab_", 1:n()))
    }
    
    # Gestion du pays
    country_cols <- names(df_clean)[grepl("pays", names(df_clean), ignore.case = TRUE)]
    if(length(country_cols) > 0) {
      df_clean <- df_clean %>%
        mutate(Pays = as.character(!!sym(country_cols[1])))
    } else {
      df_clean <- df_clean %>%
        mutate(Pays = "Non spécifié")
    }
    
    return(df_clean)
    
  }, error = function(e) {
    message("Erreur lors du chargement: ", e$message)
    return(create_sample_data())
  })
}

# Fonction pour créer des données d'exemple
create_sample_data <- function() {
  n <- 200
  
  data.frame(
    Date_de_l_enquête = sample(seq(as.Date('2024-01-01'), as.Date('2025-01-01'), by="day"), n, replace = TRUE),
    Type_d_établissement = sample(c("Public", "Privé", "Confessionnel", "Autre"), n, replace = TRUE, prob = c(0.6, 0.25, 0.1, 0.05)),
    Niveau_complexité = sample(c("Level I", "Level II", "Level III", "Level IV", "Non spécifié"), n, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.05, 0.05)),
    Ville_District = sample(c("Yaoundé", "Douala", "Garoua", "Bafoussam", "Maroua", "Bamenda"), n, replace = TRUE),
    Pays = sample(c("Cameroun", "Sénégal", "Côte d'Ivoire"), n, replace = TRUE),
    BSL3 = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.1, 0.9)),
    Surveillance_génomique = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.3, 0.7)),
    Tests_VIH = sample(c(0, 1), n, replace = TRUE, prob = c(0.2, 0.8)),
    Nom_du_Laboratoire = paste0("Lab_", 1:n),
    stringsAsFactors = FALSE
  ) %>%
    mutate(Niveau_complexité = factor(Niveau_complexité, levels = c("Level I", "Level II", "Level III", "Level IV", "Non spécifié")))
}

# === INTERFACE UTILISATEUR ===
ui <- dashboardPage(
  dashboardHeader(title = "Tableau de Bord LabMap"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Vue d'ensemble", tabName = "overview", icon = icon("dashboard")),
      menuItem("Répartition par Type", tabName = "type", icon = icon("vial")),
      menuItem("Distribution par niveau", tabName = "complexity", icon = icon("layer-group")),
      menuItem("Biosécurité", tabName = "biosafety", icon = icon("shield-alt")),
      menuItem("Analyse Géographique", tabName = "geography", icon = icon("map")),
      menuItem("Données Brutes", tabName = "rawdata", icon = icon("table")),
      menuItem("Chargement des Données", tabName = "upload", icon = icon("upload"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .info-box {min-height: 100px;}
        .info-box-content {padding: 10px;}
        .main-header .logo {font-weight: bold;}
        .box {margin-bottom: 20px;}
        .ggplot-plot {text-align: center;}
      "))
    ),
    
    tabItems(
      # Tab 1: Vue d'ensemble
      tabItem(tabName = "overview",
              fluidRow(
                box(
                  title = "Information sur les données",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  uiOutput("data_info")
                )
              ),
              fluidRow(
                valueBoxOutput("total_labs", width = 3),
                valueBoxOutput("bsl3_percentage", width = 3),
                valueBoxOutput("genomic_surveillance", width = 3),
                valueBoxOutput("avg_tests", width = 3)
              ),
              
              fluidRow(
                box(
                  title = "Répartition par Localisation",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("location_plot", height = 400),
                  width = 6
                ),
                
                box(
                  title = "Évolution Temporelle",
                  status = "info",
                  solidHeader = TRUE,
                  plotlyOutput("timeline_plot", height = 400),
                  width = 6
                )
              )
      ),
      
      # Tab 2: Répartition par type
      tabItem(tabName = "type",
              fluidRow(
                box(
                  title = "Répartition par Type d'Établissement",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("type_barplot", height = 400),
                  width = 6
                ),
                
                box(
                  title = "Détails par Type",
                  status = "info",
                  solidHeader = TRUE,
                  DTOutput("type_table"),
                  width = 6
                )
              )
      ),
      
      # Tab 3: Distribution par niveau
      tabItem(tabName = "complexity",
              fluidRow(
                box(
                  title = "Analyse de la Répartition - Graphique ggplot2",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  div(
                    style = "text-align: center;",
                    plotOutput("ggplot_complexity", height = "500px")
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Répartition Interactive",
                  status = "info",
                  solidHeader = TRUE,
                  plotlyOutput("complexity_barplot", height = 400),
                  width = 6
                ),
                
                box(
                  title = "Distribution en Camembert",
                  status = "warning",
                  solidHeader = TRUE,
                  plotlyOutput("complexity_pie", height = 400),
                  width = 6
                )
              ),
              
              fluidRow(
                box(
                  title = "Tests VIH par Niveau",
                  status = "success",
                  solidHeader = TRUE,
                  plotlyOutput("complexity_tests", height = 400),
                  width = 6
                ),
                
                box(
                  title = "Tableau de Répartition",
                  status = "danger",
                  solidHeader = TRUE,
                  DTOutput("complexity_table"),
                  width = 6
                )
              )
      ),
      
      # Tab 4: Biosécurité
      tabItem(tabName = "biosafety",
              fluidRow(
                box(
                  title = "Laboratoires BSL-3",
                  status = "danger",
                  solidHeader = TRUE,
                  plotlyOutput("bsl3_chart", height = 400),
                  width = 6
                ),
                
                box(
                  title = "Surveillance Génomique",
                  status = "warning",
                  solidHeader = TRUE,
                  plotlyOutput("genomic_chart", height = 400),
                  width = 6
                )
              )
      ),
      
      # Tab 5: Analyse Géographique
      tabItem(tabName = "geography",
              fluidRow(
                box(
                  title = "Répartition par Pays",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("country_plot", height = 400),
                  width = 6
                ),
                
                box(
                  title = "Top Villes/Districts",
                  status = "info",
                  solidHeader = TRUE,
                  plotlyOutput("top_cities_plot", height = 400),
                  width = 6
                )
              )
      ),
      
      # Tab 6: Données brutes
      tabItem(tabName = "rawdata",
              fluidRow(
                box(
                  title = "Données des Laboratoires",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("raw_data_table")
                )
              )
      ),
      
      # Tab 7: Chargement des données
      tabItem(tabName = "upload",
              fluidRow(
                box(
                  title = "Charger vos données",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  fileInput("file_upload", "Choisir un fichier Excel (.xlsx, .xls)",
                            accept = c(".xlsx", ".xls")),
                  actionButton("load_data", "Charger les données", 
                               icon = icon("upload"), class = "btn-success")
                ),
                
                box(
                  title = "Instructions",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  tags$ul(
                    tags$li("Formats supportés: .xlsx, .xls"),
                    tags$li("Taille max: 50MB"),
                    tags$li("Les colonnes sont détectées automatiquement"),
                    tags$li("Données d'exemple affichées en attendant")
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Statut du chargement",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  verbatimTextOutput("upload_status")
                )
              )
      )
    )
  )
)

# === SERVEUR ===
server <- function(input, output, session) {
  
  # Données réactives
  lab_data <- reactiveVal()
  data_source <- reactiveVal("exemple")
  
  # Initialisation avec des données d'exemple
  observe({
    data <- create_sample_data()
    lab_data(data)
  })
  
  # Gestion du chargement de fichier
  observeEvent(input$load_data, {
    req(input$file_upload)
    
    # Validation du fichier
    if(!grepl("\\.xlsx?$", input$file_upload$name)) {
      showNotification("Veuillez uploader un fichier Excel (.xlsx ou .xls)", type = "error")
      return()
    }
    
    tryCatch({
      df <- load_lab_data(input$file_upload$datapath)
      lab_data(df)
      data_source("fichier")
      
      output$upload_status <- renderText({
        data <- lab_data()
        paste("✅ Fichier chargé avec succès:", input$file_upload$name,
              "\n📊 Lignes:", nrow(data),
              "\n🏥 Niveaux détectés:", paste(unique(data$Niveau_complexité), collapse = ", "))
      })
      
      showNotification("Données chargées avec succès!", type = "message")
      
    }, error = function(e) {
      output$upload_status <- renderText({
        paste("❌ Erreur lors du chargement:", e$message)
      })
      showNotification("Erreur lors du chargement du fichier", type = "error")
    })
  })
  
  # Fonction pour calculer la répartition
  compute_repartition <- reactive({
    data <- lab_data()
    
    if("Niveau_complexité" %in% names(data)) {
      repartition <- data %>%
        filter(!is.na(Niveau_complexité), Niveau_complexité != "") %>%
        count(Niveau_complexité) %>%
        mutate(
          pourcentage = round(n / sum(n) * 100, 1)
        ) %>%
        arrange(Niveau_complexité)
      
      return(repartition)
    } else {
      return(NULL)
    }
  })
  
  # Graphique ggplot2
  output$ggplot_complexity <- renderPlot({
    repartition <- compute_repartition()
    
    if(!is.null(repartition) && nrow(repartition) > 0) {
      ggplot(repartition, aes(x = Niveau_complexité, y = n, fill = Niveau_complexité)) +
        geom_col(width = 0.7) +
        geom_text(aes(label = paste0(n, " (", pourcentage, "%)")),
                  vjust = -0.5, size = 5) +
        scale_fill_brewer(palette = "Set2") +
        labs(
          title = "Répartition des laboratoires par niveau de complexité",
          x = "Niveau de complexité",
          y = "Nombre de laboratoires",
          fill = "Niveau"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none"
        ) +
        ylim(0, max(repartition$n) * 1.1)
    } else {
      ggplot() +
        annotate("text", x = 1, y = 1, label = "Données sur les niveaux de complexité non disponibles", size = 6) +
        theme_void()
    }
  })
  
  # Graphiques interactifs
  output$complexity_barplot <- renderPlotly({
    repartition <- compute_repartition()
    
    if(!is.null(repartition) && nrow(repartition) > 0) {
      plot_ly(repartition, x = ~Niveau_complexité, y = ~n, type = "bar",
              text = ~paste0(n, " (", pourcentage, "%)"), textposition = "auto",
              marker = list(color = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd")),
              hoverinfo = "text",
              hovertext = ~paste0("<b>", Niveau_complexité, "</b><br>",
                                  n, " laboratoires<br>",
                                  pourcentage, "%")) %>%
        layout(
          title = list(text = "Distribution par Niveau de Laboratoire", x = 0.5),
          xaxis = list(title = "Niveau de complexité", tickangle = 45),
          yaxis = list(title = "Nombre de laboratoires"),
          showlegend = FALSE
        )
    } else {
      plot_ly() %>%
        add_annotations(
          text = "Données sur les niveaux de complexité non disponibles",
          x = 0.5, y = 0.5, xref = "paper", yref = "paper",
          showarrow = FALSE
        )
    }
  })
  
  output$complexity_pie <- renderPlotly({
    repartition <- compute_repartition()
    
    if(!is.null(repartition) && nrow(repartition) > 0) {
      plot_ly(repartition, labels = ~Niveau_complexité, values = ~n, type = "pie",
              textinfo = "label+percent",
              marker = list(colors = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd")),
              hole = 0.4) %>%
        layout(
          title = list(text = "Répartition par Niveau de Laboratoire", x = 0.5),
          showlegend = TRUE
        )
    } else {
      plot_ly() %>%
        add_annotations(
          text = "Données non disponibles",
          x = 0.5, y = 0.5, xref = "paper", yref = "paper",
          showarrow = FALSE
        )
    }
  })
  
  output$complexity_tests <- renderPlotly({
    data <- lab_data()
    
    if("Niveau_complexité" %in% names(data) && "Tests_VIH" %in% names(data)) {
      test_summary <- data %>%
        group_by(Niveau_complexité) %>%
        summarise(
          Pourcentage_Tests_VIH = round(mean(Tests_VIH, na.rm = TRUE) * 100, 1),
          Count = n()
        )
      
      plot_ly(test_summary, x = ~Niveau_complexité, y = ~Pourcentage_Tests_VIH, type = "bar",
              marker = list(color = "orange")) %>%
        layout(
          title = "Tests VIH par Niveau de Complexité",
          xaxis = list(title = "Niveau de complexité", tickangle = 45),
          yaxis = list(title = "% de laboratoires faisant les tests", range = c(0, 100))
        )
    } else {
      plot_ly() %>%
        add_annotations(
          text = "Données incomplètes pour ce graphique",
          x = 0.5, y = 0.5, xref = "paper", yref = "paper",
          showarrow = FALSE
        )
    }
  })
  
  output$complexity_table <- renderDT({
    repartition <- compute_repartition()
    
    if(!is.null(repartition) && nrow(repartition) > 0) {
      datatable(
        repartition,
        options = list(pageLength = 10, dom = 'tip'),
        rownames = FALSE,
        colnames = c('Niveau de complexité', 'Nombre', 'Pourcentage (%)')
      )
    } else {
      datatable(data.frame(Message = "Données non disponibles"), rownames = FALSE)
    }
  })
  
  # Information sur les données
  output$data_info <- renderUI({
    data <- lab_data()
    source_text <- if(data_source() == "exemple") {
      "Données d'exemple affichées. Chargez vos données dans l'onglet 'Chargement des Données'"
    } else {
      paste("Données chargées depuis:", input$file_upload$name)
    }
    
    tagList(
      div(class = "alert alert-info",
          strong("Source: "), source_text,
          br(),
          strong("Nombre de laboratoires: "), nrow(data),
          br(),
          strong("Période couverte: "), 
          if("Date_de_l_enquête" %in% names(data)) {
            paste(min(data$Date_de_l_enquête, na.rm = TRUE), "à", max(data$Date_de_l_enquête, na.rm = TRUE))
          } else {
            "Non disponible"
          }
      )
    )
  })
  
  # Indicateurs principaux
  output$total_labs <- renderValueBox({
    data <- lab_data()
    valueBox(nrow(data), "Laboratoires Total", icon = icon("flask"), color = "blue")
  })
  
  output$bsl3_percentage <- renderValueBox({
    data <- lab_data()
    if("BSL3" %in% names(data)) {
      bsl3_count <- sum(data$BSL3, na.rm = TRUE)
      percentage <- ifelse(nrow(data) > 0, round((bsl3_count / nrow(data)) * 100, 1), 0)
      valueBox(paste0(percentage, "%"), "Laboratoires BSL-3", icon = icon("shield-alt"), color = "red")
    } else {
      valueBox("N/A", "Laboratoires BSL-3", icon = icon("shield-alt"), color = "yellow")
    }
  })
  
  output$genomic_surveillance <- renderValueBox({
    data <- lab_data()
    if("Surveillance_génomique" %in% names(data)) {
      genomic_count <- sum(data$Surveillance_génomique, na.rm = TRUE)
      percentage <- ifelse(nrow(data) > 0, round((genomic_count / nrow(data)) * 100, 1), 0)
      valueBox(paste0(genomic_count, " (", percentage, "%)"), "Surveillance Génomique", icon = icon("dna"), color = "purple")
    } else {
      valueBox("N/A", "Surveillance Génomique", icon = icon("dna"), color = "yellow")
    }
  })
  
  output$avg_tests <- renderValueBox({
    data <- lab_data()
    if("Tests_VIH" %in% names(data)) {
      tests_percentage <- round(mean(data$Tests_VIH, na.rm = TRUE) * 100, 1)
      valueBox(paste0(tests_percentage, "%"), "Labos faisant Tests VIH", icon = icon("vial"), color = "orange")
    } else {
      valueBox("N/A", "Labos faisant Tests VIH", icon = icon("vial"), color = "yellow")
    }
  })
  
  # Autres graphiques
  output$type_barplot <- renderPlotly({
    data <- lab_data()
    
    if("Type_d_établissement" %in% names(data)) {
      type_summary <- data %>%
        count(Type_d_établissement) %>%
        mutate(percentage = round(n / sum(n) * 100, 1))
      
      plot_ly(type_summary, x = ~Type_d_établissement, y = ~n, type = "bar",
              marker = list(color = "steelblue")) %>%
        layout(
          title = "Répartition par Type d'Établissement",
          xaxis = list(title = "Type d'établissement"),
          yaxis = list(title = "Nombre de laboratoires")
        )
    } else {
      plot_ly() %>%
        add_annotations(
          text = "Données non disponibles",
          x = 0.5, y = 0.5, xref = "paper", yref = "paper",
          showarrow = FALSE
        )
    }
  })
  
  output$type_table <- renderDT({
    data <- lab_data()
    
    if("Type_d_établissement" %in% names(data)) {
      summary_table <- data %>%
        group_by(Type_d_établissement) %>%
        summarise(Nombre = n(), Pourcentage = round(n() / nrow(data) * 100, 1))
      
      datatable(summary_table, options = list(pageLength = 5), rownames = FALSE)
    } else {
      datatable(data.frame(Message = "Données non disponibles"), rownames = FALSE)
    }
  })
  
  output$bsl3_chart <- renderPlotly({
    data <- lab_data()
    
    if("BSL3" %in% names(data)) {
      bsl3_summary <- data %>%
        count(BSL3) %>%
        mutate(Label = ifelse(BSL3, "BSL-3", "Autres niveaux"))
      
      plot_ly(bsl3_summary, labels = ~Label, values = ~n, type = "pie") %>%
        layout(title = "Répartition des Laboratoires BSL-3")
    } else {
      plot_ly() %>%
        add_annotations(
          text = "Données BSL-3 non disponibles",
          x = 0.5, y = 0.5, xref = "paper", yref = "paper",
          showarrow = FALSE
        )
    }
  })
  
  output$genomic_chart <- renderPlotly({
    data <- lab_data()
    
    if("Surveillance_génomique" %in% names(data)) {
      genomic_summary <- data %>%
        count(Surveillance_génomique) %>%
        mutate(Label = ifelse(Surveillance_génomique, "Avec surveillance", "Sans surveillance"))
      
      plot_ly(genomic_summary, labels = ~Label, values = ~n, type = "pie") %>%
        layout(title = "Surveillance Génomique")
    } else {
      plot_ly() %>%
        add_annotations(
          text = "Données non disponibles",
          x = 0.5, y = 0.5, xref = "paper", yref = "paper",
          showarrow = FALSE
        )
    }
  })
  
  output$country_plot <- renderPlotly({
    data <- lab_data()
    
    if("Pays" %in% names(data)) {
      country_summary <- data %>%
        count(Pays) %>%
        arrange(desc(n))
      
      plot_ly(country_summary, x = ~Pays, y = ~n, type = "bar") %>%
        layout(
          title = "Répartition par Pays",
          xaxis = list(title = "Pays", tickangle = 45),
          yaxis = list(title = "Nombre de laboratoires")
        )
    } else {
      plot_ly() %>%
        add_annotations(
          text = "Données par pays non disponibles",
          x = 0.5, y = 0.5, xref = "paper", yref = "paper",
          showarrow = FALSE
        )
    }
  })
  
  output$top_cities_plot <- renderPlotly({
    data <- lab_data()
    
    if("Ville_District" %in% names(data)) {
      location_summary <- data %>%
        count(Ville_District) %>%
        arrange(desc(n)) %>%
        head(10)
      
      plot_ly(location_summary, x = ~n, y = ~reorder(Ville_District, n), type = "bar",
              orientation = "h") %>%
        layout(
          title = "Top Villes/Districts",
          xaxis = list(title = "Nombre de laboratoires"),
          yaxis = list(title = "Ville/District")
        )
    } else {
      plot_ly() %>%
        add_annotations(
          text = "Données de localisation non disponibles",
          x = 0.5, y = 0.5, xref = "paper", yref = "paper",
          showarrow = FALSE
        )
    }
  })
  
  output$location_plot <- renderPlotly({
    data <- lab_data()
    
    if("Ville_District" %in% names(data)) {
      location_summary <- data %>%
        count(Ville_District) %>%
        arrange(desc(n)) %>%
        head(10)
      
      plot_ly(location_summary, x = ~Ville_District, y = ~n, type = "bar") %>%
        layout(
          title = "Top Localisations",
          xaxis = list(title = "Ville/District", tickangle = 45),
          yaxis = list(title = "Nombre de laboratoires")
        )
    } else {
      plot_ly() %>%
        add_annotations(
          text = "Données de localisation non disponibles",
          x = 0.5, y = 0.5, xref = "paper", yref = "paper",
          showarrow = FALSE
        )
    }
  })
  
  output$timeline_plot <- renderPlotly({
    data <- lab_data()
    
    if("Date_de_l_enquête" %in% names(data)) {
      timeline_data <- data %>%
        mutate(Mois = format(Date_de_l_enquête, "%Y-%m")) %>%
        count(Mois) %>%
        arrange(Mois)
      
      if(nrow(timeline_data) > 0) {
        plot_ly(timeline_data, x = ~Mois, y = ~n, type = "scatter", mode = "lines+markers") %>%
          layout(
            title = "Évolution des Enquêtes par Mois",
            xaxis = list(title = "Mois"),
            yaxis = list(title = "Nombre d'enquêtes")
          )
      } else {
        plot_ly() %>%
          add_annotations(
            text = "Données temporelles non disponibles",
            x = 0.5, y = 0.5, xref = "paper", yref = "paper",
            showarrow = FALSE
          )
      }
    } else {
      plot_ly() %>%
        add_annotations(
          text = "Données temporelles non disponibles",
          x = 0.5, y = 0.5, xref = "paper", yref = "paper",
          showarrow = FALSE
        )
    }
  })
  
  output$raw_data_table <- renderDT({
    data <- lab_data()
    
    display_cols <- c("Nom_du_Laboratoire", "Ville_District", "Type_d_établissement", 
                      "Niveau_complexité", "Date_de_l_enquête", "Pays")
    
    # Ajouter les colonnes optionnelles si elles existent
    optional_cols <- c("Tests_VIH", "BSL3", "Surveillance_génomique")
    display_cols <- c(display_cols, optional_cols[optional_cols %in% names(data)])
    
    display_data <- data %>% select(any_of(display_cols))
    
    datatable(
      display_data,
      options = list(
        scrollX = TRUE,
        scrollY = "500px",
        pageLength = 10
      ),
      rownames = FALSE
    )
  })
}

# === LANCEMENT ===
shinyApp(ui, server)