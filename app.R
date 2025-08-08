# app.R
#source("dev_functions.R")

if (!require("BioSIM", character.only = TRUE)) {
  if (!require("remotes", character.only = TRUE)) {
    install.packages("remotes")
    library(remotes)
  }
  remotes::install_github("RNCan/BioSimClient_R")
  library("BioSIM", character.only = TRUE)
}



library(shiny)
library(DT)
library(Artemis2014)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(BioSIM)
library(ExtractMap)
library(plotly)
library(Billonage)
library(sf)
library(OutilsDRF)
library(data.table)

options(shiny.maxRequestSize = 500 * 1024^2)



# Interface utilisateur
ui <- dashboardPage(
  skin = "blue",

  # Entête
  dashboardHeader(
    title = "Artemis-2014",
    titleWidth = 250
  ),

  # Menu latéral
  dashboardSidebar(

      sidebarMenu(
      id = "sidebarMenu",
      menuItem("Données", tabName = "data", icon = icon("table")),


      menuItem("À propos", tabName = "about", icon = icon("info-circle")),
      uiOutput("menu_resultats")
    )
 ),



  dashboardBody(
      tags$head(
      tags$style(HTML("
      .box {border-radius: 5px;}
      .small-box {border-radius: 5px;}
      .btn {border-radius: 3px;}
      .progress {height: 10px; margin-bottom: 15px;}
      .content-wrapper {background-color: #f8f9fa;}
      .nav-tabs-custom {box-shadow: none;}

     .reset-button {
  position: absolute;
  top: 10px;
  right: 10px;
  z-index: 1000;
  background-color: #dc3545;
  color: white;
  border: none;
  border-radius: 3px;
  padding: 6px 12px;
  cursor: pointer;
  transition: background-color 0.3s;
}
.reset-button:hover {
  background-color: #c82333;
}





      /* Amélioration de l'apparence des inputs file */
      .form-control-file {
        position: relative;
        overflow: visible;
        margin-bottom: 30px; /* Espace pour le message */
      }

      /* Style de la barre de progression */
      .progress {
        margin-top: 5px;
        position: relative;
        height: 20px !important; /* Hauteur augmentée */
        clear: both;
        overflow: visible !important; /* Permettre au texte de déborder */
      }

      /* Style du message \"Upload complete\" */
      .progress-bar {
        position: relative;
        height: 20px;
        line-height: 20px;
      }

      /* Message après la barre */
      .progress-bar::after {
        content: attr(aria-valuenow);
        position: absolute;
        right: 0;
        bottom: -24px; /* Positionnement en dessous de la barre */
        color: #4D90D6;
        font-weight: bold;
        white-space: nowrap;
      }

      /* Pour le message \"Upload complete\" */
      .progress-bar[aria-valuenow=\"100%\"]::after {
        content: \"Upload complete\";
      }

      /* Style pour le conteneur du fileInput */
      .shiny-input-container {
        margin-bottom: 25px;
      }
    ")),
      tags$script(HTML("
      $(document).ready(function() {
        $('body').addClass('sidebar-collapse');
      });
    "))
    ),




    tabItems(

      tabItem(
        tabName = "data",
        fluidRow(

          box(
            width = 4,
            title = "Importation de données",
            status = "primary",
            solidHeader = TRUE,


            uiOutput("file_input_ui"),


            uiOutput("validation_status"),


            uiOutput("error_box"),


            uiOutput("extraction_question"),

            uiOutput("extraction_button"),

            uiOutput("extraction_button_final"),

            uiOutput("simulation_message")
          ),

          box(
            width = 8,
            title = "Données importées",
            status = "primary",
            solidHeader = TRUE,
            DTOutput("contents")
          )
        ),
        div(style = "position: relative; height: 40px;",
            actionButton("reset_button", "Réinitialiser", class = "reset-button", icon = icon("sync"))
        ),
      ),

      tabItem(
        tabName = "results",
        fluidRow(
          # Box pour les options de graphique
          box(
            width = 4,
            title = "Options de visualisation",
            status = "primary",
            solidHeader = TRUE,

            selectInput("espece", "Groupe d'espèces", choices = c("")),

            # Sélection de la variable
            selectInput("variable", "Choix de la variable",
                        choices = c("Surface terrière marchande (m2/ha)"="ST_HA", "Volume marchand (m3/ha)"="Vol_HA", "Diamètre quadratique moyen"="DMQ", "Densité (nb/ha)" = "nbTi_HA"),
                        selected = "ST_HA"),
            pickerInput(
              inputId = "placette",
              label = "Choix des placettes",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = list(
                `actions-box` = TRUE,
                `deselect-all-text` = "Tout supprimer",
                `select-all-text` = "Tout sélectionner",
                `none-selected-text` = "Rien de sélectionné"
              )
            ),


            uiOutput("simulation_info")
          ),




          box(
            width = 8,
            title = "Visualisation des résultats",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("resultat_graphique", height = "500px")
          )
        ),
        div(style = "position: relative; height: 40px;",
            actionButton("reset_button", "Réinitialiser", class = "reset-button", icon = icon("sync"))
        ),


        fluidRow(
          style = "margin-top: -25px;",
          box(
            width = 4,
            title = "Exportation des résultats",
            status = "primary",
            solidHeader = TRUE,

            radioButtons("simplifier", "Toutes les années de simulation",
                         choices = list("Oui" = FALSE, "Non" = TRUE),
                         selected = FALSE,
                         inline = TRUE),
            selectInput("Sortie",
                        label = "Choix de la sortie",
                        choices = c("-- Sélectionner une option --" = "",
                                    "Arbre" = "arbre",
                                    "Placette" = "placette",
                                    "À l'échelle du billon" = "echelle_billon"),
                        selected = ""),
            conditionalPanel(
              condition = "input.Sortie == 'echelle_billon'",
              selectInput("typeBillonnage", "Type de Billonnage Pétro:",
                          choices = list("DHP_Régionalisé" = "DHP", "DHP_Provincial" = "DHP2015")
              )
            ),

            conditionalPanel(
              condition = "input.Sortie == 'echelle_billon'",
              h5("Paramètres Sybille", style = "color: #856404; font-weight: bold; margin-top: 15px;"),
              # DHS parameter
              div(
                style = "margin-bottom: 10px;",
                numericInput("dhs_input",
                             label = "DHS (Diamètre à hauteur de souche):",
                             value = 0.15,
                             min = 0.01,
                             max = 1.0,
                             step = 0.01)
              ),
              # Grade 1 parameters
              div(
                style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
                h6("Grade 1", style = "color: #495057; font-weight: bold;"),
                textInput("nom_grade1", "Nom du grade 1:", value = "sciage court"),
                selectInput("long_grade1", "Longueur (pieds):",
                            choices = c("Indéfini", "4", "8", "12"),
                            selected = "8"),
                numericInput("diam_grade1", "Diamètre au fin bout(cm):",
                             value = 20, min = 0, max = 100, step = 0.1)
              ),
              # Grade 2 parameters
              div(
                style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
                h6("Grade 2", style = "color: #495057; font-weight: bold;"),
                textInput("nom_grade2", "Nom du grade 2:", value = "pate"),
                selectInput("long_grade2", "Longueur (pieds):",
                            choices = c("", "Indéfini", "4", "8", "12"),
                            selected = "4"),
                numericInput("diam_grade2", "Diamètre au fin bout(cm):",
                             value = 8, min = 0, max = 100, step = 0.1)
              ),
              # Grade 3 parameters
              div(
                style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
                h6("Grade 3", style = "color: #495057; font-weight: bold;"),
                textInput("nom_grade3", "Nom du grade 3:", value = ""),
                selectInput("long_grade3", "Longueur (pieds):",
                            choices = c("", "Indéfini", "4", "8", "12"),
                            selected = NULL),
                numericInput("diam_grade3", "Diamètre au fin bout(cm):",
                             value = NA, min = 0, max = 100, step = 0.1)
              ),
              div(
                style = "margin-top: 15px; text-align: center;",
                actionButton("calculer_billonnage",
                             "Calculer le billonnage",
                             style = "background-color: #28a745; color: white; width: 100%;",
                             icon = icon("calculator"))
              )
            ),
            div(
              style = "margin-top: 15px;",
              downloadButton("download_resultats_custom", "Télécharger les résultats",
                             style = "background-color: #4D90D6; color: white; width: 100%;")
            ),

            div(
              style = "margin-top: 10px; font-size: 0.9em; color: #6c757d; font-style: italic;",
              "Si \"Non\" est sélectionné, seuls les résultats de la première et de la dernière année de la simulation seront exportés."
            )
          )
        ),


      ),



      tabItem(
        tabName = "about",
        fluidRow(
          column(
            width = 10, offset = 1,
            div(
              class = "about-header text-center",
              style = "margin-bottom: 30px; border-bottom: 3px solid #4D90D6; padding-bottom: 15px;",
              h2("À Propos d'Artemis", style = "color: #4D90D6; font-weight: 700;"),
              p(class = "lead", "Simulateur de croissance", style = "font-style: italic; color: #6c757d;")
            )
          )
        ),

        fluidRow(
          column(
            width = 10, offset = 1,
            div(
              class = "about-content",
              style = "background-color: #fff; padding: 25px; border-radius: 8px; box-shadow: 0 2px 10px rgba(0,0,0,0.1);",

              # Section introduction
              div(
                class = "intro-section",
                style = "margin-bottom: 30px;",
                div(
                  class = "row",
                  div(
                    class = "col-md-3 text-center",
                    icon("tree", class = "fa-4x", style = "color: #4D90D6; margin-bottom: 15px;")
                  ),
                  div(
                    class = "col-md-9",
                    h3("Introduction", style = "color: #2c3e50; font-weight: 600; margin-bottom: 15px;"),
                    p("Bienvenue dans l'application Artemis, un Simulateur de croissance à l'échelle de l'arbre pour les forêts du Québec. Cette application vous permet de réaliser des simulations basées sur vos données d'inventaire forestier.", style = "font-size: 16px; line-height: 1.6;"),
                    p("", style = "font-size: 16px; line-height: 1.6;")
                  )
                )
              ),

              # Section documentation
              div(
                class = "documentation-section",
                style = "margin-bottom: 30px; background-color: #f8f9fa; padding: 20px; border-radius: 8px;",
                h3("Documentation", style = "color: #4D90D6; font-weight: 600; margin-bottom: 15px;"),
                p("Pour vous aider à utiliser efficacement Artemis, nous vous proposons un guide d'utilisation:", style = "font-size: 16px;"),
                div(
                  class = "text-center",
                  style = "margin: 20px 0;",
                  downloadButton(
                    "download_guide",
                    "Télécharger le guide d'utilisation",
                    icon = icon("file-pdf"),
                    style = "background-color: #4D90D6; color: white; padding: 10px 20px; font-size: 16px; border: none; border-radius: 4px;"
                  )
                ),
                p("Ce guide contient des instructions détaillées sur la préparation des données et la configuration des simulations.", style = "font-size: 15px; color: #6c757d; font-style: italic;")
              ),

              # Section fichiers d'exemple
              div(
                class = "examples-section",
                style = "margin-bottom: 30px;",
                h3("Fichiers d'exemple", style = "color: #2c3e50; font-weight: 600; margin-bottom: 15px;"),
                p("Pour vous familiariser avec la structure des fichiers d'Artemis, vous pouvez télécharger ces exemples:", style = "font-size: 16px;"),
                div(
                  class = "row",
                  style = "margin-top: 20px;",
                  div(
                    class = "col-md-6",
                    div(
                      class = "example-card",
                      style = "background-color: #f8f9fa; padding: 20px; border-radius: 8px; height: 100%; border-left: 4px solid #4D90D6;",
                      div(class = "text-center", style = "margin-bottom: 15px;", icon("leaf", class = "fa-2x", style = "color: #4D90D6;")),
                      h4("Données des arbres", style = "text-align: center; color: #2c3e50; margin-bottom: 15px;"),
                      p("Exemple de fichier CSV avec les données des arbres nécessaires pour la simulation.", style = "text-align: center; font-size: 15px;"),
                      div(
                        class = "text-center",
                        style = "margin-top: 15px;",
                        downloadButton(
                          "download_arbres",
                          "Télécharger",
                          style = "background-color: #4D90D6; color: white; border: none;"
                        )
                      )
                    )
                  ),
                  div(
                    class = "col-md-6",
                    div(
                      class = "example-card",
                      style = "background-color: #f8f9fa; padding: 20px; border-radius: 8px; height: 100%; border-left: 4px solid #4D90D6;",
                      div(class = "text-center", style = "margin-bottom: 15px;", icon("cloud-sun-rain", class = "fa-2x", style = "color: #4D90D6;")),
                      h4("Données climatiques", style = "text-align: center; color: #2c3e50; margin-bottom: 15px;"),
                      p("Exemples de fichiers CSV contenant les données climatiques pour les simulations.", style = "text-align: center; font-size: 15px;"),
                      div(
                        class = "row",
                        style = "margin-top: 20px;",
                        div(
                          class = "col-sm-6",
                          div(
                            class = "text-center mb-2",
                            downloadButton(
                              "download_climat_annuel",
                              "Climat annuel",
                              style = "background-color: #4D90D6; color: white; border: none; width: 100%;"
                            )
                          )
                        ),
                        div(
                          class = "col-sm-6",
                          div(
                            class = "text-center mb-2",
                            downloadButton(
                              "download_climat_mensuel",
                              "Climat mensuel",
                              style = "background-color: #4D90D6; color: white; border: none; width: 100%;"
                            )
                          )
                        )
                      )
                    )
                  )
                )
              ),




              # Section contact
              div(
                class = "contact-section",
                style = "background-color: #81B7F0; padding: 25px; border-radius: 8px;",
                h3("Contactez-nous", style = "color: #2c3e50; font-weight: 600; margin-bottom: 15px;"),
                div(
                  class = "row",
                  div(
                    class = "col-md-6",
                    div(
                      style = "background-color: white; padding: 15px; border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",
                      div(class = "text-center", icon("envelope", class = "fa-2x", style = "color: #4D90D6; margin-bottom: 10px;")),
                      h4("Email", style = "text-align: center; color: #2c3e50; margin-bottom: 10px;"),
                      p("recherche.forestiere@mrnf.gouv.qc.ca", style = "text-align: center; font-size: 16px; font-weight: 500;")
                    )
                  ),
                  div()
                )
              )
            )
          )
        ),

        # Pied de page
        fluidRow(
          column(
            width = 10, offset = 1,
            div(
              class = "footer",
              style = "margin-top: 30px; text-align: center; padding: 15px;",
              div(
                class = "footer-content",
                style = "border-top: 1px solid #e0e0e0; padding-top: 15px;",
                p("Artemis © 2025 ", style = "color: #78909c; font-size: 14px;"),
                p("Version 5.0.0", style = "color: #90a4ae; font-size: 12px;")
              )
            )
          )
        )
      )






    )
  )
)



# Serveur
server <- function(input, output, session) {

  rv <- reactiveValues(
    data_valid = FALSE,
    extraction_choice_made = FALSE,
    extraction_completed = FALSE,
    climat_annuel = NULL,
    climat_mensuel = NULL,
    extraction_option = NULL,
    extraction_horizon = NULL,
    age_moy_valid = TRUE,
    mode_visualisation = FALSE,
    resultats_simulation = NULL,
    placette = NULL,
    processed_Billonage = NULL,
    processed_Simul = NULL,
    listeEspece = NULL,
    simulation_terminee = FALSE

  )

  observe({
    query <- parseQueryString(session$clientData$url_search)

    if ("dev" %in% names(query) && file.exists("cached_simulation_results.rds")) {
      # Load cached simulation results
      rv$resultats_simulation <- readRDS("cached_simulation_results.rds")
      rv$simulation_terminee <- TRUE

      # Switch to results tab immediately
      updateTabItems(session, "sidebarMenu", "results")

      # Pre-select your working values for the export box
      updateRadioButtons(session, "simplifier", selected = FALSE)
      updateSelectInput(session, "Sortie", selected = "echelle_billon")
      updateSelectInput(session, "typeBillonnage", selected = "DHP")

      showNotification("DEV MODE: Loaded cached simulation results",
                       type = "message", duration = 3)
    }
  })

  output$file_input_ui <- renderUI({

    fileInput(paste0("file", ifelse(is.null(rv$fileInputId), "", rv$fileInputId)),
              "Choisir un fichier CSV", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
  })

  output$menu_resultats <- renderUI({
    if (rv$simulation_terminee) {

      menuItem("Résultats", tabName = "results", icon = icon("chart-line"))
    } else {
      NULL
    }
  })




  # Fonction réactive pour lire le fichier CSV
  data <- reactive({

    file_input <- input[[paste0("file", ifelse(is.null(rv$fileInputId), "", rv$fileInputId))]]

    req(file_input)

    # Réinitialiser les variables d'état lors du chargement d'un nouveau fichier
    rv$data_valid <- FALSE
    rv$extraction_choice_made <- FALSE
    rv$extraction_completed <- FALSE
    rv$climat_annuel <- NULL
    rv$climat_mensuel <- NULL
    rv$simulation_terminee <- FALSE

    # Vider les sorties précédentes
    output$extraction_question <- renderUI({})
    output$extraction_button <- renderUI({})
    output$simulation_message <- renderUI({})


    showNotification("Chargement des données en cours...", type = "message", duration = 3)


    df <- read.csv(file_input$datapath,
                   header = TRUE,
                   sep = ";",
                   quote = "",
                   encoding = "UTF-8")
    return(df)
  })



  # Fonction réactive pour valider les données
  validation_errors <- reactive({
    req(data())

    # Appliquer les deux fonctions de validation existantes
    erreurs1 <- valide_data(data(), "ORI", "ORI")
    erreurs2 <- trouver_noms_absents(data(), "ORI", "ORI")

    # Combiner toutes les erreurs
    all_errors <- c(erreurs1, erreurs2)


    rv$age_moy_valid <- valide_Age_moy(data(), "ORI", "ORI")

    rv$data_valid <- length(all_errors) == 0

    return(all_errors)
  })


  # Afficher le tableau de données
  output$contents <- renderDT({
    req(data())
    datatable(data(),
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
              ),
              rownames = FALSE,
              filter = 'top',
              class = 'cell-border stripe')
  })

  # Indicateur visuel de validation
  output$validation_status <- renderUI({
    req(validation_errors())
    errors <- validation_errors()

    result_div <- div(style = "margin-top: 15px; margin-bottom: 15px;")

    if (length(errors) > 0) {
      result_div <- tagAppendChild(result_div,
                                   infoBox(
                                     width = 12,
                                     title = "Statut",
                                     value = "Validation échouée",
                                     subtitle = paste(length(errors), "erreur(s) détectée(s)"),
                                     icon = icon("times-circle"),
                                     color = "red",
                                     fill = TRUE
                                   )
      )
    } else {
      result_div <- tagAppendChild(result_div,
                                   infoBox(
                                     width = 12,
                                     title = "Statut",
                                     value = "Validation réussie",
                                     subtitle = "Les données sont valides",
                                     icon = icon("check-circle"),
                                     color = "blue",
                                     fill = TRUE
                                   )
      )

      # Ajouter l'avertissement si l'âge moyen n'est pas valide
      if (!rv$age_moy_valid) {
        result_div <- tagAppendChild(result_div,
                                     div(
                                       style = "background-color: #fff3cd; color: #856404; padding: 15px; border: 1px solid #ffeeba; border-radius: 5px; margin-top: 10px;",
                                       icon("exclamation-triangle"),
                                       span(style = "font-weight: bold; margin-left: 5px;", "Attention:"),
                                       " La colonne Age_moy est manquante ou contient des erreurs. Vous ne pouvez pas utiliser les données climatiques dans votre simulation."
                                     )
        )
      }
    }

    return(result_div)
  })



  # Afficher les erreurs ou rien
  output$error_box <- renderUI({
    req(validation_errors())
    errors <- validation_errors()

    if (length(errors) > 0) {
      div(
        style = "background-color: #f8d7da; color: #721c24; padding: 15px; border: 1px solid #f5c6cb; border-radius: 5px; margin-top: 10px; max-height: 200px; overflow-y: auto;",
        h4("Erreurs détectées:"),
        tags$ul(
          lapply(errors, function(error) {
            tags$li(error)
          })
        )
      )
    }
  })


  # Modifier la question d'extraction pour inclure les trois options
  observe({
    # Si données valides et choix d'extraction pas encore fait
    if (rv$data_valid && !rv$extraction_choice_made && !rv$extraction_completed) {

      rv$placette <- unique(data()$PlacetteID)

      output$extraction_question <- renderUI({
        div(
          style = "margin-top: 20px; padding: 15px; background-color: #e8f4f8; border-radius: 5px; border: 1px solid #81B7F0;",
          h4("Concernant les données climatiques, que souhaitez-vous faire ?"),

          # Si l'âge moyen n'est pas valide, on désactive les deux premières options
          if (!rv$age_moy_valid) {
            tagList(
              radioButtons("extraction_choice", "",
                           choices = list(
                             "Simuler les données climatiques" = "extract",
                             "Fournir mes propres fichiers climatiques" = "upload",
                             "Ne pas utiliser de données climatiques" = "none"
                           ),
                           selected = "none"),
              tags$script(HTML("
              $(document).ready(function() {
                $('input[name=\"extraction_choice\"][value=\"extract\"]').prop('disabled', true);
                $('input[name=\"extraction_choice\"][value=\"upload\"]').prop('disabled', true);
              });
            ")),
              tags$div(
                style = "color: #d9534f; font-style: italic; font-size: 0.9em; margin-top: 5px; margin-bottom: 10px;",
                icon("exclamation-triangle"),
                "La colonne Age_moy est manquante ou contient des erreurs. Vous ne pouvez pas utiliser les données climatiques dans votre simulation."
              )
            )
          } else {
            radioButtons("extraction_choice", "",
                         choices = list(
                           "Simuler les données climatiques" = "extract",
                           "Fournir mes propres fichiers climatiques" = "upload",
                           "Ne pas utiliser de données climatiques" = "none"
                         ),
                         selected = character(0))
          },

          # Ajout du bouton Valider
          div(
            style = "margin-top: 15px; text-align: center;",
            actionButton("validate_extraction_choice", "Valider",
                         style = "background-color: #4D90D6; color: white; width: 100%;")
          )
        )
      })
    }
  })





  # Observer qui réagit au clic sur le bouton Valider - corrigé
  observeEvent(input$validate_extraction_choice, {
    # Vérifier si une option a été sélectionnée
    req(input$extraction_choice)

    if (!rv$age_moy_valid && input$extraction_choice != "none") {

      showNotification(
        "La colonne Age_moy est manquante ou contient des erreurs. Vous ne pouvez pas utiliser les données climatiques dans votre simulation.", type = "error",
        duration = 5
      )
      return()
    }



    rv$extraction_choice_made <- TRUE

    # Stocker explicitement le choix d'extraction dans la variable réactive
    rv$extraction_option <- input$extraction_choice

    # Faire disparaître la question d'extraction
    output$extraction_question <- renderUI({})

    if (input$extraction_choice == "extract") {
      # Afficher les paramètres de configuration d'extraction
      output$extraction_button <- renderUI({
        div(
          style = "margin-top: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
          h4("Configuration de l'extraction", style = "margin-top: 0;"),

          # Année de départ
          numericInput(
            "annee_depart",
            "Année de départ :",
            value = 2025,
            min = 2000,
            step = 1
          ),

          # Horizon
          numericInput(
            "horizon",
            "Nombre d'années de simulation (multiple de 10) :",
            value = 10,
            min = 10,
            step = 10
          ),

          # RCP
          radioButtons(
            "rcp",
            "Scénario RCP :",
            choices = list("RCP 4.5" = "RCP45", "RCP 8.5" = "RCP85"),
            selected = "RCP45"
          ),

          # Bouton d'extraction (apparaît seulement quand tous les paramètres sont définis)
          uiOutput("extraction_button_final")
        )
      })

      # Effacer le message de simulation
      output$simulation_message <- renderUI({})

    } else if (input$extraction_choice == "upload") {
      # Afficher les options pour téléverser ses propres fichiers climatiques
      output$extraction_button <- renderUI({
        div(
          style = "margin-top: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
          h4("Importer vos propres fichiers climatiques", style = "margin-top: 0;"),

          # File input pour le climat annuel
          fileInput("climat_annuel_file", "Fichier climat annuel (CSV)",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
          ),

          # File input pour le climat mensuel
          fileInput("climat_mensuel_file", "Fichier climat mensuel (CSV)",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
          ),

          # Bouton pour valider l'importation
          div(
            style = "margin-top: 15px;",
            actionBttn(
              "validate_climat_files",
              "Valider les fichiers climatiques",
              style = "gradient",
              color = "royal",
              icon = icon("check"),
              block = TRUE
            )
          )
        )
      })

      # Effacer le bouton d'extraction final
      output$extraction_button_final <- renderUI({})

      # Effacer le message de simulation pour le moment
      output$simulation_message <- renderUI({})

    } else if (input$extraction_choice == "none") {
      # Ne pas utiliser de données climatiques
      # Effacer le bouton d'extraction
      output$extraction_button <- renderUI({})
      output$extraction_button_final <- renderUI({})

      # Définir les variables climatiques comme NULL pour indiquer qu'elles ne sont pas utilisées
      rv$climat_annuel <- NULL
      rv$climat_mensuel <- NULL

      # Mettre à jour l'état indiquant que le processus est terminé
      rv$extraction_completed <- TRUE

      # Passer directement à la question de simulation
      output$simulation_message <- renderUI({
        div(
          style = "margin-top: 20px; padding: 15px; background-color: #81B7F0; color: #4D90D6; border-radius: 5px; text-align: center;",
          icon("info-circle"),
          span(style = "font-weight: bold; margin-left: 5px;", "Vous avez choisi de ne pas utiliser de données climatiques"),
          div(
            style = "margin-top: 15px; padding: 15px; background-color: #f8f9fa; border-radius: 5px; text-align: center;",
            h4("Souhaitez-vous effectuer une simulation?"),
            radioButtons("simulation_choice", "",
                         choices = list("Oui" = "yes", "Non" = "no"),
                         selected = character(0))
          )
        )
      })
    }
  })







  # Ajouter un nouvel observateur pour la validation des fichiers climatiques importés
  observeEvent(input$validate_climat_files, {
    # Vérifier que les deux fichiers ont été téléversés
    if (is.null(input$climat_annuel_file) || is.null(input$climat_mensuel_file)) {
      showNotification(
        "Veuillez téléverser les deux fichiers climatiques (annuel et mensuel).",
        type = "error",
        duration = 5
      )
      return()
    }

    # Lire les fichiers climatiques téléversés
    tryCatch({
      # Lire le fichier climat annuel
      climat_annuel <- read.csv(input$climat_annuel_file$datapath,
                                header = TRUE,
                                sep = ";",
                                quote = "",
                                encoding = "UTF-8")

      # Lire le fichier climat mensuel
      climat_mensuel <- read.csv(input$climat_mensuel_file$datapath,
                                 header = TRUE,
                                 sep = ";",
                                 quote = "",
                                 encoding = "UTF-8")

      # Vérifier les fichiers avec les fonctions du package Artemis
      erreurs_annuel <- verifier_colonnes_ClimAn(climat_annuel)
      erreurs_mensuel <- verifier_colonnes_Clim(climat_mensuel)

      # Vérifier s'il y a des erreurs
      if (length(erreurs_annuel) > 0 || length(erreurs_mensuel) > 0) {
        showModal(modalDialog(
          title = "Erreurs dans les fichiers climatiques",
          div(
            style = "max-height: 400px; overflow-y: auto;",

            # Section pour les erreurs du fichier climat annuel
            if (length(erreurs_annuel) > 0) {
              div(
                style = "background-color: #f8d7da; color: #721c24; padding: 15px; border: 1px solid #f5c6cb; border-radius: 5px; margin-bottom: 15px;",
                h4(paste0("Erreurs dans le fichier climat annuel (", input$climat_annuel_file$name, "):"),
                   style = "border-bottom: 1px solid #721c24; padding-bottom: 5px;"),
                tags$ul(
                  lapply(erreurs_annuel, function(error) {
                    tags$li(error)
                  })
                )
              )
            },

            # Section pour les erreurs du fichier climat mensuel
            if (length(erreurs_mensuel) > 0) {
              div(
                style = "background-color: #fff3cd; color: #856404; padding: 15px; border: 1px solid #ffeeba; border-radius: 5px;",
                h4(paste0("Erreurs dans le fichier climat mensuel (", input$climat_mensuel_file$name, "):"),
                   style = "border-bottom: 1px solid #856404; padding-bottom: 5px;"),
                tags$ul(
                  lapply(erreurs_mensuel, function(error) {
                    tags$li(error)
                  })
                )
              )
            }
          ),
          footer = tagList(
            div(
              style = "text-align: center; width: 100%;",
              p("Veuillez corriger les erreurs et réimporter les fichiers.",
                style = "font-style: italic; margin-bottom: 10px;"),
              modalButton("Fermer")
            )
          ),
          size = "l",
          easyClose = TRUE
        ))
        return()
      } else {
        # Si aucune erreur, stocker les données dans les variables réactives
        rv$climat_annuel <- climat_annuel
        rv$climat_mensuel <- climat_mensuel

        # Afficher une notification de succès
        showNotification(
          "Fichiers climatiques validés et importés avec succès !",
          type = "message",
          duration = 5
        )

        # Mettre à jour l'état
        rv$extraction_completed <- TRUE

        # Afficher le message pour passer à la simulation
        output$simulation_message <- renderUI({
          div(
            style = "margin-top: 20px; padding: 15px; background-color: #81B7F0; color: #4D90D6; border-radius: 5px; text-align: center;",
            icon("check-circle"),
            span(style = "font-weight: bold; margin-left: 5px;", "Fichiers climatiques importés avec succès"),
            div(
              style = "margin-top: 15px; padding: 15px; background-color: #f8f9fa; border-radius: 5px; text-align: center;",
              h4("Souhaitez-vous effectuer une simulation?"),
              radioButtons("simulation_choice", "",
                           choices = list("Oui" = "yes", "Non" = "no"),
                           selected = character(0))
            )
          )
        })
      }
    }, error = function(e) {
      # Afficher une notification d'erreur
      showNotification(
        paste("Erreur lors de l'importation des fichiers climatiques:", e$message),
        type = "error",
        duration = 10
      )
    })
  })















  # Action pour l'extraction climatique
  observeEvent(input$extract_climate, {
    # Vérifier les paramètres
    req(input$annee_depart, input$horizon, input$rcp)

    # S'assurer que l'horizon est d'au moins 2
    if (input$horizon < 2) {
      showNotification(
        "L'horizon doit être d'au moins 2 ans.",
        type = "error",
        duration = 5
      )
      return()
    }

    # Récupérer les paramètres pour le résumé
    annee_depart <- input$annee_depart
    horizon <- input$horizon
    annee_fin <- annee_depart + horizon
    rcp <- input$rcp

    showModal(modalDialog(
      title = "Extraction en cours",
      div(
        style = "text-align: center;",
        img(src = "https://i.gifer.com/origin/b4/b4d657e7ef262b88eb5f7ac021edda87.gif",
            height = "100px",
            style = "margin-bottom: 20px;"),
        p("Extraction des données climatiques en cours..."),
        p(style = "font-size: 0.9em; color: #6c757d;",
          paste0("Paramètres: Année de départ = ", annee_depart,
                 ", Horizon = ", horizon, " ans (jusqu'à ", annee_fin,
                 "), Scénario = ", rcp))
      ),
      footer = NULL,
      easyClose = FALSE
    ))

    # Appeler la fonction GenereClimat
    result <- tryCatch({
      GenereClimat(Data_Ori= data() ,AnneeDep = annee_depart,AnneeFin = annee_fin,  RCP = rcp)
    }, error = function(e) {
      showNotification(paste("Erreur lors de l'extraction:", e$message), type = "error", duration = 10)
      return(NULL)
    })

    # Stocker les résultats dans les variables réactives
    if (!is.null(result) && length(result) == 2) {
      rv$climat_annuel <- result[[1]]
      rv$climat_mensuel <- result[[2]]
      rv$extraction_horizon <- horizon/10  # Stocker l'horizon utilisé pour l'extraction
    }



    # Fermer la boîte de dialogue
    removeModal()

    # Afficher un résultat d'extraction avec les paramètres utilisés
    showModal(modalDialog(
      title = "Extraction terminée",
      div(
        style = "text-align: center;",
        icon("check-circle", class = "fa-3x", style = "color: #4D90D6; margin-bottom: 15px;"),
        h4("Les données climatiques ont été extraites avec succès !"),
        p("Vous pouvez télécharger les fichiers ci-dessous :"),
        div(
          style = "margin-top: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px; text-align: left;",
          h5("Paramètres utilisés :"),
          tags$ul(
            tags$li(paste0("Année de départ : ", input$annee_depart)),
            tags$li(paste0("Horizon : ", input$horizon)),
            tags$li(paste0("Scénario climatique : ", input$rcp))
          )
        ),
        div(
          style = "margin-top: 20px; display: flex; justify-content: space-around;",
          downloadButton("download_annuel", "Télécharger climat annuel",
                         style = "background-color: #4D90D6; color: white;"),
          downloadButton("download_mensuel", "Télécharger climat mensuel",
                         style = "background-color: #17a2b8; color: white;")
        )
      ),
      footer = actionButton("close_extraction", "Fermer",
                            style = "background-color: #007bff; color: white;"),
      easyClose = FALSE,      # Changement ici: passer à FALSE
      backdrop = "static"     # Ajout: empêche la fermeture en cliquant sur l'arrière-plan
    ))



  })


  output$download_annuel <- downloadHandler(
    filename = function() {
      paste("climat_annuel_", input$annee_depart, "_", input$annee_depart + input$horizon - 1, "_", input$rcp, ".csv", sep = "")
    },
    content = function(file) {

      write.table(rv$climat_annuel, file, sep = ";", row.names = FALSE)
    }
  )

  output$download_mensuel <- downloadHandler(
    filename = function() {
      paste("climat_mensuel_", input$annee_depart, "_", input$annee_depart + input$horizon - 1, "_", input$rcp, ".csv", sep = "")
    },
    content = function(file) {
      write.table(rv$climat_mensuel, file, sep = ";", row.names = FALSE)
    }
  )

  # Rendre le bouton d'extraction final une fois que tous les paramètres sont définis
  output$extraction_button_final <- renderUI({
    req(input$annee_depart, input$horizon, input$rcp)

    # Vérifier que l'horizon est au moins de 2
    if (input$horizon < 2) {
      div(
        style = "color: #dc3545; margin-top: 15px;",
        icon("exclamation-triangle"),
        "L'horizon doit être d'au moins 2 ans."
      )
    } else {
      # Tout est valide, afficher le bouton
      div(
        style = "margin-top: 15px;",
        actionBttn(
          "extract_climate",
          "Simuler les données climatiques",
          style = "gradient",
          color = "royal",
          icon = icon("cloud-download-alt"),
          block = TRUE
        ),
        p(style = "margin-top: 8px; font-size: 0.85em; color: #666; text-align: center;",
          paste0("Période: ", input$annee_depart, " - ", input$annee_depart + input$horizon - 1,
                 " | Scénario: ", ifelse(input$rcp == "RCP45", "RCP 4.5", "RCP 8.5"))
        )
      )
    }
  })

  # Fermer la boîte de dialogue d'extraction
  observeEvent(input$close_extraction, {
    removeModal()

    # Effacer les paramètres et le bouton d'extraction
    output$extraction_button <- renderUI({})
    output$extraction_button_final <- renderUI({})

    # Demander à l'utilisateur s'il souhaite effectuer une simulation
    output$simulation_message <- renderUI({
      div(
        style = "margin-top: 20px; padding: 15px; background-color: #81B7F0; color: #4D90D6; border-radius: 5px; text-align: center;",
        icon("check-circle"),
        span(style = "font-weight: bold; margin-left: 5px;", "Extraction terminée avec succès"),
        div(
          style = "margin-top: 15px; padding: 15px; background-color: #f8f9fa; border-radius: 5px; text-align: center;",
          h4("Souhaitez-vous effectuer une simulation?"),
          radioButtons("simulation_choice", "",
                       choices = list("Oui" = "yes", "Non" = "no"),
                       selected = character(0))
        )
      )
    })

    # Mettre à jour l'état
    rv$extraction_completed <- TRUE
  })




  # Observateur pour le choix de simulation - avec désactivation des options supplémentaires
  observeEvent(input$simulation_choice, {
    if (input$simulation_choice == "yes") {
      # Rediriger vers le panel de simulation avec les nouvelles options
      output$simulation_message <- renderUI({
        # Variable pour savoir si l'option "none" a été choisie
        no_climate_data <- !is.null(rv$extraction_option) && rv$extraction_option == "none"

        extracted_climate_data <- !is.null(rv$extraction_option) && rv$extraction_option == "extract"


        div(
          style = "margin-top: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
          h4("Configuration de la simulation", style = "margin-top: 0;"),

          # Paramètres de recrutement ajustés
          div(
            style = "margin-top: 15px;",
            h5("Paramètres de recrutement ajustés"),
            radioButtons("recrutement_ajuste", "",
                         choices = list("Non" = "non", "Oui" = "oui"),
                         selected = "non")
          ),

          # Coupe partielle
          div(
            style = "margin-top: 15px;",
            h5("Coupe partielle réalisée depuis moins de 10 ans"),
            radioButtons("coupe_partielle", "",
                         choices = list("Non" = "non", "Oui" = "oui"),
                         selected = "non")
          ),

          # Module d'accroissement - avec désactivation des options avancées si pas de données climatiques
          div(
            style = "margin-top: 15px;",
            h5("Module d'accroissement"),
            if (no_climate_data) {
              # Si pas de données climatiques, on désactive les options avancées
              tags$div(
                radioButtons("module_accroissement", "",
                             choices = list(
                               "Original" = "original",
                               "BRT" = "brt",
                               "GAM" = "gam"),
                             selected = "original"),
                tags$script(HTML(paste0("
                $(document).ready(function() {
                  $('input[name=\"module_accroissement\"][value=\"brt\"]').prop('disabled', true);
                  $('input[name=\"module_accroissement\"][value=\"gam\"]').prop('disabled', true);
                });
              "))),
                tags$div(
                  style = "color: #6c757d; font-style: italic; font-size: 0.9em; margin-top: 5px;",
                  "Les options avancées (BRT, GAM) sont désactivées car vous avez choisi de ne pas utiliser de données climatiques"
                )
              )
            } else {
              # Options normales si données climatiques disponibles
              radioButtons("module_accroissement", "",
                           choices = list(
                             "Original" = "original",
                             "BRT" = "brt",
                             "GAM" = "gam"),
                           selected = "original")
            }
          ),

          # Module de mortalité - avec désactivation de l'option QUE si pas de données climatiques
          div(
            style = "margin-top: 15px;",
            h5("Module de mortalité"),
            if (no_climate_data) {
              # Si pas de données climatiques, on désactive l'option QUE
              tags$div(
                radioButtons("module_mortalite", "",
                             choices = list(
                               "Original" = "original",
                               "QUE" = "que"),
                             selected = "original"),
                tags$script(HTML("
                $(document).ready(function() {
                  $('input[name=\"module_mortalite\"][value=\"que\"]').prop('disabled', true);
                });
              ")),
                tags$div(
                  style = "color: #6c757d; font-style: italic; font-size: 0.9em; margin-top: 5px;",
                  "L'option QUE est désactivée car vous avez choisi de ne pas utiliser de données climatiques"
                )
              )
            } else {
              # Options normales si données climatiques disponibles
              radioButtons("module_mortalite", "",
                           choices = list(
                             "Original" = "original",
                             "QUE" = "que"),
                           selected = "original")
            }
          ),

          # Nombre d'années de simulation
          div(
            style = "margin-top: 15px;",
            h5("Nombre d'années de simulation (multiple de 10)"),
            if (extracted_climate_data && !is.null(rv$extraction_horizon)) {
              # Si données extraites, afficher un champ désactivé avec l'horizon * 10
              div(
                numericInput("annees_simulation", "",
                             value = rv$extraction_horizon * 10,
                             min = 10,
                             step = 10),
                tags$script(HTML("
                $(document).ready(function() {
                  $('#annees_simulation').prop('disabled', true);
                });
              ")),
                tags$div(
                  style = "color: #6c757d; font-style: italic; font-size: 0.9em; margin-top: 5px;",
                  "Ce champ est automatiquement défini selon l'horizon d'extraction climatique"
                )
              )
            } else {
              # Champ normal si données climatiques importées ou non utilisées
              numericInput("annees_simulation", "",
                           value = 10,
                           min = 10,
                           step = 10)
            }
          )

          ,

          # Évolution du climat - désactivée si pas de données climatiques
          div(
            style = "margin-top: 15px;",
            h5("Évolution du climat"),
            if (no_climate_data) {
              # Option désactivée avec message d'information
              tags$div(
                radioButtons("evolution_climat", "",
                             choices = list("Oui" = "yes", "Non" = "no"),
                             selected = "no"),
                tags$script(HTML("
                $(document).ready(function() {
                  $('input[name=\"evolution_climat\"]').prop('disabled', true);
                });
              ")),
                tags$div(
                  style = "color: #6c757d; font-style: italic; font-size: 0.9em; margin-top: 5px;",
                  "Option désactivée car vous avez choisi de ne pas utiliser de données climatiques"
                )
              )
            } else {
              # Options normales
              radioButtons("evolution_climat", "",
                           choices = list("Oui" = "yes", "Non" = "no"),
                           selected = "yes")
            }
          ),

          # Bouton pour lancer la simulation
          div(
            style = "margin-top: 20px;",
            actionBttn(
              "lancer_simulation",
              "Lancer la simulation",
              style = "gradient",
              color = "primary",
              icon = icon("play-circle"),
              block = TRUE
            )
          )
        )
      })

    } else if (input$simulation_choice == "no") {
      # Afficher un message de fin
      output$simulation_message <- renderUI({
        div(
          style = "margin-top: 20px; padding: 15px; background-color: #81B7F0; color: #4D90D6; border-radius: 5px; text-align: center;",
          icon("check-circle"),
          span(style = "font-weight: bold; margin-left: 5px;", "Processus terminé")
        )
      })
    }
  })






  # Ajout d'un observateur pour l'action de lancer la simulation - avec restrictions des options
  observeEvent(input$lancer_simulation, {
    # Vérifier que tous les paramètres sont sélectionnés
    if (is.null(input$recrutement_ajuste) || is.null(input$coupe_partielle) ||
        is.null(input$module_accroissement) || is.null(input$module_mortalite) ||
        is.null(input$annees_simulation)) {

      showNotification(
        "Veuillez sélectionner tous les paramètres avant de lancer la simulation.",
        type = "error",
        duration = 5
      )
      return()
    }

    # Vérifier que le nombre d'années est un multiple de 10
    if (input$annees_simulation %% 10 != 0) {
      showNotification(
        "Le nombre d'années de simulation doit être un multiple de 10.",
        type = "error",
        duration = 5
      )
      return()
    }

    # Si données climatiques sont requises mais pas disponibles (pas pour option "none")
    if (!is.null(rv$extraction_option) && rv$extraction_option != "none" &&
        (is.null(rv$climat_annuel) || is.null(rv$climat_mensuel))) {
      showNotification(
        "Les données climatiques sont nécessaires pour lancer la simulation.",
        type = "error",
        duration = 5
      )
      return()
    }

    # Variable pour savoir si l'option "none" a été choisie (pas de données climatiques)
    no_climate_data <- !is.null(rv$extraction_option) && rv$extraction_option == "none"

    # Vérification supplémentaire pour les options incompatibles avec l'absence de données climatiques
    if (no_climate_data) {
      if (input$module_accroissement == "brt" || input$module_accroissement == "gam") {
        showNotification(
          "Les modules d'accroissement BRT et GAM nécessitent des données climatiques.",
          type = "error",
          duration = 5
        )
        return()
      }

      if (input$module_mortalite == "que") {
        showNotification(
          "Le module de mortalité QUE nécessite des données climatiques.",
          type = "error",
          duration = 5
        )
        return()
      }

      if (input$evolution_climat == "yes") {
        showNotification(
          "L'évolution du climat nécessite des données climatiques.",
          type = "error",
          duration = 5
        )
        return()
      }
    }

    # Afficher un message de traitement
    showModal(modalDialog(
      title = "Simulation en cours",
      div(
        style = "text-align: center;",
        img(src = "https://i.gifer.com/origin/b4/b4d657e7ef262b88eb5f7ac021edda87.gif",
            height = "100px",
            style = "margin-bottom: 20px;"),
        p("Simulation en cours..."),
        p(style = "font-size: 0.9em; color: #6c757d;",
          "Cela peut prendre plusieurs minutes. Veuillez patienter.")
      ),
      footer = NULL,
      easyClose = FALSE,
      backdrop = "static"
    ))

    # Conversion des choix d'interface en paramètres pour la fonction
    Tendance <- ifelse(input$recrutement_ajuste == "oui", 1, 0)
    Residuel <- ifelse(input$coupe_partielle == "oui", 1, 0)

    if (!is.null(rv$extraction_option) && rv$extraction_option == "extract" && !is.null(rv$extraction_horizon)) {
      Horizon <- rv$extraction_horizon
    } else {
      # Sinon, utilisez le nombre d'années divisé par 10
      Horizon <- input$annees_simulation/10
    }



    # Si l'utilisateur a choisi "none" (pas de données climatiques), force EvolClim à 0
    # et force certains modules à "ORI"
    if (no_climate_data) {
      EvolClim <- 0
      AccModif <- "ORI"  # Forcer le module d'accroissement à Original
      MortModif <- "ORI"  # Forcer le module de mortalité à Original
    } else {
      EvolClim <- ifelse(input$evolution_climat == "yes", 1, 0)
      AccModif <- switch(input$module_accroissement,
                         "original" = "ORI",
                         "brt" = "BRT",
                         "gam" = "GAM")
      MortModif <- switch(input$module_mortalite,
                          "original" = "ORI",
                          "que" = "QUE")
    }

    # Déterminer le RCP à utiliser
    RCP_value <- ifelse(!is.null(input$rcp),
                        input$rcp,
                        "RCP45")  # Valeur par défaut

    # Exécuter la fonction simulateurArtemis dans un bloc tryCatch pour gérer les erreurs
    result <- tryCatch({
      # Appel à la fonction simulateurArtemis avec les paramètres appropriés
      simulateurArtemis(
        Data_ori = data(),
        Horizon = Horizon,
        ClimMois = rv$climat_mensuel,
        ClimAn = rv$climat_annuel,
        Tendance = Tendance,
        Residuel = Residuel,
        EvolClim = EvolClim,
        AccModif = AccModif,
        MortModif = "ORI",
        RCP = RCP_value
      )
    }, error = function(e) {
      removeModal()
      showNotification(
        paste("Erreur lors de la simulation:", e$message),
        type = "error",
        duration = 10
      )
      return(NULL)
    })

    # Stocker le résultat dans une variable réactive pour le téléchargement
    rv$resultats_simulation <- result

    # Fermer la boîte de dialogue si l'opération a réussi
    if (!is.null(result)) {
      removeModal()

      # Définir les valeurs réelles utilisées pour les modules en cas d'absence de données climatiques
      module_acc_utilise <- if (no_climate_data) "Original (ORI)" else switch(input$module_accroissement,
                                                                              "original" = "Original (ORI)",
                                                                              "brt" = "BRT",
                                                                              "gam" = "GAM")

      module_mort_utilise <- if (no_climate_data) "Original (ORI)" else switch(input$module_mortalite,
                                                                               "original" = "Original (ORI)",
                                                                               "que" = "QUE")

      # Afficher un résultat de simulation
      showModal(modalDialog(
        title = "Simulation terminée",
        div(
          style = "text-align: center;",
          icon("check-circle", class = "fa-3x", style = "color: #4D90D6; margin-bottom: 15px;"),
          h4("La simulation a été effectuée avec succès !"),
          p("Vous pouvez télécharger les résultats ci-dessous :"),
          div(
            style = "margin-top: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px; text-align: left;",
            h5("Paramètres utilisés :"),
            tags$ul(
              tags$li(paste0("Paramètres de recrutement ajustés : ", input$recrutement_ajuste)),
              tags$li(paste0("Coupe partielle récente : ", input$coupe_partielle)),
              tags$li(paste0("Module d'accroissement : ", module_acc_utilise)),
              tags$li(paste0("Module de mortalité : ", module_mort_utilise)),
              tags$li(paste0("Nombre d'années : ", input$annees_simulation)),
              if (no_climate_data) {
                tags$li(paste0("Évolution du climat : Non (Données climatiques non utilisées)"))
              } else {
                tags$li(paste0("Évolution du climat : ", ifelse(input$evolution_climat == "yes", "Oui", "Non")))
              },
              if (!no_climate_data) {
                tags$li(paste0("Scénario RCP : ", RCP_value))
              }
            )
          ),
          div(
            # style = "margin-top: 20px;",
            # downloadButton("download_resultats", "Télécharger les résultats",
            #                style = "background-color: #28a745; color: white;")
          )
        ),
        footer = actionButton("close_simulation", "Fermer",
                              style = "background-color: #007bff; color: white;"),
        easyClose = FALSE,
        backdrop = "static"
      ))
    }
  })

  observe({
    if (!is.null((rv$resultats_simulation))) {
      listeEspece <- unique((rv$resultats_simulation)$GrEspece)
      listeEspece2 <- append("TOT", listeEspece)
      updateSelectInput(session = session,
                        inputId = "espece",
                        label = "Groupe d'espèces",
                        choices = listeEspece2)
    }
  })

  # Gestionnaire de téléchargement pour les résultats de simulation (ne garder que celui-ci)
  output$download_resultats <- downloadHandler(
    filename = function() {
      paste("resultats_simulation_", format(Sys.Date(), "%Y%m%d"), ".csv", sep = "")
    },
    content = function(file) {
      # Utiliser le dataframe résultant de la simulation
      if (!is.null(rv$resultats_simulation)) {
        write.table(rv$resultats_simulation, file, sep = ";", row.names = FALSE)
      } else {
        # Créer un fichier vide ou avec un message d'erreur si aucun résultat n'est disponible
        write.csv(data.frame(Erreur = "Aucun résultat de simulation disponible"), file, row.names = FALSE)
      }
    }
  )


  output$download_arbres<- downloadHandler(
    filename = function() {
      "Donnees_Exemple_Artemis.csv"
    },
    content = function(file) {
      file.copy("WWW/Donnees_Exemple.csv", file)
    }
  )

  output$download_climat_annuel<- downloadHandler(
    filename = function() {
      "Donnees_ClimAn_Exemple_Artemis.csv"
    },
    content = function(file) {
      file.copy("WWW/ClimAn_Exemple.csv", file)
    }
  )


  output$download_climat_mensuel<- downloadHandler(
    filename = function() {
      "Donnees_ClimMois_Exemple_Artemis.csv"
    },
    content = function(file) {
      file.copy("WWW/ClimMois_Exemple.csv", file)
    }
  )

  output$download_guide<- downloadHandler(
    filename = function() {
      "Guide d'utilisation Artemis.pdf"
    },
    content = function(file) {
      file.copy("WWW/Guide_Artemis.pdf", file)
    }
  )

  observeEvent(input$close_simulation, {
    removeModal()

    if (!is.null(rv$resultats_simulation)) {
      saveRDS(rv$resultats_simulation, "cached_simulation_results.rds")
      cat("✓ Simulation results saved for development\n")
    }

    rv$simulation_terminee <- TRUE
    updateTabItems(session, "sidebarMenu", "results")
  })


  observe({

    if (input$sidebarMenu == "results" && !rv$simulation_terminee) {

      updateTabItems(session, "sidebarMenu", "data")

      showNotification(
        "Veuillez d'abord effectuer une simulation pour accéder aux résultats.",
        type = "warning",
        duration = 5
      )
    }
  })



  observe({
    req(rv$resultats_simulation)

    # Extraire toutes les placettes uniques des résultats
    placettes <- unique(rv$resultats_simulation$PlacetteID)

    # Mettre à jour le sélecteur de placettes
    updatePickerInput(
      session,
      "placette",
      choices = placettes,
      selected = placettes
    )
  })



  observeEvent(input$calculer_billonnage, {
    # Vérifier SEULEMENT les paramètres de base requis pour Shiny
    req(input$dhs_input, input$typeBillonnage, rv$resultats_simulation)

    # Validation minimale : s'assurer qu'au moins le nom du Grade 1 n'est pas vide
    if(is.null(input$nom_grade1) || input$nom_grade1 == "") {
      showNotification("Le nom du Grade 1 est obligatoire",
                       type = "error", duration = 5)
      return()
    }

    # REMPLACER la notification par une barre de progression
    withProgress(message = 'Calcul du billonnage en cours...', value = 0, {

      incProgress(0.1, detail = "Validation des paramètres...")

      # Conversion des types (permettre NA pour tous les grades)
      dhs_val <- as.numeric(input$dhs_input)

      incProgress(0.2, detail = "Traitement des longueurs...")

      # Gestion des longueurs avec menu déroulant
      long_grade1_val <- if(is.null(input$long_grade1) || input$long_grade1 == "Indéfini") {
        NA_real_
      } else {
        as.numeric(input$long_grade1)
      }

      long_grade2_val <- if(is.null(input$long_grade2) || input$long_grade2 == "Indéfini") {
        NA_real_
      } else {
        as.numeric(input$long_grade2)
      }

      long_grade3_val <- if(is.null(input$long_grade3) || input$long_grade3 == "Indéfini") {
        NA_real_
      } else {
        as.numeric(input$long_grade3)
      }

      incProgress(0.3, detail = "Traitement des diamètres...")

      # Gestion des diamètres (maintenant numericInput avec min/max)
      diam_grade1_val <- if(is.null(input$diam_grade1) || is.na(input$diam_grade1)) {
        NA_real_
      } else {
        as.numeric(input$diam_grade1)
      }

      diam_grade2_val <- if(is.null(input$diam_grade2) || is.na(input$diam_grade2)) {
        NA_real_
      } else {
        as.numeric(input$diam_grade2)
      }

      diam_grade3_val <- if(is.null(input$diam_grade3) || is.na(input$diam_grade3)) {
        NA_real_
      } else {
        as.numeric(input$diam_grade3)
      }

      incProgress(0.4, detail = "Préparation des noms de grades...")

      # Gestion des noms (avec valeurs par défaut)
      nom_grade1_val <- as.character(input$nom_grade1) # Valeur par défaut "sciage court" définie dans l'UI
      nom_grade2_val <- if(is.null(input$nom_grade2) || input$nom_grade2 == "") {
        NA_character_
      } else {
        as.character(input$nom_grade2) # Valeur par défaut "pate" définie dans l'UI
      }
      nom_grade3_val <- if(is.null(input$nom_grade3) || input$nom_grade3 == "") {
        NA_character_
      } else {
        as.character(input$nom_grade3)
      }

      incProgress(0.5, detail = "Exécution du calcul de billonnage...")

      # Exécuter SortieBillesFusion - laisser la fonction faire sa propre validation
      tryCatch({
        rv$processed_Billonage <- SortieBillesFusion(
          Data = rv$resultats_simulation,
          Type = as.character(input$typeBillonnage),
          dhs = dhs_val,
          nom_grade1 = nom_grade1_val,
          long_grade1 = long_grade1_val,
          diam_grade1 = diam_grade1_val,
          nom_grade2 = nom_grade2_val,
          long_grade2 = long_grade2_val,
          diam_grade2 = diam_grade2_val,
          nom_grade3 = nom_grade3_val,
          long_grade3 = long_grade3_val,
          diam_grade3 = diam_grade3_val
        )

        incProgress(0.9, detail = "Finalisation...")

        # Mettre à jour processed_Simul immédiatement
        rv$processed_Simul <- rv$processed_Billonage

        incProgress(1, detail = "Terminé!")

        showNotification("Billonnage calculé avec succès!", type = "message", duration = 3)

      }, error = function(e) {
        cat("✗ Erreur billonnage:", e$message, "\n")
        # L'erreur de calcul_vol_bille sera affichée à l'utilisateur
        showNotification(paste("Erreur:", e$message), type = "error", duration = 5)
        rv$processed_Billonage <- NULL
        rv$processed_Simul <- NULL
      })

    }) # Fin du withProgress
  })

  observeEvent(c(input$Sortie, input$simplifier), {
    req(input$Sortie, rv$resultats_simulation)

    switch(input$Sortie,
           "arbre" = {
             rv$processed_Simul <- SortieArbre(SimulHtVol = rv$resultats_simulation,
                                               simplifier = input$simplifier)
           },
           "placette" = {
             rv$processed_Simul <- SortiePlacette(SimulHtVol = rv$resultats_simulation,
                                                  simplifier = input$simplifier)
           },
           "echelle_billon" = {
             # Attendre que processed_Billonage soit disponible
             if (!is.null(rv$processed_Billonage)) {
               rv$processed_Simul <- rv$processed_Billonage
             } else {
               # Si pas encore traité, déclencher une invalidation pour réessayer
               invalidateLater(100, session)
               return()
             }
           }
    )
  }, ignoreInit = TRUE)



  output$resultat_graphique <- renderPlot({
    req(rv$resultats_simulation)
    req(input$espece)
    req(input$variable)

    # S'assurer qu'il y a au moins une placette sélectionnée
    if (is.null(input$placette) || length(input$placette) == 0) {
      # Si aucune placette n'est sélectionnée, utiliser toutes les placettes
      placettes_to_use <- unique(rv$resultats_simulation$PlacetteID)
    } else {
      placettes_to_use <- input$placette
    }

    # Appel de la fonction Graph du package Artemis
    Graph(
      Data = rv$resultats_simulation,
      Espece = input$espece,
      Variable = input$variable,
      listePlacette = placettes_to_use
    )
  })

  # Information sur la simulation
  output$simulation_info <- renderUI({
    req(rv$resultats_simulation)

    # Variable pour savoir si l'option "none" a été choisie (pas de données climatiques)
    no_climate_data <- !is.null(rv$extraction_option) && rv$extraction_option == "none"

    div(
      style = "margin-top: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
      h5("Informations sur la simulation:"),
      tags$ul(
        tags$li(paste0("Recrutement ajusté: ", ifelse(input$recrutement_ajuste == "oui", "Oui", "Non"))),
        tags$li(paste0("Coupe partielle: ", ifelse(input$coupe_partielle == "oui", "Oui", "Non"))),
        if (no_climate_data) {
          tags$li("Données climatiques: Non utilisées")
        } else {
          tagList(
            tags$li("Données climatiques: Utilisées"),
            tags$li(paste0("Évolution climat: ", ifelse(input$evolution_climat == "yes", "Oui", "Non")))
          )
        },
        tags$li(paste0("Années de simulation: ", input$annees_simulation))
      )

    )
  })

  # Gestionnaire de téléchargement dans l'onglet Résultats


  output$download_resultats_viz <- downloadHandler(
    filename = function() {
      paste("resultats_simulation_",Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Utiliser le dataframe résultant de la simulation
      if (!is.null(rv$resultats_simulation)) {
        write.table(rv$resultats_simulation, file, sep = ";", row.names = FALSE)
      } else {
        # Créer un fichier vide ou avec un message d'erreur si aucun résultat n'est disponible
        write.csv(data.frame(Erreur = "Aucun résultat de simulation disponible"), file, row.names = FALSE)
      }
    }
  )



  output$download_resultats_custom <- downloadHandler(
    filename = function() {
      if(!(input$Sortie == "echelle_billon")){

        paste("resultats_simulation_artemis_sortie_",input$Sortie,"_",Sys.Date(), ".csv", sep = "")

      }
      else{
        paste("resultats_simulation_artemis_sortie_",input$Sortie,"_",input$typeBillonnage,"_",Sys.Date(), ".csv", sep = "")
      }
    },
    content = function(file) {
      # Utiliser le dataframe résultant de la simulation
      if (!is.null(rv$processed_Simul)) {
        write.table(rv$processed_Simul, file, sep = ";", row.names = FALSE)
      } else {
        # Créer un fichier vide ou avec un message d'erreur si aucun résultat n'est disponible
        write.csv(data.frame(Erreur = "Aucun résultat de simulation disponible"), file, row.names = FALSE)
      }
    }
  )




  observeEvent(input$close_simulation, {
    removeModal()

    # Afficher un message de fin
    output$simulation_message <- renderUI({
      div(
        style = "margin-top: 20px; padding: 15px; background-color: #81B7F0; color: #4D90D6; border-radius: 5px; text-align: center;",
        icon("check-circle"),
        span(style = "font-weight: bold; margin-left: 5px;", "Simulation terminée avec succès")
      )
    })
  })



  observeEvent(input$analyse_climat, {
    # Vérifier les paramètres
    req(input$annee_debut_analyse, input$annee_fin_analyse, input$type_analyse)

    # S'assurer que l'année de fin est postérieure à l'année de début
    if (input$annee_fin_analyse <= input$annee_debut_analyse) {
      showNotification(
        "L'année de fin doit être postérieure à l'année de début.",
        type = "error",
        duration = 5
      )
      return()
    }


    showModal(modalDialog(
      title = "Analyse en cours",
      div(
        style = "text-align: center;",
        img(src = "https://i.gifer.com/origin/b4/b4d657e7ef262b88eb5f7ac021edda87.gif",
            height = "100px",
            style = "margin-bottom: 20px;"),
        p("Analyse de l'évolution climatique en cours..."),
        p(style = "font-size: 0.9em; color: #6c757d;",
          paste0("Période: ", input$annee_debut_analyse, " - ", input$annee_fin_analyse,
                 ", Type: ", switch(input$type_analyse,
                                    "temp_moy" = "Température moyenne",
                                    "precip" = "Précipitations",
                                    "gel" = "Jours de gel",
                                    "canicule" = "Jours de canicule")))
      ),
      footer = NULL,
      easyClose = FALSE
    ))


    Sys.sleep(3)


    removeModal()

    # Afficher un résultat
    showModal(modalDialog(
      title = "Analyse terminée",
      div(
        style = "text-align: center;",
        icon("chart-line", class = "fa-3x", style = "color: #4D90D6; margin-bottom: 15px;"),
        h4("L'analyse climatique a été effectuée avec succès !"),
        p("Un rapport d'évolution climatique a été généré."),
        div(
          style = "margin-top: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px; text-align: left;",
          h5("Paramètres utilisés :"),
          tags$ul(
            tags$li(paste0("Période : ", input$annee_debut_analyse, " - ", input$annee_fin_analyse)),
            tags$li(paste0("Type d'analyse : ", switch(input$type_analyse,
                                                       "temp_moy" = "Température moyenne",
                                                       "precip" = "Précipitations",
                                                       "gel" = "Jours de gel",
                                                       "canicule" = "Jours de canicule")))
          )
        )
      ),
      footer = actionButton("close_analyse", "Fermer",
                            style = "background-color: #4D90D6; color: white;"),
      easyClose = TRUE
    ))
  })


  observeEvent(input$close_analyse, {
    removeModal()

    # Afficher le message de confirmation
    output$simulation_message <- renderUI({
      div(
        style = "margin-top: 20px; padding: 15px; background-color: #81B7F0; color: #4D90D6; border-radius: 5px; text-align: center;",
        icon("check-circle"),
        span(style = "font-weight: bold; margin-left: 5px;", "Analyse terminée avec succès")
      )
    })


    rv$extraction_completed <- TRUE
  })


  # pour gérer la réinitialisation
  observeEvent(input$reset_button, {
    # Afficher une boîte de dialogue de confirmation
    showModal(modalDialog(
      title = "Confirmation de réinitialisation",
      div(
        style = "text-align: center;",
        p("Êtes-vous sûr de vouloir réinitialiser l'application?"),
        p("Toutes les données et simulations actuelles seront perdues."),
        style = "color: #721c24; font-weight: bold;"
      ),
      footer = tagList(
        actionButton("confirm_reset", "Oui, réinitialiser",
                     style = "background-color: #dc3545; color: white;"),
        modalButton("Annuler")
      ),
      easyClose = TRUE
    ))
  })


  observeEvent(input$confirm_reset, {
    rv$simulation_terminee <- FALSE
    session$reload()
  })





}

# Lancer l'application
shinyApp(ui = ui, server = server)
