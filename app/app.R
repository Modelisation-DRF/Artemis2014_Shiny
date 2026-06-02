# app.R

# Chargement des library
library(shiny)
library(DT)
library(bslib)
library(Artemis2014)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(BioSIM)
library(ExtractMap)
library(plotly)
library(BillonnagePetro)
library(sf)
library(OutilsDRF)
library(data.table)
library(readxl)

# Permettre le chargement de gros fichiers
options(shiny.maxRequestSize = 500 * 1024^2)

# Detection Contexte
# Détecter si on est dans RStudio (interactif) ou lancé en batch (VBS)
# Cela permet de ne pas tuer la session R quand on est dans RStudio
is_rstudio <- Sys.getenv("RSTUDIO") == "1"
is_interactive_session <- interactive() && is_rstudio

# Extraction information sur version du package Artemis
version_artemis <- as.character(packageVersion("Artemis2014"))
desc <- packageDescription("Artemis2014")
annee <- format(as.Date(substr(desc$Packaged, 1, 10)), "%Y")

# Interface utilisateur
ui <- fluidPage(
  theme = bs_theme(version = 5),
  # Importation du .css ministériel
  tags$head(
    tags$link(rel = "stylesheet", href = "theme-gouvernemental.css"),
  ),

  # Header gouvernemental
  tags$header(
    div(
      class = "container-fluid piv py-2",
      role = "banner",
      #style = "font-family: var(--qc-font-family-header);",

      div(
        class = "d-flex align-items-center",
        tags$a(
          href = "https://www.quebec.ca/",
          target = "_blank",

          tags$img(
            src = "signature-PIV.svg",
            height = "50px"
          )
        ),
        p(
          "Artemis‑2014",
          class = "fs-3 text-white ms-3 header-title",
          style = "font-family: var(--qc-font-family-header);"
        )
      )
    )
  ),
  # Menu de navigation
    div(
      class = "headline pt-3",
      div(class = "container-fluid",

          div(class = "d-flex align-items-center",
              uiOutput("nav_menu")
          )
      )
    )
  ,

  # Contenu de la page
  div(
    class = "container-fluid mt-5",
    # Gérer coté server
    uiOutput("main_content")
  ),

  # Footer
  div(
    class = "container-fluid text-center py-3",

    # Info version Artemis- Au dessus de la ligne grise
    div(class = "d-flex justify-content-center gap-3 mb-1",
        p(paste("Artemis ©", annee), class = "text-muted small mb-0"),
        p(paste("Version", version_artemis), class = "text-muted small mb-0")
    ),

    # Ligne grise + reste du footer
    div(class = "border-top pt-2",

        div(class = "mt-0",
            tags$a(
              href = "https://www.quebec.ca/gouvernement/ministere/ressources-naturelles-forets",
              target = "_blank",
              tags$img(
                src = "MRNF_couleur.svg",
                height = "60px"
              )
            ),
            div(class = "mt-2",
                tags$a(
                  paste0("© Gouvernement du Québec, ", format(Sys.Date(), "%Y")),
                  href = "https://www.quebec.ca/droit-auteur",
                  target = "_blank",
                  class = "text-muted small"
                )
            )
        )
    )
  )

)



# Serveur
server <- function(input, output, session) {

  session$onSessionEnded(function() {
    # Nettoyage mémoire
    gc()
    message("Session fermée.")

    # Quitter R seulement si on n'est PAS dans RStudio (lancé via VBS)
    if (!is_interactive_session) {
      Sys.sleep(0.5)
      q(save = "no")
    }
  })

  # Active un mode dev pour charger des résultats simulés depuis un fichier
  observe({
    query <- parseQueryString(session$clientData$url_search)

    if ("dev" %in% names(query) && file.exists("cached_simulation_results.rds")) {
      # Load cached simulation results
      rv$resultats_simulation <- readRDS("cached_simulation_results.rds")
      rv$simulation_terminee <- TRUE

      # Switch to results tab immediately
      current_tab("resultats")

      # Pre-select your working values for the export box
      #updateRadioButtons(session, "simplifier", selected = FALSE)
      #updateSelectInput(session, "Sortie", selected = "echelle_billon")
      #updateSelectInput(session, "typeBillonnage", selected = "DHP")

      showNotification("DEV MODE: Loaded cached simulation results",
                       type = "message", duration = 3)
    }
  })

  # Page à l'ouverture (défaut)
  current_tab <- reactiveVal("donnees")

  # Activation de la page cliquée
  observeEvent(input$tab_accueil, {
    current_tab("accueil")
  })

  observeEvent(input$tab_donnees, {
    current_tab("donnees")
  })

  # Chargement dynamique du menu de navigation
  output$nav_menu <- renderUI({
    current <- current_tab()
    div(class = "d-flex ms-2 align-items-center",

        actionLink(
          "tab_accueil", "Accueil",
          class = "text-white px-2 fs-2",
          style = paste0(
            "text-decoration:none; padding-bottom:10px;",
            if (current == "accueil")
              "border-bottom:4px solid lightgray;" else ""
          )
        ),

        actionLink(
          "tab_donnees", "Données",
          class = "text-white px-2 fs-2",
          style = paste0(
            "text-decoration:none; padding-bottom:10px;",
            if (current == "donnees")
              "border-bottom:4px solid lightgray;" else ""
          )
        )
    )
  })

  # Chargement dynamique du contenu des pages
  output$main_content <- renderUI({
    # Page Accueil
    if (current_tab() == "accueil") {
      div(
        div(
          h1("Accueil"),
          p("Bienvenue dans l'application Artemis, un Simulateur de croissance à l'échelle de l'arbre pour les forêts du Québec. Cette application vous permet de réaliser des simulations basées sur vos données d'inventaire forestier.")
        ),

        div(
          class = "mt-4",
          h3("Documentation", class = "mb-3"),
            p("Pour vous aider à utiliser efficacement Artemis veuillez consulter la page Wiki de l'application: ",
            tags$a(
              "Aide application R Artémis",
              href = "https://github.com/Modelisation-DRF/Artemis2014_Shiny/wiki",
              target = "_blank",
              class = "fw-bold"
            )),

            p("Cette page contient des instructions détaillées sur la préparation des données et la configuration des simulations.",
              class = "text-muted fst-italic"
            )
          ),

        # Fichiers d'exemple
        tags$div(
          class = "mb-4",
          tags$h3("Fichiers d'exemple", class = "mb-3"),
          tags$p("Pour vous familiariser avec la structure des fichiers d'Artemis, vous pouvez télécharger ces exemples:"),

          tags$div(class = "row mt-4",

            # Données arbres
            tags$div(
              class = "col-md-6",
              tags$div(
                class = "bg-light p-4 rounded h-100 border-start border-primary border-4",
                tags$div(class = "text-center mb-3", icon("tree",class = "text-primary")),
                tags$h4("Données des arbres", class = "text-center mb-3"),
                tags$p("Exemple de fichier CSV avec les données des arbres nécessaires pour la simulation.", class = "text-center"),
                tags$div(
                  class = "text-center mt-3",
                  downloadButton("download_arbres", "Télécharger", class = "btn btn-primary")
                )
              )
            ),

            # Données climatiques
            tags$div(
              class = "col-md-6",
              tags$div(
                class = "bg-light p-4 rounded h-100 border-start border-primary border-4",
                tags$div(class = "text-center mb-3", icon("cloud-sun-rain", class = "text-primary")),
                tags$h4("Données climatiques", class = "text-center mb-3"),
                tags$p("Exemples de fichiers CSV contenant les données climatiques pour les simulations.", class = "text-center"),
                tags$div(
                  class = "d-flex justify-content-center gap-2 mt-3",
                  downloadButton("download_climat_annuel", "Climat annuel", class = "btn btn-primary"),
                  downloadButton("download_climat_mensuel", "Climat mensuel", class = "btn btn-primary")
                )
              )
            )
          )
        ),
        # Section contact
        div(
          class = "mt-4",
          h3(
            "Contactez-nous",
            class = "mb-3",
          ),
          p("Pour toute question ou demande d'information, vous pouvez nous contacter par courriel:",

            tags$a(
              "recherche.forestiere@mrnf.gouv.qc.ca",
              href = "mailto:recherche.forestiere@mrnf.gouv.qc.ca",
              class = "fw-bold"
            )
          )
        )
      )

    # Page Données
    } else if (current_tab() == "donnees") {

      div(class = "container-fluid px-0 mt-2",
        div(class = "row g-2",

            # Colonne de gauche (1/3)
            div(class = "col-md-4",

                # Importations des données
                div(class = "card mb-3",
                    div(
                      class = "card-header bg-secondary text-white d-flex justify-content-between align-items-center",
                      span("Importation de données"),

                      tags$a(
                        href = "#collapse_import",
                        `data-bs-toggle` = "collapse",
                        role = "button",
                        icon("chevron-down",class="text-white")
                      )
                    ),

                    div(
                      id = "collapse_import",
                      class = "collapse show",
                      div(class = "card-body",
                          p("Choisir un fichier CSV"),
                          fileInput("file", NULL, buttonLabel = "Parcourir", placeholder = " - ")
                      ),
                      uiOutput("validation_status"),
                      uiOutput("error_box"),
                      uiOutput("Avertissement_box"),
                      uiOutput("Info_box"),
                      uiOutput("extraction_question")
                    )

                ),

                # Configuration
                div(class = "card",
                    div(
                      class = "card-header bg-secondary text-white d-flex justify-content-between align-items-center",
                      span("Configuration de la simulation"),
                      tags$a(
                        href = "#collapse_config",
                        `data-bs-toggle` = "collapse",
                        role = "button",
                        icon("chevron-down",class="text-white")
                      )
                    ),

                    div(
                      id = "collapse_config",
                      class = "collapse show",

                      div(class = "card-body",
                          p("Paramètres à venir...")
                      )
                    )
                )
            ),

            # colonne de droite (2/3)
            div(class = "col-md-8",
                # Données importées
                div(class = "card",
                    div(
                      class = "card-header bg-secondary text-white d-flex justify-content-between align-items-center",
                      span("Données importées"),
                      tags$a(
                        href = "#collapse_data",
                        `data-bs-toggle` = "collapse",
                        role = "button",
                        icon("chevron-down",class="text-white")
                      )
                    ),

                    div(
                      id = "collapse_data",
                      class = "collapse show",

                      div(class = "card-body py-0 fs-5",
                          DTOutput("contents")
                      )
                    )
                )
            ),

            # Bouton de réinitialisation
            div(
              class = "d-flex justify-content-end mt-2",

              actionButton(
                "reset_button",
                "Réinitialiser",
                class = "btn btn-danger",
                icon = icon("sync")
              )
            )
        )
      )

    }
  })


  #---------------Section Accueil----------------------------------
  output$download_arbres<- downloadHandler(
    filename = function() {
      "Donnees_Exemple_Artemis.csv"
    },
    content = function(file) {
      file.copy("data/Donnees_Exemple.csv", file)
    }
  )

  output$download_climat_annuel<- downloadHandler(
    filename = function() {
      "Donnees_ClimAn_Exemple_Artemis.csv"
    },
    content = function(file) {
      file.copy("data/ClimAn_Exemple.csv", file)
    }
  )

  output$download_climat_mensuel<- downloadHandler(
    filename = function() {
      "Donnees_ClimMois_Exemple_Artemis.csv"
    },
    content = function(file) {
      file.copy("data/ClimMois_Exemple.csv", file)
    }
  )


  #---------------Section Données -------------------------------
  data_input <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })

  # Affichage du tableau de donnée
  output$contents <- renderDT({
    req(data())
    datatable(data(),
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel'),
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json')
              ),
              rownames = FALSE,
              filter = 'top',
              class = 'cell-border stripe compact small fs-5'
    )
  })

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
    simulation_terminee = FALSE,
    show_grade2 = FALSE,
    show_grade3 = FALSE

  )

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
    rv$max_annees_simulation <- NA
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
    df<-renommer_les_colonnes(df)

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

  # fonction réactive pour valider les champs optionels
  valider_champ_optionel <- reactive({
    req(data())

    # Appliquer les fonctions
    champ_optionel_absent <- trouver_noms_optionels(data())

    return(champ_optionel_absent)
  })

  # Indicateur visuel de validation
  output$validation_status <- renderUI({

    req(validation_errors())
    errors <- validation_errors()

    div(class = "mt-1 mb-0",

        # Si erreur
        if (length(errors) > 0) {
          div(class = "alert alert-danger d-flex align-items-center",
            icon("times-circle", class = "me-2"),
            div(
              tags$strong("Validation échouée — "),
              paste(length(errors), "erreur(s) détectée(s)")
            )
          )
        }

        # Si succès
        else {
          tagList(
            div(
              class = "alert alert-success d-flex align-items-center",
              icon("check-circle", class = "me-2"),
              div(
                tags$strong("Validation réussie: "),
                "Les données sont valides"
              )
            )
          )
        }
    )
  })

  # Afficher les erreurs
  output$error_box <- renderUI({
    req(validation_errors())
    errors <- validation_errors()
    if (length(errors) > 0){
      div( class = "alert alert-danger ",
           tags$strong("Erreurs détectées: "),

        tags$ul( class = "small ps-3 mb-0",
          lapply(errors, function(error) {
            tags$li(style = "margin-bottom: 1px; padding: 0;", error)
          })
        )
      )
    }
  })

  # Afficher les avertissements
  output$Avertissement_box <- renderUI({
      # Avertissement pour âge moyen
      if (!rv$age_moy_valid) {
        div(class = "alert alert-warning d-flex align-items-center",
          icon("exclamation-triangle", class = "me-2"),
          div(
            tags$strong("Attention: "),
            span(class= "small", "La colonne Age_moy est manquante ou contient des erreurs. ",
            "Vous ne pouvez pas utiliser les données climatiques.")
          )
        )
      }


    })

  # Afficher les autres informations
  output$Info_box <- renderUI({
    req(valider_champ_optionel())
    champ_optionel_absent <- valider_champ_optionel()

    # Avertissement pour champ optionnel
    if (length(champ_optionel_absent) > 0) {
      div( class = "alert alert-info d-flex align-items-center",
           icon("info-circle", class = "me-2"),
      div(
        tags$strong("Champs optionnels absents :"),
        tags$ul(class = "small ps-3 mb-0",
                lapply(champ_optionel_absent, function(x) {
                  tags$li(style = "margin-bottom: 1px; padding: 0;",x
                  )
                })
        ))
      )
      }
  })


  # Modifier la question d'extraction pour inclure les trois options
  observe({

    if (rv$data_valid && !rv$extraction_choice_made && !rv$extraction_completed) {

      rv$placette <- unique(data()$PlacetteID)

      output$extraction_question <- renderUI({

        div(
          class = "pt-1 pb-1",

          # Section pour choisir le type de simulation
          div(
            class = "ms-2 me-2 form-section text-primary fw-bold",
            span("Données climatiques")
          ),

          # Section Radio-buttons
          div(
            class = "card-body small pt-1 pb-0 mt-0",

            # Cas 1 : âge moyen invalide
            if (!rv$age_moy_valid) {

              tagList(
                radioButtons(
                  "extraction_choice", NULL,
                  choices = list(
                    "Simuler les données climatiques" = "extract",
                    "Fournir les données climatiques" = "upload",
                    "Simulation sans données climatiques" = "none"
                  ),
                  selected = "none"
                ),
                # Désactiver les 2 premiers radios buttons
                tags$script(HTML("
                $(document).ready(function() {
                  $('input[name=\"extraction_choice\"][value=\"extract\"]').prop('disabled', true);
                  $('input[name=\"extraction_choice\"][value=\"upload\"]').prop('disabled', true);
                });
              ")),

                div(
                  class = "text-warning fst-italic small",
                  icon("exclamation-triangle", class = "me-1"),

                  "La colonne Age_moy est manquante ou contient des erreurs. ",
                  "Vous ne pouvez pas utiliser les données climatiques dans votre simulation."
                )
              )
            }

            # Cas si trop de placettes
            else if (length(rv$placette) > 100) {

              tagList(

                radioButtons(
                  "extraction_choice", NULL,
                  choices = list(
                    "Simuler les données climatiques" = "extract",
                    "Fournir les données climatiques" = "upload",
                    "Simulation sans données climatiques" = "none"
                  ),
                  selected = "none"
                ),

                tags$script(HTML("
                $(document).ready(function() {
                  $('input[name=\"extraction_choice\"][value=\"extract\"]').prop('disabled', true);
                });
              ")),

                div(
                  class = "text-warning fst-italic small",

                  icon("exclamation-triangle", class = "me-1"),

                  "Nombre de placettes trop grand pour simuler les données climatiques. ",
                  "Ne doit pas dépasser 100."
                )
              )
            }

            # Cas normal
            else {

              radioButtons(
                "extraction_choice", NULL,
                choices = list(
                  "Simuler les données climatiques" = "extract",
                  "Fournir les données climatiques" = "upload",
                  "Simulation sans données climatiques" = "none"
                ),
                selected = character(0)
              )
            },

            # Section du bouton suivant
            div(
              class = "d-flex justify-content-center mt-1 mb-0",

              actionButton(
                "validate_extraction_choice",
                "Suivant",
                class = "btn btn-primary w-100",
                icon = icon("arrow-right")
              )
            )
          )
        )
      })
    }
  })

}


# Lancer l'application
shinyApp(ui = ui, server = server)
