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

    # Collapse la section
    tags$script(HTML("
    Shiny.addCustomMessageHandler('collapse_import_close', function(message) {
      var collapseElement = document.getElementById('collapse_import');
      var bsCollapse = new bootstrap.Collapse(collapseElement, {
        toggle: false }); bsCollapse.hide();});")),

    # Ouverture/fermture section tbe
    tags$script(HTML("
      Shiny.addCustomMessageHandler('toggle_tbe', function(msg){
      $('#enable_tbe').prop('disabled', msg.disable === true).prop('checked',false).trigger('change') ;
      if (msg.checked === true) { $('#tbe_details').prop('open', true);}
      });   ")),

    # Ouverture/fermeture section coupe
    tags$script(HTML("
      Shiny.addCustomMessageHandler('toggle_coupe', function(msg){
      $('#enable_coupe').prop('disabled', msg.disable === true).prop('checked',true) ;
      if (msg.checked === true) { $('#coupe_details').prop('open', true);}
      });   ")),

    # Style boite de placette
    tags$style(HTML("
    .bootstrap-select .dropdown-menu li {
    margin: 0 ;
    padding: 0 ;}

    .bootstrap-select .filter-option-inner-inner {
    font-size: 0.85em;}
    .bootstrap-select .dropdown-menu li a {
     padding-bottom: 6px;padding-top: 6px;line-height: 0.8;min-height: unset }")),

    tags$style(HTML("
    .bootstrap-select .bs-actionsbox {
    padding: 2px; }
    .bootstrap-select .bs-actionsbox .btn-group {
    display: flex !important;
    flex-direction: row;width: 100%;gap: 1px; }
    .bootstrap-select .bs-actionsbox .btn {
    display: flex;justify-content: center;align-items: center; }")),

    tags$style(HTML("
    .bootstrap-select {
    width: 100%; }"))




  ),

  # Solution temporaire pour faire fonctionner les renderUI des inputs (important de laisser ça là)
  div(style = "display:none;",
      selectInput("dummy_hidden", NULL, choices = "")
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

  observeEvent(input$tab_resultat, {
    current_tab("resultat")
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
        ),
        if (rv$simulation_terminee){
        actionLink(
          "tab_resultat", "Résultat",
          class = "text-white px-2 fs-2",
          style = paste0(
            "text-decoration:none; padding-bottom:10px;",
            if (current == "resultat")
              "border-bottom:4px solid lightgray;" else ""
          )
        )}
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
    }
    else if (current_tab() == "donnees") {

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

                      div(
                        class = "p-2",

                        fileInput(
                          "file",
                          "Choisir un fichier CSV",
                          buttonLabel = "Parcourir",
                          placeholder = "Aucun fichier sélectionné"
                        )
                      )
                     ,
                      uiOutput("validation_status"),
                      uiOutput("error_box"),
                      uiOutput("Avertissement_box"),
                      uiOutput("Info_box"),
                      uiOutput("extraction_question"),
                      uiOutput("extraction_button")
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
                      uiOutput("simulation_message")
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
    else if (current_tab() == "resultat") {

      div(class = "container-fluid px-0 mt-2",
          div(class = "row g-2",

      # Colonne de gauche (1/3)
      div(class = "col-md-4",

          # Options de visualisation
          div(class = "card mb-3",
              div(
                class = "card-header bg-secondary text-white d-flex justify-content-between align-items-center",
                span("Options de visualisation"),

                tags$a(
                  href = "#collapse_opt_vis",
                  `data-bs-toggle` = "collapse",
                  role = "button",
                  icon("chevron-down",class="text-white")
                )
              ),

              div(
                id = "collapse_opt_vis",
                class = "collapse show",
                div(class = "mt-1 ms-2",
                  # Groupe d'espèces
                  div(class = "fw-bold text-body mb-1",
                      "Groupe d'espèces"
                  ),
                  div(class = "pe-2",
                      selectInput(
                        inputId = "espece",
                        label = NULL,
                        choices = c("")
                      )
                  ),

                  # Variable
                  div(class = "mt-2",

                      div(class = "fw-bold text-body mb-1",
                          "Choix de la variable"
                      ),

                      div(class = "pe-2",
                          selectInput(
                            "variable",
                            label = NULL,
                            choices = c(
                              "Surface terrière marchande (m²/ha)" = "ST_HA",
                              "Volume marchand (m³/ha)" = "Vol_HA",
                              "Diamètre quadratique moyen" = "DMQ",
                              "Densité (nb/ha)" = "nbTi_HA"
                            ),
                            selected = "ST_HA"
                          )
                      )
                  ),

                # Placette
                  div(class = "mt-2",

                      div(class = "fw-bold text-body mb-1",
                          "Choix des placettes"
                      ),
                      div(class = "pe-2",
                          pickerInput(
                            inputId = "placette",
                            label = NULL,
                            choices = NULL,
                            selected = NULL,
                            multiple = TRUE,
                            width = "100%",
                            options = list(
                              `actions-box` = TRUE,
                              `deselect-all-text` = "Tout supprimer",
                              `select-all-text` = "Tout sélectionner",
                              `none-selected-text` = "Rien de sélectionné"
                            )
                          )
                      )
                  ),

                  # Info simulation
                  div(class = "",
                      div(class = "fw-bold text-primary mb-1",
                          "Information sur la simulation" ),
                      uiOutput("simulation_info")
                  )


                ))

              ),


              # Exportation des résultats
              div(class = "card",
                  div(
                    class = "card-header bg-secondary text-white d-flex justify-content-between align-items-center",
                    span("Exportation des résultats"),
                    tags$a(
                      href = "#collapse_exp_res",
                      `data-bs-toggle` = "collapse",
                      role = "button",
                      icon("chevron-down",class="text-white")
                    )
                  ),

                  div(
                    id = "collapse_exp_res",
                    class = "collapse show"
                    #ici
                  )
              )),

              # colonne de droite (2/3)
              div(class = "col-md-8",
                  # Visualisation des résultats
                  div(class = "card",
                      div(
                        class = "card-header bg-secondary text-white d-flex justify-content-between align-items-center",
                        span("Visualisation des résultats"),
                        tags$a(
                          href = "#collapse_resultat",
                          `data-bs-toggle` = "collapse",
                          role = "button",
                          icon("chevron-down",class="text-white")
                        )
                      ),

                      div(
                        id = "collapse_resultat",
                        class = "collapse show",

                        div(class = "card-body py-0 fs-5",
                            plotOutput("resultat_graphique", height = "600px")
                        )
                      )
                  )
              )

            ))

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

  # Observer qui réagit au clic sur le bouton Valider
  observeEvent(input$validate_extraction_choice, {
    # Vérifier si une option a été sélectionnée
    req(input$extraction_choice)
    rv$extraction_choice_made <- TRUE

    # Stocker explicitement le choix d'extraction dans la variable réactive
    rv$extraction_option <- input$extraction_choice

    # Faire disparaître la question d'extraction
    output$extraction_question <- renderUI({})

    # Fermer le collapse Importation des données
    if (input$extraction_choice != "upload"){
    session$sendCustomMessage(
      type = "collapse_import_close",
      message = list()
      )}

    # Simuler les données climatiques
    if (input$extraction_choice == "extract") {

      output$simulation_message <- renderUI({

        div(
          class = "px-3 p-2 mt-2",


          # Paramètre caché
          div(
            class = "d-none",
            numericInput(
              "annee_depart",
              "Année de départ :",
              value = as.numeric(format(Sys.Date(), "%Y")),
              min = 2000,
              step = 1
            )
          ),

          div(class = "fw-bold mb-1 text-primary", "Nombre d'années de simulation (multiple de 10) :"),

          # Choisir l'horizon
          div(class = "mb-2",
              numericInput(
                "horizon",NULL,
                value = 10,
                min = 10,
                step = 10
              )
          ),

          div(class = "fw-bold mb-1 text-primary", "Scénario RCP :"),

          # Choisir le scénario
          div(class = "mb-2 small",
              radioButtons(
                "rcp",NULL,
                choices = list("RCP 4.5" = "RCP45", "RCP 8.5" = "RCP85"),
                selected = "RCP45"
              )
          ),

          # Bouton d'extraction
          div(
            class = "d-flex justify-content-end mt-2 mb-0",
            uiOutput("extraction_button_final")
          )
        )
      })
    }

    # Fournir les données climatiques
    else if (input$extraction_choice == "upload") {

      output$extraction_button <- renderUI({

        div(
          class = "p-2 px-3 mt-2",

          # Titre
          div( class = "fw-bold text-primary mb-2",
            "Importer des données climatiques" ),

          # Fichier climat annuel
          div(class = "mb-2",
              fileInput(
                "climat_annuel_file",
                "Fichier climat annuel (CSV)",
                buttonLabel = "Parcourir",
                placeholder = "Aucun fichier sélectionné",
                accept = c( "text/csv", "text/comma-separated-values,text/plain", ".csv")
              )
          ),

          # Fichier climat mensuel
          div(class = "mb-2",
              fileInput(
                "climat_mensuel_file",
                "Fichier climat mensuel (CSV)",
                buttonLabel = "Parcourir",
                placeholder = "Aucun fichier sélectionné",
                accept = c( "text/csv", "text/comma-separated-values,text/plain", ".csv")
              )
          ),

          # Choisi le scénario
          div(class = "fw-bold mb-1 text-primary", "Scénario RCP :"),
          div(class = "mb-2 small",
              radioButtons(
                "rcp",NULL,
                choices = list("RCP 4.5" = "RCP45", "RCP 8.5" = "RCP85"),
                selected = "RCP45"
              )
          ),

          # Bouton
          div(
            class = "d-flex justify-content-end mt-2",
            actionButton(
              "validate_climat_files",
              "Valider les fichiers climatiques",
              class = "btn btn-sm btn-primary",
              icon = icon("check")
            )
          )
        )
      })
    }

    else if (input$extraction_choice == "none") {
      # Ne pas utiliser de données climatiques
      # Effacer le bouton d'extraction
      output$extraction_button <- renderUI({})
      output$extraction_button_final <- renderUI({})

      # Définir les variables climatiques comme NULL pour indiquer qu'elles ne sont pas utilisées
      rv$climat_annuel <- NULL
      rv$climat_mensuel <- NULL
      rv$max_annees_simulation <- NA

      # Mettre à jour l'état indiquant que le processus est terminé
      rv$extraction_completed <- TRUE

      simulation_ui()

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
                                sep = ";")

      # Lire le fichier climat mensuel
      climat_mensuel <- read.csv(input$climat_mensuel_file$datapath,
                                 header = TRUE,
                                 sep = ";")

      # Vérifier les fichiers avec les fonctions du package Artemis
      erreurs_annuel <- verifier_colonnes_ClimAn(climat_annuel)
      erreurs_annuel <- c(erreurs_annuel, validation_annuel(data(), climat_annuel,input$rcp))
      erreurs_mensuel <- verifier_colonnes_Clim(climat_mensuel)
      erreurs_mensuel <- c(erreurs_mensuel, validation_mensuel(data(), climat_mensuel,input$rcp))
      erreurs_mensuel <- c(erreurs_mensuel, valider_Mois(climat_mensuel,input$rcp) )


      # Valider que le fichier annuel et mensuel sont cohérents
      erreurs_comparaison <- comparer_annee_scenario(data(), climat_annuel,climat_mensuel,input$rcp)
      #erreurs_comparaison <- NULL

      # Vérifier s'il y a des erreurs
      if (length(erreurs_annuel) > 0 || length(erreurs_mensuel) > 0 || length(erreurs_comparaison) > 0 ) {
        showModal(modalDialog(
          div(class = "",
             h3("Erreurs dans les fichiers climatiques")),
          div( class="overflow-auto",
            style = "max-height: 400px;",

            # Section pour l'afficahge des erreurs du fichier climat annuel
            if (length(erreurs_annuel) > 0) {
              div(class = "alert alert-danger mb-3",
                h6(class = "mt-0 mb-2",
                  paste0( "Erreurs dans le fichier climat annuel (",
                    input$climat_annuel_file$name,"):"
                  )
                ),
                tags$ul(class = "small ps-3 mb-0",
                  lapply(erreurs_annuel, tags$li)
                )
              )
            },

            # Section pour l'afficahge des erreurs du fichier climat mensuel
            if (length(erreurs_mensuel) > 0) {
              div(class = "alert alert-danger mb-3",
                h6(class = "mt-0 mb-2",
                  paste0( "Erreurs dans le fichier climat mensuel (",
                    input$climat_mensuel_file$name, "):"
                  )
                ),
                tags$ul(
                  class = "small ps-3 mb-0",
                  lapply(erreurs_mensuel, tags$li)
                )
              )
            },
            # Section pour l'affichage des incohérences entre fichiers
            if (length(erreurs_comparaison) > 0 ){
              div(
                class = "alert alert-warning",
                h6( class = "mt-0 mb-2",
                  paste0( "Incohérence entre les fichiers : ",
                    input$climat_annuel_file$name,
                    " et ",
                    input$climat_mensuel_file$name
                  )
                ),
                tags$ul(
                  class = "small ps-3 mb-0",
                  lapply(erreurs_comparaison, tags$li)
                )
              )

            }
          ),
          # Footer
          footer = tagList(

            div(class = "text-center w-100",
              p(class = "fst-italic mb-2",
                "Veuillez corriger les erreurs et réimporter les fichiers."
              ),
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
        rv$max_annees_simulation <- floor(extraire_nb_annee(climat_annuel,AnneeDep=as.numeric(format(Sys.Date(), "%Y")))/10)*10

        # Afficher une notification de succès
        showNotification(
          "Fichiers climatiques validés et importés avec succès !",
          type = "message",
          duration = 5
        )

        # Mettre à jour l'état
        rv$extraction_completed <- TRUE

        # Collapse importation de données
        session$sendCustomMessage(
          type = "collapse_import_close",
          message = list()
        )

        simulation_ui()
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

  # Rendre le bouton d'extraction final (option simuler les données climatiques)
  output$extraction_button_final <- renderUI({
    req(input$annee_depart, input$horizon, input$rcp)

    # Cas erreur
    if (input$horizon < 10 || input$horizon %% 10 != 0) {

      div(class = "mt-2 small text-danger",
        icon("exclamation-triangle", class = "me-1"),
        "L'horizon doit être un multiple de 10 d'au moins 10 ans."
      )

    } else {
      # Cas valide
      div(
        class = "mt-0 w-100 justify-content-center",

        div(
          actionButton(
            "extract_climate",
            "Simuler les données climatiques",
            class = "btn btn-primary w-100",
            icon = icon("cloud-download-alt")
          )
        ),

        div(  class = "small text-muted text-center mt-1",

          paste0(
            "Période : ",
            input$annee_depart, " - ",
            input$annee_depart + input$horizon,
            " | Scénario : ",
            ifelse(input$rcp == "RCP45", "RCP 4.5", "RCP 8.5")
          )
        )

      )

    }
  })

  # Action pour l'extraction climatique
  observeEvent(input$extract_climate, {
    # Vérifier les paramètres
    req(input$annee_depart, input$horizon, input$rcp)

    # S'assurer que l'horizon est d'au moins 10 ans
    if (input$horizon < 10) {
      showNotification(
        "L'horizon doit être d'au moins 10 ans.",
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
      title = "Simulation en cours",

      div(class = "text-center",

        img(
          src = "https://i.gifer.com/origin/b4/b4d657e7ef262b88eb5f7ac021edda87.gif",
          height = "100px",
          class = "mb-3"
        ),

        p("Simulation des données climatiques en cours..."),

        div(class = "small text-muted",

          paste0(
            "Paramètres : Année de départ = ", annee_depart,
            ", Horizon = ", horizon, " ans (jusqu'à ", annee_fin,
            "), Scénario = ", rcp
          )
        )
      ),

      footer = NULL,
      easyClose = FALSE
    ))

    # Appeler la fonction GenereClimat
    result <- tryCatch({
      GenereClimat(Data_Ori= data() ,AnneeDep = annee_depart,AnneeFin = annee_fin,  RCP = rcp)
    }, error = function(e) {
      showNotification(paste("Erreur lors de la simulation:", e$message), type = "error", duration = 10)
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
      title = "Simulation terminée",

      div(class = "text-center",
        icon( "check-circle",
          class = "fa-3x text-primary mb-2"
        ),

        div(class = "fw-bold fs-3 mb-2",
          "Les données climatiques ont été extraites avec succès !"
        ),

        p("Vous pouvez télécharger les fichiers ci-dessous :"),

        # Bloc paramètres
        div(class = "border rounded p-2 bg-light text-start mt-2",

          div(
            class = "fw-semibold mb-1",
            "Paramètres utilisés :"
          ),

          tags$ul(class = "mb-0 ps-3",
            tags$li(style = "margin-bottom: 1px; padding: 0;", paste0("Année de départ : ", input$annee_depart)),
            tags$li(style = "margin-bottom: 1px; padding: 0;", paste0("Horizon : ", input$horizon)),
            tags$li(style = "margin-bottom: 1px; padding: 0;", paste0("Scénario climatique : ", input$rcp))
          )
        ),

        # Boutons download
        div(class = "d-flex justify-content-center gap-2 mt-3",

          downloadButton(
            "download_annuel",
            "Climat annuel",
            class = "btn btn-primary"
          ),

          downloadButton(
            "download_mensuel",
            "Climat mensuel",
            class = "btn btn-primary"
          )
        )
      ),

      # Bouton suivant
      footer = actionButton(
        "close_extraction",
        "Suivant",
        class = "btn btn-primary w-100"
      ),

      easyClose = FALSE,
      backdrop = "static"
    ))
  })

  # Télécharger simulation annuelle
  output$download_annuel <- downloadHandler(
    filename = function() {
      paste("climat_annuel_", input$annee_depart, "_", input$annee_depart + input$horizon - 1, "_", input$rcp, ".csv", sep = "")
    },
    content = function(file) {

      write.table(rv$climat_annuel, file, sep = ";", row.names = FALSE)
    }
  )

  # Télécharger simulation mensuelle
  output$download_mensuel <- downloadHandler(
    filename = function() {
      paste("climat_mensuel_", input$annee_depart, "_", input$annee_depart + input$horizon - 1, "_", input$rcp, ".csv", sep = "")
    },
    content = function(file) {
      write.table(rv$climat_mensuel, file, sep = ";", row.names = FALSE)
    }
  )

  # Fermer la boîte de dialogue d'extraction
  observeEvent(input$close_extraction, {
    removeModal()

    # Effacer les paramètres et le bouton d'extraction
    output$extraction_button <- renderUI({})
    output$extraction_button_final <- renderUI({})

    simulation_ui()

    # Mettre à jour l'état
    rv$extraction_completed <- TRUE
  })

  # Observateur pour le choix de simulation - avec désactivation des options supplémentaires
  simulation_ui <- function()
  {
    # Rediriger vers le panel de simulation avec les nouvelles options
    output$simulation_message <- renderUI({
      # Variable pour savoir si l'option "none" a été choisie
      no_climate_data <- !is.null(rv$extraction_option) && rv$extraction_option == "none"

      extracted_climate_data <- !is.null(rv$extraction_option) && rv$extraction_option == "extract"

      div(class = "mt-1 ms-2",

        # Paramètres de recrutement
        div(class = "mt-3",

          div(class = "fw-bold text-body mb-1",
            "Paramètres de recrutement ajustés"
          ),

          div(class = "small",
            radioButtons(
              "recrutement_ajuste",
              NULL,
              choices = list("Non" = "Non", "Oui" = "Oui"),
              selected = "Non",
              inline = TRUE
            )
          )
        ),

        # Coupe partielle
        div(class = "mt-3",

          div(class = "fw-bold text-body mb-1",
            "Coupe partielle réalisée depuis moins de 10 ans"
          ),

          div(class = "small",
            radioButtons(
              "coupe_partielle",
              NULL,
              choices = list("Non" = "Non", "Oui" = "Oui"),
              selected = "Non",
              inline = TRUE
            )
          )
        ),

        # Maladie corticale du hêtre
        div(class = "mt-3",

          div(class = "fw-bold text-body mb-1",
            "Maladie corticale du hêtre"
          ),

          div(class = "small",
            radioButtons(
              "mch",
              NULL,
              choices = list("Non" = "Non", "Oui" = "Oui"),
              selected = "Non",
              inline = TRUE
            )
          )
        ),

        # Module d'accroissement
        div(class = "mt-3",

          # Titre
          div(class = "fw-bold text-body mb-1",
            "Module d'accroissement"
          ),
            div(class = "pe-2",
              # selectInput
              selectInput(
                inputId = "module_accroissement",
                label = NULL,
                choices = list(
                  "Original" = "original",
                  "Wang 2023" = "brt",
                  "D'Orangeville 2018" = "gam",
                  "Fortin 2026" = "fortin"
                ),
                selected = "original",
                selectize = FALSE
              )),

          # Désactivation conditionnelle

          if (no_climate_data){
            tagList(
              tags$script(HTML("
        $(document).ready(function() {
          $('#module_accroissement option[value=\"brt\"]').prop('disabled', true);
          $('#module_accroissement option[value=\"gam\"]').prop('disabled', true);
          $('#module_accroissement option[value=\"fortin\"]').prop('disabled', true);
        });
      ")),
            )

          }
        ),

        # Module de mortalité
        div(class = "mt-3",

          # Titre
          div(class = "fw-bold text-body mb-1",
            "Module de mortalité"
          ),

          div(class = "pe-2",

            selectInput(
              inputId = "module_mortalite",
              label = NULL,
              choices = list(
                "Original" = "original",
                "Power 2025" = "que",
                "Power 2026" = "caneu"
              ),
              selected = "original",
              selectize = FALSE
            )
          ),

          # Désactivation conditionnelle
          if (no_climate_data) {
            tagList(
              tags$script(HTML("
        $(document).ready(function() {
          $('#module_mortalite option[value=\"que\"]').prop('disabled', true);
          $('#module_mortalite option[value=\"caneu\"]').prop('disabled', true);
        });
      ")),

              # Message utilisateur pour module d'accroissement et de mortalité
              div(class = "small fst-italic text-muted mt-1 pe-2",
                icon("info-circle", class = "me-1"),
                "Les modules d'accroissement et de mortalité avancés sont désactivés ",
                "car aucune donnée climatique n'est utilisée."
              )
            )
          }
        ),
        # Nombre d'années de simulation
        div(class = "mt-3",

          # Titre
          div(class = "fw-bold text-body mb-1",
            "Nombre d'années de simulation (multiple de 10)"
          ),

          div(class = "pe-2",
            numericInput(
              "annees_simulation",
              NULL,
              value = if (extracted_climate_data && !is.null(rv$extraction_horizon)) {
                rv$extraction_horizon * 10
              } else {
                10
              },
              min = 10,
              max = if (!no_climate_data) rv$max_annees_simulation else NA,
              step = 10
            )
          ),

          # Désactivation conditionnelle
          if (extracted_climate_data && !is.null(rv$extraction_horizon)) {
            tagList(
              tags$script(HTML("
        $(document).ready(function() {
          $('#annees_simulation').prop('disabled', true);
        });
      ")),

              # Message utilisateur
              div(class = "small fst-italic text-muted mt-1 pe-2",
                icon("info-circle", class = "me-1"),
                "Ce champ est automatiquement défini selon l'horizon de simulation climatique."
              )
            )
          }
        ),

        # Évolution du climat
        div(class = "mt-3",

          # Titre
          div(class = "fw-bold text-body mb-1",
            "Évolution du climat"
          ),
          div(class = "pe-2 small",

            radioButtons(
              "evolution_climat",
              NULL,
              choices = list("Oui" = "yes", "Non" = "no"),
              selected = if (no_climate_data) "no" else "yes",
              inline = TRUE
            )
          ),

          # Désactivation conditionnelle
          if (no_climate_data) {
            tagList(
              tags$script(HTML("
        $(document).ready(function() {
          $('input[name=\"evolution_climat\"]').prop('disabled', true);
        });
      ")),

              # Message utilisateur
              div(class = "small fst-italic text-muted mt-1 pe-2",
                icon("info-circle", class = "me-1"),
                "Option désactivée car aucune donnée climatique n'est utilisée."
              )
            )
          }
        ),

        # Traitement de coupe
        div(class = "mt-3",

          # Titre
          div(class = "fw-bold text-body mb-1",
            "Traitement de coupe"
          ),

          # Checkbox
          div(class = "pe-2 small",
            checkboxInput(
              "enable_coupe",
              "Activer les traitements de coupe",
              value = FALSE
            )
          ),

          # Panneau conditionnel
          conditionalPanel(
            condition = "input.enable_coupe == true",

            div(class = "pe-2 mt-1",

              tags$details(id="coupe_details",
                class = "border rounded p-2 bg-light",

                tags$summary(
                  class = "fw-semibold",
                  "Configurer les traitements de coupe par décennie"
                ),

                div(class = "mt-2",
                  uiOutput("coupe_config_ui")
                )
              )
            )
          )
        ),

        # TBE
        div(class = "mt-3",

          # Titre
          div(class = "fw-bold text-body mb-1",
            "Tordeuse des bourgeons de l'épinette (TBE)"
          ),

          # Checkbox
          div(class = "pe-2 small",
            checkboxInput(
              "enable_tbe",
              "Activer défoliation TBE",
              value = FALSE
            )
          ),

          # Message utilisateur
          conditionalPanel(
            condition = "input.module_mortalite != 'original' || input.module_accroissement != 'original'",

            div(class = "small fst-italic text-muted mt-1 pe-2",
              icon("info-circle", class = "me-1"),
              "La défoliation TBE s'active uniquement avec les modules ",
              "'Original' pour l'accroissement et la mortalité."
            )
          ),


          # Panneau conditionnelle
          conditionalPanel(
            condition = "input.enable_tbe == true",

            div(class = "pe-2 mt-1",
              tags$details(id="tbe_details", class = "border rounded p-2 bg-light",
                tags$summary( class = "fw-semibold",
                  "Sélectionnez les décennies avec défoliation sévère :"
                ),

                div(class = "mt-2",
                  uiOutput("tbe_config_ui")
                )
              )
            )
          )
        ),

        # Bouton pour lancer la simulation
        div(  class = "mt-3 mb-3 pe-2",

          actionBttn(
            "lancer_simulation",
            "Lancer la simulation",
            class = "btn btn-primary w-100",
            icon = icon("play-circle")
          )
        )
      )
      })
  }

  # Activation/désactivation de la case coupe
  observeEvent(input$enable_coupe, {

    if (input$enable_coupe) {
      session$sendCustomMessage(
        "toggle_coupe", list(disable = FALSE, checked = TRUE) )    }
  })


  #Observateur pour la coupe
  observeEvent(input$enable_coupe, {
    if (input$enable_coupe && !is.null(input$annees_simulation)) {
      horizon <- input$annees_simulation / 10
      # Initialiser seulement si pas déjà fait
      if (is.null(rv$coupe_on_vector)) {
        rv$coupe_on_vector <- rep(NA_real_, horizon)
        rv$coupe_modif_vector <- vector("list", horizon)
      }
    } else {
      # Réinitialiser les vecteurs quand la case est décochée
      rv$coupe_on_vector <- NULL
      rv$coupe_modif_vector <- NULL
    }
  })

  # Activation/désactivation de la case TBE selon module d'accroissement et de mortalité
  observeEvent(
    list(input$module_accroissement, input$module_mortalite),
    ignoreInit = TRUE,
    {
      desactiver_tbe <- !(input$module_accroissement == "original" &&
                            input$module_mortalite     == "original")
      # enabled si au moins un est "original"
      session$sendCustomMessage("toggle_tbe", list(disable = desactiver_tbe, checked = !desactiver_tbe))
    }
  )

  # Observateur pour TBE
  observeEvent(input$enable_tbe, {
    if (input$enable_tbe && !is.null(input$annees_simulation)) {
      horizon <- input$annees_simulation / 10
      # Initialiser seulement si pas déjà fait
      if (is.null(rv$tbe_vector)) {
        rv$tbe_vector <- rep(0, horizon)
      }
    } else {
      # Réinitialiser le vecteur quand la case est décochée
      rv$tbe_vector <- NULL
    }
  })

  # Section lorsque Traitement de coupe est activé
  output$coupe_config_ui <- renderUI({

    req(input$enable_coupe)
    horizon <- input$annees_simulation / 10

    isolate({


      div(class= "small",
          div(class = "fw-bold text-body mb-1",
              "Décennie de coupe: "
          ),
        selectInput(
          "decennie_coupe",
          label = NULL,
          choices = setNames( 0:(horizon - 1), paste("Décennie", 0:(horizon - 1), "-", 1:horizon)
          ),selected = NULL
        ),
        div(class = "fw-bold text-body mb-1",
            "Type de coupe: "
        ),
        selectInput(
          "type_coupe",
          label= NULL,
          choices = c("Aucune coupe" = "NA",
                      setNames(c(0:1,6:9,12:19), c("Coupe d'amélioration","Coupe d'éclaircie","Coupe de jardinage","Coupe progressive",
                                                   "Éclaircie commerciale","Éclaicie sélective","Coupe progressive (CPI_CP)",
                                                   "Coupe progressive (CPI_RL)","Coupe réserve semanciers","Jardinage CIMOTFF",
                                                   "Jarinage gr. arbres CIMOTFF", "CPI_CP CIMOTFF","CPI_RL CIMOTFF","CPRS"))),
          selected = "NA"
        ),

        # Section pour le type de modificateur
        div(class = "fw-bold text-body mb-1",
            "Type de modificateur: "
        ),
        radioButtons(
          "type_modif",
          label = NULL,
          choices = c(
            "Modificateur simple (même valeur pour toutes les essences)" = "simple",
            "Fichier (modificateurs par essence)" = "excel"
          ),
          selected = "simple",
          inline = TRUE
        ),

        # Interface conditionnelle selon le choix
        uiOutput("modificateur_ui"),

        # Button effacer
        actionButton(class = "btn btn-danger",
          "clear_coupes",
          "Effacer toutes les coupes",
          width= "100%"
        ),

        # Affichage du vecteur actuel
        div(class = "fw-bold text-body mb-1",
            "Configuration actuelle des coupes: "
        ),
        verbatimTextOutput("display_coupes")
      )
    })
  })

  output$modificateur_ui <- renderUI({

    req(input$type_modif)

    if (input$type_modif == "simple") {
      div(
      div(class = "fw-bold text-body mb-1",
          "Modificateur (%)"
      ),
      div(class= "mb-3",
      numericInput("modif_coupe", label=NULL,
          value = 0, min = -80, max = 160, step = 5)))

    } else {
      div(
        div(class = "fw-bold text-body mb-1",
            "Fichier "
        ),

      fileInput("modif_excel_file", label=NULL,
                buttonLabel = "Parcourir",
                placeholder = "Aucun fichier sélectionné",
                accept = c(".xlsx", ".xls", ".csv"),
                width= "100%"),

      div(style = "margin-top:-12px;",
          class = "small fst-italic text-muted mb-2",
          icon("info-circle", class = "me-1"),
          "Le fichier doit contenir les colonnes 'ess_ind' et 'modifier' (Excel ou CSV). Le modificateur doit se situer entre -80 et 160 %."
      )

      )

    }
  })

  # Interface pour TBE
  output$tbe_config_ui <- renderUI({

    req(input$enable_tbe)
    horizon <- input$annees_simulation / 10

    isolate({
      div(class = "small",

        div(class = "fw-bold text-body mb-1",
            "Décennie:"
        ),
        selectInput("decennie_tbe", label = NULL,
          choices = setNames(
            0:(horizon - 1),
            paste("Décennie", 0:(horizon - 1), "-", 1:horizon)
          ),
          selected = NULL
        ),

        div(class = "fw-bold text-body mb-1",
            "Défoliation TBE:"
        ),
        selectInput("effet_tbe",label = NULL,
          choices = c(
            "Absent" = 0,
            "Présent" = 1
          ),
          selected = 0
        ),

        # Button effacer
        div(class = "mt-2",
            actionButton("clear_tbe",
              "Effacer défoliations TBE",
              class = "btn btn-danger",
              width = "100%"
            )
        ),

        # Affichage du vecteur actuel
        div(class = "fw-bold text-body mb-1",
            "Configuration actuelle TBE:"
        ),
        uiOutput("display_tbe")

      )
    })
  })

  # Observateurs pour appliquer les modifications aux vecteurs
  observeEvent( list(input$type_coupe, input$modif_coupe, input$modif_excel_file), {
    req(input$decennie_coupe, input$type_coupe)

    if (input$type_coupe == "NA") {
      showNotification("Impossible d'appliquer une configuration avec 'Aucune coupe' sélectionnée.",
                       type = "warning", duration = 4)
      return()
    }

    decennie_idx <- as.numeric(input$decennie_coupe) + 1

    if (input$type_coupe == "NA") {
      rv$coupe_on_vector[decennie_idx] <- NA_real_
      rv$coupe_modif_vector[[decennie_idx]] <- NA
    } else {
      rv$coupe_on_vector[decennie_idx] <- as.numeric(input$type_coupe)

      if (!is.null(input$type_modif) && input$type_modif == "simple") {
        rv$coupe_modif_vector[[decennie_idx]] <- input$modif_coupe
      } else if (!is.null(input$type_modif) && input$type_modif == "excel") {
        if (!is.null(input$modif_excel_file) && !is.null(input$modif_excel_file$datapath)) {
          tryCatch({
            # Détecter le type de fichier par l'extension
            file_ext <- tools::file_ext(input$modif_excel_file$name)

            if (file_ext %in% c("xlsx", "xls")) {
              modif_data <- readxl::read_excel(input$modif_excel_file$datapath)
            } else if (file_ext == "csv") {
              modif_data <- read.csv(input$modif_excel_file$datapath, sep = ";", header = TRUE)
            } else {
              showNotification("Format de fichier non supporté. Utilisez Excel (.xlsx, .xls) ou CSV.",
                               type = "error", duration = 5)
              return()
            }

            if (!all(c("ess_ind", "modifier") %in% colnames(modif_data))) {
              showNotification("Le fichier doit contenir les colonnes 'ess_ind' et 'modifier'",
                               type = "error", duration = 5)
              return()
            }

            # Ajouter le nom du fichier au data.frame
            attr(modif_data, "filename") <- input$modif_excel_file$name
            rv$coupe_modif_vector[[decennie_idx]] <- modif_data

          }, error = function(e) {
            showNotification(paste("Erreur lors de la lecture du fichier:", e$message),
                             type = "error", duration = 5)
            return()
          })
        } else {
          showNotification("Veuillez sélectionner un fichier",
                           type = "error", duration = 5)
          return()
        }
      } else {
        rv$coupe_modif_vector[[decennie_idx]] <- 0
      }
    }

    showNotification(paste("Coupe appliquée à la décennie", input$decennie_coupe),
                     type = "message", duration = 2)
  })

  observeEvent({input$decennie_tbe
    input$effet_tbe
  }, {
    req(input$decennie_tbe, input$effet_tbe)

    decennie_idx <- as.numeric(input$decennie_tbe) + 1

    # Vérifier que l'index est valide
    if (decennie_idx > length(rv$tbe_vector)) {
      showNotification("Erreur: Index de décennie invalide", type = "error", duration = 5)
      return()
    }

    rv$tbe_vector[decennie_idx] <- as.numeric(input$effet_tbe)

    showNotification(paste("TBE appliqué à la décennie", input$decennie_tbe),
                     type = "message", duration = 2)
  })

  # Boutons pour effacer
  observeEvent(input$clear_coupes, {
    if (!is.null(rv$coupe_on_vector)) {
      rv$coupe_on_vector <- rep(NA_real_, length(rv$coupe_on_vector))
      rv$coupe_modif_vector <- vector("list", length(rv$coupe_modif_vector))
      showNotification("Toutes les coupes ont été effacées", type = "message", duration = 2)
    }
  })

  observeEvent(input$clear_tbe, {
    if (!is.null(rv$tbe_vector)) {
      rv$tbe_vector <- rep(0, length(rv$tbe_vector))
      showNotification("Tous les effets TBE ont été effacés", type = "message", duration = 2)
    }
  })

  # Affichage des vecteurs actuels
  output$display_coupes <- renderText({
    if (!is.null(rv$coupe_on_vector) && length(rv$coupe_on_vector) > 0) {
      coupe_display <- ifelse(is.na(rv$coupe_on_vector), "NA", as.character(rv$coupe_on_vector))

      modif_display <- sapply(seq_along(rv$coupe_modif_vector), function(i) {
        x <- rv$coupe_modif_vector[[i]]
        if (is.null(x) || (length(x) == 1 && is.na(x))) {
          "NA"
        } else if (is.numeric(x) && length(x) == 1) {
          paste0(x, "%")
        } else if (is.data.frame(x) && nrow(x) > 0) {
          filename <- attr(x, "filename")
          if (!is.null(filename)) {
            filename
          } else {
            paste0("Excel (", nrow(x), " essences)")
          }
        } else {
          "Vide"
        }
      })

      paste0("Coupe_ON: [", paste(coupe_display, collapse = ", "), "]\n",
             "Modif: [", paste(modif_display, collapse = ", "), "]")
    } else {
      "Aucune configuration"
    }
  })

  output$display_tbe <- renderText({
    if (!is.null(rv$tbe_vector)) {
      paste0("TBE: [", paste(rv$tbe_vector, collapse = ", "), "]")
    } else {
      "Aucune configuration"
    }
  })

  observe({
    if (!is.null(input$annees_simulation) && !is.null(input$enable_coupe) && input$enable_coupe) {
      new_horizon <- input$annees_simulation / 10

      # Redimensionner le vecteur coupe_on
      if (is.null(rv$coupe_on_vector) || length(rv$coupe_on_vector) != new_horizon) {
        old_vector <- rv$coupe_on_vector
        rv$coupe_on_vector <- rep(NA_real_, new_horizon)

        # Conserver les anciennes valeurs si elles existent
        if (!is.null(old_vector) && length(old_vector) > 0) {
          copy_length <- min(length(old_vector), new_horizon)
          rv$coupe_on_vector[1:copy_length] <- old_vector[1:copy_length]
        }
      }

      # Redimensionner la liste coupe_modif
      if (is.null(rv$coupe_modif_vector) || length(rv$coupe_modif_vector) != new_horizon) {
        old_list <- rv$coupe_modif_vector
        rv$coupe_modif_vector <- vector("list", new_horizon)

        # Conserver les anciennes valeurs si elles existent
        if (!is.null(old_list) && length(old_list) > 0) {
          copy_length <- min(length(old_list), new_horizon)
          rv$coupe_modif_vector[1:copy_length] <- old_list[1:copy_length]
        }
      }
    }
  })

  # Observer similaire pour TBE
  observe({
    if (!is.null(input$annees_simulation) && !is.null(input$enable_tbe) && input$enable_tbe) {
      new_horizon <- input$annees_simulation / 10

      # Redimensionner le vecteur TBE
      if (is.null(rv$tbe_vector) || length(rv$tbe_vector) != new_horizon) {
        old_vector <- rv$tbe_vector
        rv$tbe_vector <- rep(0, new_horizon)

        # Conserver les anciennes valeurs si elles existent
        if (!is.null(old_vector) && length(old_vector) > 0) {
          copy_length <- min(length(old_vector), new_horizon)
          rv$tbe_vector[1:copy_length] <- old_vector[1:copy_length]
        }
      }
    }
  })

  # Ajout d'un observateur pour l'action de lancer la simulation - avec restrictions des options
  observeEvent(input$lancer_simulation, {
    # Vérifier que tous les paramètres sont sélectionnés
    if (is.null(input$recrutement_ajuste) || is.null(input$coupe_partielle) || is.null(input$mch) ||
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

    # Vérifier que le nombre d'années est inférieur ou égal au nombre d'années du fichier climatique
    if (input$extraction_choice=="upload"){

      if (input$annees_simulation > rv$max_annees_simulation) {
        showNotification(
          "Le nombre d'années de simulation dépasse l'horizon des données climatiques",
          type = "error",
          duration = 5
        )
        return()
      }
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
          "Les modules d'accroissement Wang 2023 et D'Orangeville 2018 nécessitent des données climatiques.",
          type = "error",
          duration = 5
        )
        return()
      }

      if (input$module_mortalite == "que") {
        showNotification(
          "Le module de mortalité Power 2025 nécessite des données climatiques.",
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
        div(class = "text-center",

          img(
            src = "https://i.gifer.com/origin/b4/b4d657e7ef262b88eb5f7ac021edda87.gif",
            height = "100px",
            class = "mb-3"
          ),

          p(class = "fw-bold mb-2",
            "Simulation en cours..."
          ),

          p(class = "small text-muted",
            "Cela peut prendre plusieurs minutes. Veuillez patienter."
          )
        ),

        footer = NULL,
        easyClose = FALSE,
        backdrop = "static"
      )
    )

    # Conversion des choix d'interface en paramètres pour la fonction
    Tendance <- ifelse(input$recrutement_ajuste == "oui", 1, 0)
    Residuel <- ifelse(input$coupe_partielle == "oui", 1, 0)
    mch <- ifelse(input$mch == "oui", 1, 0)

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
                         "gam" = "GAM",
                         "fortin"="QUE")
      MortModif <- switch(input$module_mortalite,
                          "original" = "ORI",
                          "que" = "QUE",
                          "caneu" = "CANEU")
    }

    # Déterminer le RCP à utiliser
    RCP_value <- ifelse(!is.null(input$rcp),
                        input$rcp,
                        "RCP45")  # Valeur par défaut


    coupe_on <- if (!is.null(input$enable_coupe) && input$enable_coupe) {
      rv$coupe_on_vector
    } else {
      NULL
    }

    coupe_modif <- if (!is.null(input$enable_coupe) && input$enable_coupe) {
      as.list(rv$coupe_modif_vector)
    } else {
      NULL
    }

    tbe <- if (!is.null(input$enable_tbe) && input$enable_tbe) {
      rv$tbe_vector
    } else {
      NULL
    }

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
        MortModif = MortModif,
        RCP = RCP_value,
        Coupe_ON = coupe_on,
        Coupe_modif = coupe_modif,
        TBE = tbe,
        MCH = mch
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
      module_acc_utilise <- if (no_climate_data) "Original" else switch(input$module_accroissement,
                                                                        "original" = "Original",
                                                                        "brt" = "Wang 2023",
                                                                        "gam" = "D'Orangeville 2018",
                                                                        "fortin"= "Fortin 2026")

      module_mort_utilise <- if (no_climate_data) "Original" else switch(input$module_mortalite,
                                                                         "original" = "Original",
                                                                         "que" = "Power 2025",
                                                                         "caneu" = "Power 2026")

      # Afficher un résultat de simulation
      showModal(modalDialog(title = "Simulation terminée",

        div(class = "text-center",
          icon( "check-circle",
          class = "fa-3x text-primary mb-2" ),
            div(class = "fw-bold fs-3 mb-2",
              "La simulation a été effectuée avec succès !"),

            # Résumé
          div(class = "border rounded p-2 bg-light text-start mt-2",

              div(class = "fw-semibold mb-1",
                "Paramètres utilisés :"
              ),

              tags$ul(class = "mb-0 ps-3",
                tags$li(style = "margin-bottom: 1px; padding: 0;",paste0("Paramètres de recrutement ajustés : ", input$recrutement_ajuste)),
                tags$li(style = "margin-bottom: 1px; padding: 0;",paste0("Coupe partielle récente : ", input$coupe_partielle)),
                tags$li(style = "margin-bottom: 1px; padding: 0;",paste0("Maladie corticale du hêtre : ", input$mch)),
                tags$li(style = "margin-bottom: 1px; padding: 0;",paste0("Module d'accroissement : ", module_acc_utilise)),
                tags$li(style = "margin-bottom: 1px; padding: 0;",paste0("Module de mortalité : ", module_mort_utilise)),
                tags$li(style = "margin-bottom: 1px; padding: 0;",paste0("Nombre d'années : ", input$annees_simulation)),
                tags$li(style = "margin-bottom: 1px; padding: 0;",paste0("Défoliation TBE : ", ifelse(input$enable_tbe, "Oui", "Non"))),
                tags$li(style = "margin-bottom: 1px; padding: 0;",paste0("Traitement de coupe : ", ifelse(input$enable_coupe, "Oui", "Non"))),

                if (input$enable_coupe) {
                  div(class = "text-body", style = "margin-bottom: 1px; padding: 0;",
                    uiOutput("display_coupes")
                  )
                },

                if (no_climate_data) {
                  tags$li(style = "margin-bottom: 1px; padding: 0;","Évolution du climat : Non (données climatiques non utilisées)")
                } else {
                  tags$li(style = "margin-bottom: 1px; padding: 0;",paste0("Évolution du climat : ", ifelse(input$evolution_climat == "yes", "Oui", "Non")))
                },

                if (!no_climate_data) {
                  tags$li(style = "margin-bottom: 1px; padding: 0;",paste0("Scénario RCP : ", RCP_value))
                }
              )
            )
          ),

          footer = actionButton(
            "close_simulation",
            "Suivant",
            class = "btn btn-primary"
          ),

          easyClose = FALSE,
          backdrop = "static"
        )
      )
      print("test")

    }
  })


#-----------------------Section Résultat------------------------------

  # Changement de page vers les résultats
  observeEvent(input$close_simulation, {
    removeModal()
    if (!is.null(rv$resultats_simulation)) {
      saveRDS(rv$resultats_simulation, "cached_simulation_results.rds")
      cat("✓ Simulation results saved for development\n")
    }

    rv$simulation_terminee <- TRUE
    current_tab("resultat")

  })

  # Charger les options de visualisation
  observeEvent(current_tab(), {

    if (current_tab() == "resultat") {

      req(rv$resultats_simulation)
      # Extraire les especes uniques
      listeEspece <- unique(rv$resultats_simulation$GrEspece)

      # Mettre à jour la liste déroulante
      updateSelectInput(
        session,
        "espece",
        choices = c("TOT", listeEspece),
        selected = "TOT"
      )

      # Extraire toutes les placettes uniques des résultats
      placettes <- unique(rv$resultats_simulation$PlacetteID)

      # Mettre à jour le sélecteur de placettes
      updatePickerInput(
        session,
        "placette",
        choices = placettes,
        selected = placettes
      )
    }

  })


  # Information sur la simulation
  output$simulation_info <- renderUI({

    req(rv$resultats_simulation)

    # Détection climat
    no_climate_data <- !is.null(rv$extraction_option) && rv$extraction_option == "none"
    # Définir les valeurs réelles utilisées pour les modules en cas d'absence de données climatiques
    module_acc_utilise <- if (no_climate_data) "Original" else switch(input$module_accroissement,
                                                                      "original" = "Original",
                                                                      "brt" = "Wang 2023",
                                                                      "gam" = "D'Orangeville 2018",
                                                                      "fortin"= "Fortin 2026")

    module_mort_utilise <- if (no_climate_data) "Original" else switch(input$module_mortalite,
                                                                       "original" = "Original",
                                                                       "que" = "Power 2025",
                                                                       "caneu" = "Power 2026")

    div(class = "border rounded p-1 bg-light text-start mt-2 me-2 mb-2 ",

        tags$ul(class = "mb-0 ps-3",
                tags$li(style = "margin-bottom: 1px; padding: 0;",paste0("Recrutement ajustés : ", input$recrutement_ajuste)),
                tags$li(style = "margin-bottom: 1px; padding: 0;",paste0("Coupe partielle : ", input$coupe_partielle)),
                tags$li(style = "margin-bottom: 1px; padding: 0;",paste0("MCH : ", input$mch)),
                tags$li(style = "margin-bottom: 1px; padding: 0;",paste0("Module d'accroissement : ", module_acc_utilise)),
                tags$li(style = "margin-bottom: 1px; padding: 0;",paste0("Module de mortalité : ", module_mort_utilise)),
                tags$li(style = "margin-bottom: 1px; padding: 0;",paste0("Années de simulation: ", input$annees_simulation)),
                tags$li(style = "margin-bottom: 1px; padding: 0;",paste0("Défoliation TBE : ", ifelse(input$enable_tbe, "Oui", "Non"))),
                tags$li(style = "margin-bottom: 1px; padding: 0;",paste0("Traitement de coupe : ", ifelse(input$enable_coupe, "Oui", "Non"))),

                if (input$enable_coupe) {
                  div(class = "text-body", style = "margin-bottom: 1px; padding: 0;",
                      uiOutput("display_coupes")
                  )
                },

                if (no_climate_data) {
                  tags$li(style = "margin-bottom: 1px; padding: 0;","Évolution du climat : Non (données climatiques non utilisées)")
                } else {
                  tags$li(style = "margin-bottom: 1px; padding: 0;",paste0("Évolution du climat : ", ifelse(input$evolution_climat == "yes", "Oui", "Non")))
                },

                if (!no_climate_data) {
                  tags$li(style = "margin-bottom: 1px; padding: 0;",paste0("Scénario RCP : ", input$rcp))
                }
        )
    )
  })

  # Affichage du résultat en graphique
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

  observeEvent(input$add_grade2, {
    rv$show_grade2 <- TRUE
  })

  observeEvent(input$add_grade3, {
    rv$show_grade3 <- TRUE
  })

  observeEvent(input$remove_grade2, {
    rv$show_grade2 <- FALSE
    rv$show_grade3 <- FALSE  # Si on supprime Grade 2, supprimer aussi Grade 3

    # Réinitialiser les valeurs du Grade 2 et 3
    updateTextInput(session, "nom_grade2", value = "")
    updateSelectInput(session, "long_grade2", selected = "-- Aucune --")
    updateNumericInput(session, "diam_grade2", value = NA)

    updateTextInput(session, "nom_grade3", value = "")
    updateSelectInput(session, "long_grade3", selected = "-- Aucune --")
    updateNumericInput(session, "diam_grade3", value = NA)
  })

  observeEvent(input$remove_grade3, {
    rv$show_grade3 <- FALSE

    # Réinitialiser les valeurs du Grade 3
    updateTextInput(session, "nom_grade3", value = "")
    updateSelectInput(session, "long_grade3", selected = "-- Aucune --")
    updateNumericInput(session, "diam_grade3", value = NA)
  })

  # Observer pour afficher le Grade 3 (seulement si Grade 2 existe)
  observeEvent(input$add_grade3, {
    if (rv$show_grade2) {  # Vérification de sécurité
      rv$show_grade3 <- TRUE
    }
  })

# ------------ Bouton reset -------------------
  # Gestion de la réinitialisation
  observeEvent(input$reset_button, {

    showModal(
      modalDialog(
        title = "Confirmation de réinitialisation",

        div(
          class = "text-center",

          # Message principal
          p(
            class = "fw-bold mb-2",
            "Êtes-vous sûr de vouloir réinitialiser l'application?"
          ),

          # Message secondaire
          p(
            class = "small text-muted mb-2",
            "Toutes les données et simulations actuelles seront perdues."
          ),

          # Message d'avertissement
          p(
            class = "text-danger fw-semibold",
            "Cette action est irréversible."
          )
        ),

        footer = tagList(

          # Bouton danger
          actionButton(
            "confirm_reset",
            "Oui, réinitialiser",
            class = "btn btn-danger"
          ),

          # Bouton annuler
          modalButton(
            "Annuler",
            class = "btn btn-secondary"
          )
        ),

        easyClose = TRUE
      )
    )
  })

  observeEvent(input$confirm_reset, {
    rv$simulation_terminee <- FALSE
    session$reload()
  })



}


# Lancer l'application
shinyApp(ui = ui, server = server)
