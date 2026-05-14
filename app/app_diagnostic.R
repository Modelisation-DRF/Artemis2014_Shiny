# ============================================================================
# Application de diagnostic Artemis - Installation des packages manquants
# Version robuste qui fonctionne même si certains packages sont absents
# ============================================================================

# Détecter si on est dans RStudio (interactif) ou lancé en batch (VBS)
is_rstudio <- Sys.getenv("RSTUDIO") == "1"
is_interactive_session <- interactive() && is_rstudio

# Configuration du miroir CRAN
options(repos = c(CRAN = "https://cloud.r-project.org"))

# ============================================================================
# Chargement des packages de base (avec gestion d'erreur)
# ============================================================================

# Charger shiny (obligatoire)
if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Le package 'shiny' est requis mais n'est pas installé.")
}
library(shiny)

# Essayer de charger shinyjs (optionnel mais utile)
has_shinyjs <- requireNamespace("shinyjs", quietly = TRUE)
if (has_shinyjs) {
  library(shinyjs)
}

# ============================================================================
# Lecture des packages manquants depuis le fichier sauvegardé
# ============================================================================

packages_manquants <- character(0)
packages_cran <- character(0)
packages_github <- character(0)

status_file <- "package_status.rds"

if (file.exists(status_file)) {
  status <- readRDS(status_file)
  packages_manquants <- status$manquants
  packages_cran <- status$cran
  packages_github <- status$github
  cat("Packages manquants chargés:", paste(packages_manquants, collapse = ", "), "\n")
} else {
  cat("ATTENTION: Fichier package_status.rds introuvable dans:", getwd(), "\n")
}

# ============================================================================
# Mapping des noms de packages vers les repos GitHub
# (certains packages ont un nom différent du repo)
# ============================================================================

# Artemis est dans le repo "Artemis2014" sur GitHub
github_repo_mapping <- c(
  "Artemis2014" = "Artemis2014",
  "BillonnagePetro" = "BillonnagePetro",
  "ExtractMap" = "ExtractMap",
  "OutilsDRF" = "OutilsDRF",
  "BioSIM" = "BioSIM"
)

# Fonction pour obtenir le nom du repo GitHub
get_github_repo <- function(pkg_name) {
  if (pkg_name %in% names(github_repo_mapping)) {
    return(github_repo_mapping[[pkg_name]])
  }
  return(pkg_name)
}

# ============================================================================
# Server - Logique serveur
# ============================================================================

server <- function(input, output, session) {
  
  # État de l'installation
  install_state <- reactiveValues(
    installing = FALSE,
    completed = FALSE,
    logs = character(0),
    installed_packages = character(0),
    failed_packages = character(0)
  )
  
  # Fonction pour ajouter un log
  add_log <- function(message) {
    timestamp <- format(Sys.time(), "%H:%M:%S")
    install_state$logs <- c(install_state$logs, paste0("[", timestamp, "] ", message))
  }
  
  # ============================================================================
  # Contenu principal (dynamique selon l'état)
  # ============================================================================
  
  output$main_content <- renderUI({
    
    if (install_state$completed) {
      # ═══════════════════════════════════════════════════════════════
      # ÉTAT 3 : Installation terminée
      # ═══════════════════════════════════════════════════════════════
      tagList(
        div(class = "warning-icon", "✅"),
        h2(style = "text-align: center; color: #28a745; margin-bottom: 20px;", 
           "Installation terminée"),
        
        if (length(install_state$failed_packages) == 0) {
          div(
            class = "success-box",
            h4(style = "color: #28a745;", "✓ Tous les packages ont été installés !"),
            p("Vous pouvez maintenant relancer l'application Artemis.")
          )
        } else {
          tagList(
            if (length(install_state$installed_packages) > 0) {
              div(
                class = "success-box",
                h4(style = "color: #28a745;", "✓ Packages installés :"),
                p(paste(install_state$installed_packages, collapse = ", "))
              )
            },
            div(
              class = "package-list",
              h4(style = "color: #ff6b6b;", "✗ Échec d'installation :"),
              p(paste(install_state$failed_packages, collapse = ", ")),
              p("Essayez l'installation manuelle ci-dessous.")
            )
          )
        },
        
        div(
          class = "info-box",
          style = "border-left-color: #28a745;",
          h4(style = "color: #28a745;", "📋 Prochaine étape :"),
          p("Fermez cette fenêtre puis relancez Artemis normalement ",
            "(double-clic sur Lancer_Artemis.vbs ou depuis RStudio).")
        ),
        
        actionButton("close_btn_final", "✓ Fermer cette fenêtre", 
                     class = "btn-install btn-success"),
        
        h4(style = "margin-top: 30px; color: #888;", "📜 Journal d'installation :"),
        div(class = "log-output", HTML(paste(install_state$logs, collapse = "<br>")))
      )
      
    } else if (install_state$installing) {
      # ═══════════════════════════════════════════════════════════════
      # ÉTAT 2 : Installation en cours
      # ═══════════════════════════════════════════════════════════════
      tagList(
        div(class = "warning-icon", "⏳"),
        h2(style = "text-align: center; color: #f39c12; margin-bottom: 20px;", 
           "Installation en cours..."),
        p(style = "text-align: center; font-size: 16px;", 
          "Veuillez patienter, ne fermez pas cette fenêtre."),
        
        div(
          style = "margin: 30px 0;",
          div(
            style = "background: rgba(255,255,255,0.1); border-radius: 10px; height: 20px; overflow: hidden;",
            div(
              style = "background: linear-gradient(90deg, #008000, #00aa00); height: 100%; width: 100%; animation: progress 2s ease-in-out infinite;",
              class = "progress-bar-animated"
            )
          )
        ),
        
        tags$style(HTML("
          @keyframes progress {
            0% { transform: translateX(-100%); }
            100% { transform: translateX(100%); }
          }
        ")),
        
        h4(style = "margin-top: 30px; color: #ccc;", "📜 Progression :"),
        div(class = "log-output", HTML(paste(install_state$logs, collapse = "<br>")))
      )
      
    } else {
      # ═══════════════════════════════════════════════════════════════
      # ÉTAT 1 : Affichage initial - Packages manquants
      # ═══════════════════════════════════════════════════════════════
      tagList(
        div(class = "warning-icon", "⚠️"),
        h2(style = "text-align: center; color: #f39c12; margin-bottom: 10px;", 
           "Packages manquants"),
        p(style = "text-align: center; font-size: 16px; color: #ccc;", 
          "Certains packages requis par Artemis ne sont pas installés."),
        
        # Liste des packages manquants
        div(
          class = "package-list",
          h4(style = "color: #ff6b6b;", 
             paste0("📦 ", length(packages_manquants), " package(s) manquant(s) :")),
          lapply(packages_manquants, function(pkg) {
            source_type <- if (pkg %in% packages_github) " (GitHub)" else " (CRAN)"
            div(class = "package-item", paste0(pkg, source_type))
          })
        ),
        
        # Bouton d'installation
        actionButton("install_btn", "🚀 Installer automatiquement", 
                     class = "btn-install btn-primary"),
        
        # Avertissement Rtools pour packages GitHub
        if (length(packages_github) > 0) {
          div(
            class = "warning-box",
            h4(style = "color: #f1c40f; margin-bottom: 10px;", 
               "⚠️ Note pour les packages GitHub"),
            p("L'installation des packages GitHub nécessite ", 
              tags$strong("Rtools"), " sur Windows."),
            p("Si l'installation automatique échoue, téléchargez Rtools depuis :"),
            p(tags$a(href = "https://cran.r-project.org/bin/windows/Rtools/", 
                     target = "_blank",
                     "https://cran.r-project.org/bin/windows/Rtools/"))
          )
        },
        
        # Avertissement spécial pour ExtractMap (package volumineux)
        if ("ExtractMap" %in% packages_github) {
          div(
            class = "danger-box",
            h4(style = "color: #e74c3c; margin-bottom: 10px;", 
               "⚠️ Note importante pour ExtractMap"),
            p("Le package ", tags$strong("ExtractMap"), " est très volumineux et peut échouer ",
              "lors de l'installation automatique (timeout ou mémoire)."),
            p("Si l'installation échoue, procédez à l'installation manuelle :"),
            tags$ol(
              style = "margin: 10px 0 0 0; padding-left: 20px;",
              tags$li(
                "Télécharger le dossier ZIP depuis GitHub : ",
                tags$a(
                  href = "https://github.com/Modelisation-DRF/ExtractMap",
                  target = "_blank",
                  "ExtractMap"
                )
              ),
              tags$li("Décompresser le dossier"),
              tags$li(
                "Dans R, installer depuis le dossier local :",
                tags$pre(
                  style = "margin-top: 5px; font-size: 13px;",
                  'install.packages("chemin/vers/ExtractMap", repos = NULL, type = "source")'
                )
              )
            )
          )
        },
        
        # Instructions d'installation manuelle
        div(
          class = "info-box",
          h4(style = "color: #3498db;", "🔧 Installation manuelle (alternative)"),
          p("Si l'installation automatique ne fonctionne pas, ouvrez R ou RStudio et exécutez :"),
          
          if (length(packages_cran) > 0) {
            tagList(
              p(style = "margin-top: 15px; font-weight: bold;", "Packages CRAN :"),
              tags$pre(paste0('install.packages(c("', paste(packages_cran, collapse = '", "'), '"))'))
            )
          },
          
          if (length(packages_github) > 0) {
            tagList(
              p(style = "margin-top: 15px; font-weight: bold;", "Packages GitHub :"),
              tags$pre(paste0(
                '# Installer remotes si nécessaire\n',
                'install.packages("remotes")\n\n',
                '# Installer les packages GitHub\n',
                paste0(
                  sapply(packages_github, function(pkg) {
                    repo <- get_github_repo(pkg)
                    paste0('remotes::install_github("Modelisation-DRF/', repo, '")')
                  }),
                  collapse = '\n'
                )
              ))
            )
          }
        ),
        
        # Bouton fermer
        actionButton("close_btn", "✕ Fermer l'application", 
                     class = "btn-install btn-secondary",
                     style = "margin-top: 20px;"),
        
        # Footer
        hr(style = "margin-top: 40px; border-color: rgba(255,255,255,0.1);"),
        p(style = "text-align: center; color: #666; font-size: 13px;",
          "Artemis - Ministère des Ressources Naturelles et des Forêts du Québec")
      )
    }
  })
  
  # ============================================================================
  # Gestion du bouton d'installation
  # ============================================================================
  
  observeEvent(input$install_btn, {
    install_state$installing <- TRUE
    install_state$logs <- character(0)
    
    add_log("🚀 Démarrage de l'installation...")
    
    # --- Installer les packages CRAN ---
    if (length(packages_cran) > 0) {
      add_log(paste0("📦 Packages CRAN : ", paste(packages_cran, collapse = ", ")))
      
      for (pkg in packages_cran) {
        add_log(paste0("   → ", pkg, "..."))
        
        tryCatch({
          utils::install.packages(pkg, quiet = TRUE, dependencies = TRUE)
          
          if (nzchar(system.file(package = pkg))) {
            add_log(paste0("   ✓ ", pkg, " installé avec succès"))
            install_state$installed_packages <- c(install_state$installed_packages, pkg)
          } else {
            add_log(paste0("   ✗ ", pkg, " - échec"))
            install_state$failed_packages <- c(install_state$failed_packages, pkg)
          }
        }, error = function(e) {
          add_log(paste0("   ✗ ", pkg, " - ", conditionMessage(e)))
          install_state$failed_packages <- c(install_state$failed_packages, pkg)
        })
      }
    }
    
    # --- Installer les packages GitHub ---
    if (length(packages_github) > 0) {
      add_log(paste0("📦 Packages GitHub : ", paste(packages_github, collapse = ", ")))
      
      # S'assurer que remotes est installé
      if (!nzchar(system.file(package = "remotes"))) {
        add_log("   → Installation de remotes...")
        tryCatch({
          utils::install.packages("remotes", quiet = TRUE)
          add_log("   ✓ remotes installé")
        }, error = function(e) {
          add_log(paste0("   ✗ remotes - ", conditionMessage(e)))
        })
      }
      
      for (pkg in packages_github) {
        # Obtenir le nom du repo GitHub (peut être différent du nom du package)
        repo_name <- get_github_repo(pkg)
        
        add_log(paste0("   → ", pkg, " (depuis GitHub: Modelisation-DRF/", repo_name, ")..."))
        
        tryCatch({
          remotes::install_github(
            paste0("Modelisation-DRF/", repo_name),
            quiet = TRUE,
            upgrade = "never"
          )
          
          if (nzchar(system.file(package = pkg))) {
            add_log(paste0("   ✓ ", pkg, " installé avec succès"))
            install_state$installed_packages <- c(install_state$installed_packages, pkg)
          } else {
            add_log(paste0("   ✗ ", pkg, " - échec"))
            install_state$failed_packages <- c(install_state$failed_packages, pkg)
          }
        }, error = function(e) {
          add_log(paste0("   ✗ ", pkg, " - ", conditionMessage(e)))
          install_state$failed_packages <- c(install_state$failed_packages, pkg)
        })
      }
    }
    
    add_log("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
    add_log(paste0("✅ Terminé : ", length(install_state$installed_packages), " réussi(s), ",
                   length(install_state$failed_packages), " échec(s)"))
    
    install_state$installing <- FALSE
    install_state$completed <- TRUE
  })
  
  # ============================================================================
  # Gestion des boutons fermer
  # ============================================================================
  
  close_app <- function() {
    # Tenter de fermer l'onglet du navigateur
    session$sendCustomMessage(type = "closeWindow", message = "close")
    
    showModal(modalDialog(
      div(
        style = "text-align: center; padding: 20px;",
        div(style = "font-size: 48px; margin-bottom: 20px;", "✅"),
        h3("Application fermée", style = "color: #28a745;"),
        p("Le serveur R a été arrêté."),
        p(tags$strong("Vous pouvez fermer cet onglet du navigateur.")),
        p(style = "color: #888; font-size: 14px;", "(Raccourci : Ctrl + W)"),
        hr(),
        p(style = "color: #008000; font-weight: bold;", 
          "N'oubliez pas de relancer Artemis !")
      ),
      title = NULL,
      footer = NULL,
      easyClose = FALSE
    ))
    
    # Délai avant fermeture
    if (has_shinyjs) {
      shinyjs::delay(500, {
        stopApp()
        if (!is_interactive_session) {
          q(save = "no")
        }
      })
    } else {
      # Sans shinyjs, utiliser invalidateLater via un observer
      observe({
        invalidateLater(500)
        isolate({
          stopApp()
          if (!is_interactive_session) {
            q(save = "no")
          }
        })
      })
    }
  }
  
  observeEvent(input$close_btn, { close_app() })
  observeEvent(input$close_btn_final, { close_app() })
  
  # Fermeture de session (X du navigateur)
  session$onSessionEnded(function() {
    if (!is_interactive_session) {
      q(save = "no")
    }
  })
}

# ============================================================================
# Lancement de l'application
# ============================================================================

# Utiliser runApp() pour que l'app s'ouvre dans le navigateur
# quand le script est sourcé (et pas juste retourner un objet shinyApp)
app <- shinyApp(ui, server)
runApp(app, launch.browser = TRUE)
