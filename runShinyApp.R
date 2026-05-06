# ============================================================================
# Script de lancement Artemis
# Ce script vérifie les packages et lance l'application appropriée
# ============================================================================

# Créer le fichier log unique (nom fixe, sera écrasé à chaque lancement)
log_file <- "artemis_log.txt"
log_con <- file(log_file, open = "wt")

# Fonction de logging qui écrit dans le fichier ET affiche à la console
log_cat <- function(...) {
  msg <- paste0(...)
  cat(msg, file = log_con)
  cat(msg)  # Afficher aussi à la console
  flush(log_con)
}

# Sauvegarder le chemin du log dans l'environnement pour app.R
Sys.setenv(ARTEMIS_LOG_FILE = log_file)

log_cat("========================================\n")
log_cat("       Démarrage Artemis\n")
log_cat("========================================\n\n")

# Configuration du miroir CRAN
options(repos = c(CRAN = "https://cloud.r-project.org"))

# ============================================================================
# ÉTAPE 0 : Configuration des chemins de bibliothèques
# ============================================================================

log_cat("Configuration des chemins R...\n")

# Lire le fichier .Renviron pour trouver R_LIBS_USER
renviron_path <- file.path(Sys.getenv("HOME"), ".Renviron")
if (file.exists(renviron_path)) {
  lines <- readLines(renviron_path, warn = FALSE)
  for (line in lines) {
    if (grepl("^R_LIBS_USER", line)) {
      lib_path <- gsub("R_LIBS_USER\\s*=\\s*['\"]?([^'\"]+)['\"]?", "\\1", line)
      lib_path <- Sys.getenv("R_LIBS_USER", unset = lib_path)
      if (nzchar(lib_path) && dir.exists(lib_path)) {
        .libPaths(c(lib_path, .libPaths()))
        log_cat("  Bibliothèque utilisateur ajoutée:", lib_path, "\n")
      }
    }
  }
}

# Afficher les chemins actuels
log_cat("  Chemins de bibliothèques R:\n")
for (p in .libPaths()) {
  log_cat("    -", p, "\n")
}
log_cat("\n")

# ============================================================================
# ÉTAPE 1 : Vérifier/Installer les packages de BASE pour l'app diagnostic
# ============================================================================

# Ces packages sont ESSENTIELS pour que app_diagnostic.R puisse démarrer
# rlang est une dépendance critique de shiny - sans lui, shiny ne peut pas se charger
packages_base <- c("rlang", "shiny", "shinyjs")

log_cat("Vérification des packages de base...\n")

install_base_package <- function(pkg) {
  if (!nzchar(system.file(package = pkg))) {
    log_cat("  Installation de", pkg, "(requis pour le diagnostic)...\n")
    tryCatch({
      install.packages(pkg, quiet = TRUE, dependencies = TRUE)
      if (nzchar(system.file(package = pkg))) {
        log_cat("    ✓", pkg, "installé avec succès\n")
        return(TRUE)
      } else {
        log_cat("    ✗ Échec de l'installation de", pkg, "\n")
        return(FALSE)
      }
    }, error = function(e) {
      log_cat("    ✗ Erreur:", conditionMessage(e), "\n")
      return(FALSE)
    })
  } else {
    return(TRUE)
  }
}

base_ok <- TRUE
for (pkg in packages_base) {
  if (!install_base_package(pkg)) {
    base_ok <- FALSE
  }
}

if (!base_ok) {
  log_cat("\n")
  log_cat("╔══════════════════════════════════════════════════════════════╗\n")
  log_cat("║  ERREUR CRITIQUE : Impossible d'installer les packages de   ║\n")
  log_cat("║  base (rlang, shiny, shinyjs).                              ║\n")
  log_cat("║                                                              ║\n")
  log_cat("║  Veuillez ouvrir R ou RStudio et exécuter :                 ║\n")
  log_cat("║  install.packages(c('rlang', 'shiny', 'shinyjs'))           ║\n")
  log_cat("╚══════════════════════════════════════════════════════════════╝\n")
  log_cat("\nAppuyez sur Entrée pour fermer...")
  invisible(readline())
  close(log_con)
  q(save = "no")
}

log_cat("  ✓ Packages de base OK\n\n")

# ============================================================================
# ÉTAPE 2 : Définir les packages requis par l'application Artemis
# ============================================================================

# Packages CRAN requis par Artemis
# Note: rlang est déjà installé comme package de base (dépendance de shiny)
packages_cran <- c("shinydashboard","shinyWidgets","DT","dplyr", "ggplot2", "plotly","data.table","readxl","sf")

# Packages GitHub requis par SaMARE (tous sur Modelisation-DRF)
packages_github <- c(
  "Artemis2014",
  "BillonnagePetro",
  "ExtractMap",
  "OutilsDRF",
  "BioSIM"
)

# ============================================================================
# ÉTAPE 3 : Fonction de vérification des packages
# ============================================================================

check_package_installed <- function(pkg) {
  # Vérifie si un package est installé SANS le charger
  installed <- nzchar(system.file(package = pkg))
  return(installed)
}

# ============================================================================
# ÉTAPE 4 : Vérification de tous les packages requis
# ============================================================================

log_cat("Vérification des packages de l'application...\n")

packages_manquants_cran <- character(0)
packages_manquants_github <- character(0)

# Vérifier packages CRAN
for (pkg in packages_cran) {
  if (!check_package_installed(pkg)) {
    packages_manquants_cran <- c(packages_manquants_cran, pkg)
    log_cat("  ✗", pkg, "(CRAN) - MANQUANT\n")
  } else {
    log_cat("  ✓", pkg, "\n")
  }
}

# Vérifier packages GitHub
for (pkg in packages_github) {
  if (!check_package_installed(pkg)) {
    packages_manquants_github <- c(packages_manquants_github, pkg)
    log_cat("  ✗", pkg, "(GitHub) - MANQUANT\n")
  } else {
    log_cat("  ✓", pkg, "\n")
  }
}

packages_manquants <- c(packages_manquants_cran, packages_manquants_github)

log_cat("\n")

# ============================================================================
# ÉTAPE 5 : Décision - Lancer app principale ou app diagnostic
# ============================================================================

# Définir le répertoire de l'application
app_dir <- file.path(getwd(), "app")
if (!dir.exists(app_dir)) {
  app_dir <- getwd()
}

if (length(packages_manquants) > 0) {
  # ========================================
  # Des packages sont manquants -> Lancer app_diagnostic.R
  # ========================================
  
  log_cat("╔══════════════════════════════════════════════════════════════╗\n")
  log_cat("║  PACKAGES MANQUANTS DÉTECTÉS                                 ║\n")
  log_cat("╚══════════════════════════════════════════════════════════════╝\n\n")
  
  log_cat("Packages manquants:", paste(packages_manquants, collapse = ", "), "\n\n")
  
  # Sauvegarder l'état pour app_diagnostic.R
  status_file <- file.path(app_dir, "package_status.rds")
  saveRDS(
    list(
      manquants = packages_manquants,
      cran = packages_manquants_cran,
      github = packages_manquants_github,
      timestamp = Sys.time()
    ),
    status_file
  )
  
  # Lancer l'application de diagnostic
  diagnostic_app <- file.path(app_dir, "app_diagnostic.R")
  
  if (file.exists(diagnostic_app)) {
    log_cat("Démarrage de l'application de diagnostic...\n")
    log_cat("(Interface pour installer les packages manquants)\n\n")
    
    tryCatch({
      # Se déplacer dans le dossier app pour que l'app trouve package_status.rds
      old_wd <- getwd()
      setwd(app_dir)
      source("app_diagnostic.R")
      setwd(old_wd)
    }, error = function(e) {
      log_cat("\n")
      log_cat("╔══════════════════════════════════════════════════════════════╗\n")
      log_cat("║  ERREUR lors du lancement de l'app diagnostic               ║\n")
      log_cat("╚══════════════════════════════════════════════════════════════╝\n")
      log_cat("Erreur:", conditionMessage(e), "\n\n")
      log_cat("Installation manuelle requise. Ouvrez R/RStudio et exécutez:\n\n")
      
      if (length(packages_manquants_cran) > 0) {
        log_cat("# Packages CRAN:\n")
        log_cat(paste0('install.packages(c("', paste(packages_manquants_cran, collapse = '", "'), '"))\n\n'))
      }
      
      if (length(packages_manquants_github) > 0) {
        log_cat("# Packages GitHub:\n")
        log_cat('install.packages("remotes")\n')
        for (pkg in packages_manquants_github) {
          log_cat(paste0('remotes::install_github("Modelisation-DRF/', pkg, '")\n'))
        }
      }
      
      log_cat("\nAppuyez sur Entrée pour fermer...")
      invisible(readline())
      close(log_con)
      q(save = "no")
    })
  } else {
    # Pas d'app diagnostic disponible
    log_cat("╔══════════════════════════════════════════════════════════════╗\n")
    log_cat("║  app_diagnostic.R introuvable                               ║\n")
    log_cat("╚══════════════════════════════════════════════════════════════╝\n\n")
    log_cat("Installation manuelle requise. Ouvrez R/RStudio et exécutez:\n\n")
    
    if (length(packages_manquants_cran) > 0) {
      log_cat("# Packages CRAN:\n")
      log_cat(paste0('install.packages(c("', paste(packages_manquants_cran, collapse = '", "'), '"))\n\n'))
    }
    
    if (length(packages_manquants_github) > 0) {
      log_cat("# Packages GitHub:\n")
      log_cat('install.packages("remotes")\n')
      for (pkg in packages_manquants_github) {
        log_cat(paste0('remotes::install_github("Modelisation-DRF/', pkg, '")\n'))
      }
    }
    
    log_cat("\nAppuyez sur Entrée pour fermer...")
    invisible(readline())
    close(log_con)
    q(save = "no")
  }
  
} else {
  # ========================================
  # Tous les packages sont présents -> Lancer app.R
  # ========================================
  
  log_cat("╔══════════════════════════════════════════════════════════════╗\n")
  log_cat("║  ✓ Tous les packages sont installés                         ║\n")
  log_cat("╚══════════════════════════════════════════════════════════════╝\n\n")
  
  main_app <- file.path(app_dir, "app.R")
  
  if (file.exists(main_app)) {
    log_cat("Démarrage de l'application Artemis...\n")
    log_cat("L'application va s'ouvrir dans votre navigateur.\n")
    log_cat(paste0("Le log continuera dans app.R (", log_file, ")\n\n"))
    
    tryCatch({
      # Utiliser runApp avec le chemin du dossier pour que Shiny trouve le dossier www
      shiny::runApp(app_dir, launch.browser = TRUE)
    }, error = function(e) {
      log_cat("\n")
      log_cat("╔══════════════════════════════════════════════════════════════╗\n")
      log_cat("║  ERREUR lors du lancement de l'application                  ║\n")
      log_cat("╚══════════════════════════════════════════════════════════════╝\n")
      log_cat("Erreur:", conditionMessage(e), "\n")
      log_cat("\nAppuyez sur Entrée pour fermer...")
      invisible(readline())
      close(log_con)
      q(save = "no")
    })
  } else {
    log_cat("ERREUR: app.R introuvable dans", app_dir, "\n")
    log_cat("Vérifiez que le fichier app.R existe dans le dossier 'app'.\n")
    log_cat("\nAppuyez sur Entrée pour fermer...")
    invisible(readline())
    close(log_con)
    q(save = "no")
  }
}
