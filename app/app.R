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

# Drapeau global indiquant qu'une réinitialisation (rechargement de la page)
# est en cours. Permet à onSessionEnded de NE PAS quitter R lors d'un reset,
# car la fermeture de la session déclenche normalement q() en mode VBS.
.artemis_reset_en_cours <- FALSE

# Extraction information sur version du package Artemis
version_artemis <- as.character(packageVersion("Artemis2014"))
desc <- packageDescription("Artemis2014")
annee <- format(as.Date(substr(desc$Packaged, 1, 10)), "%Y")

# Version anti-cache pour forcer le rechargement du CSS à chaque démarrage.
# Évite que le navigateur serve une ancienne version mise en cache.
css_version <- as.integer(Sys.time())

# ------------------------------------------------------------------
# Icônes : Material Symbols (bibliothèque officielle SDG) au lieu de
# Font Awesome. On redéfinit icon() pour qu'elle produise un <span>
# Material Symbols. Une table traduit les anciens noms Font Awesome
# vers les ligatures Material Symbols ; ainsi tout le code existant
# (icon("chevron-down"), icon("info-circle"), ...) fonctionne sans
# modification. Les noms inconnus sont utilisés tels quels.
# ------------------------------------------------------------------
icon <- function(name, class = NULL, ...) {
  correspondance <- c(
    "up-right-from-square" = "open_in_new",
    "tree"                 = "park",
    "cloud-sun-rain"       = "partly_cloudy_day",
    "chevron-down"         = "expand_more",
    "sync"                 = "sync",
    "times-circle"         = "cancel",
    "check-circle"         = "check_circle",
    "exclamation-triangle" = "warning",
    "info-circle"          = "info",
    "arrow-right"          = "arrow_forward",
    "check"                = "check",
    "cloud-download-alt"   = "cloud_download",
    "circle-xmark"         = "cancel",
    "play-circle"          = "play_circle",
    "plus-circle"          = "add_circle",
    "trash"                = "delete"
  )

  glyphe <- if (name %in% names(correspondance)) unname(correspondance[name]) else name

  attribs <- list(...)
  # Ajouter aria-hidden seulement s'il n'est pas déjà fourni (évite un doublon)
  if (is.null(attribs[["aria-hidden"]])) {
    attribs[["aria-hidden"]] <- "true"
  }

  classe_finale <- trimws(paste("material-symbols-rounded", if (is.null(class)) "" else class))

  do.call(
    tags$span,
    c(list(glyphe, class = classe_finale), attribs)
  )
}

# ------------------------------------------------------------------
# Pont de classes Bootstrap -> SDG (côté R)
# Permet une migration progressive sans réécrire immédiatement tous
# les appels UI existants.
# ------------------------------------------------------------------
sdg_bridge_classes <- function(class) {
  if (is.null(class) || !nzchar(trimws(class))) return(class)

  tokens <- unlist(strsplit(class, "\\s+"))
  mapping <- c(
    "btn" = "qc-button",
    "btn-primary" = "qc-primary",
    "btn-secondary" = "qc-secondary",
    "btn-danger" = "qc-danger",
    "btn-tertiary" = "qc-tertiary",
    "card" = "sdg-card",
    "card-header" = "sdg-card-header",
    "card-body" = "sdg-card-body",
    "bg-tertiary" = "sdg-card-header-muted",
    "rounded-pill" = "qc-button-rounded",
    "rounded-circle" = "qc-button-rounded"
  )

  mapped <- unname(mapping[tokens])
  mapped <- mapped[!is.na(mapped)]

  # Colonnes Bootstrap -> colonnes SDG
  col_tokens <- grep("^col(-|$)", tokens, value = TRUE)
  col_mapped <- paste0("qc-", col_tokens)

  out <- unique(c(tokens, mapped, col_mapped))
  paste(out, collapse = " ")
}

# Wrappers Shiny : appliquent automatiquement les classes SDG aux boutons.
actionButton <- function(inputId, label, icon = NULL, width = NULL, ..., class = NULL) {
  shiny::actionButton(
    inputId = inputId,
    label = label,
    icon = icon,
    width = width,
    ...,
    class = sdg_bridge_classes(class)
  )
}

downloadButton <- function(outputId, label = "Download", class = NULL, ...) {
  shiny::downloadButton(
    outputId = outputId,
    label = label,
    class = sdg_bridge_classes(class),
    ...
  )
}

# Interface utilisateur
ui <- fluidPage(
  theme = bs_theme(version = 5),
  # Importation du .css ministériel
  tags$head(
    tags$link(rel = "stylesheet", href = paste0("theme-gouvernemental.css?v=", css_version)),
    # Trousse SDG (sans grille) pour migration progressive depuis Bootstrap.
    # On conserve Bootstrap temporairement pour éviter de briser les composants Shiny,
    # puis on remappe les classes vers SDG via le script plus bas.
    tags$link(rel = "stylesheet", href = "dist/css/qc-sdg-no-grid.min.css"),
    tags$script(src = "dist/js/qc-sdg.min.js", defer = NA),

    # Pont de migration Bootstrap -> SDG (appliqué au DOM initial + contenu dynamique Shiny)
    tags$script(HTML("(function () {
      function convertNode(root) {
        if (!root || !root.querySelectorAll) return;

        var mapping = {
          'container': 'qc-container',
          'container-fluid': 'qc-container-fluid',
          'row': 'qc-row',
          'btn': 'qc-button',
          'btn-primary': 'qc-primary',
          'btn-secondary': 'qc-secondary',
          'btn-danger': 'qc-danger',
          'btn-tertiary': 'qc-tertiary',
          'card': 'sdg-card',
          'card-header': 'sdg-card-header',
          'card-body': 'sdg-card-body',
          'bg-tertiary': 'sdg-card-header-muted',
          'rounded-pill': 'qc-button-rounded',
          'rounded-circle': 'qc-button-rounded'
        };

        function mapClassList(el) {
          if (!el.classList) return;

          Object.keys(mapping).forEach(function (from) {
            if (el.classList.contains(from)) {
              el.classList.add(mapping[from]);
            }
          });

          // Colonnes Bootstrap -> colonnes SDG (col-*, col-sm-*, col-md-* ...)
          Array.from(el.classList).forEach(function (c) {
            if (/^col(-|$)/.test(c)) {
              el.classList.add('qc-' + c);
            }
          });
        }

        mapClassList(root);
        root.querySelectorAll('*').forEach(mapClassList);
      }

      function bootBridge() {
        convertNode(document.body);

        var observer = new MutationObserver(function (mutations) {
          mutations.forEach(function (m) {
            m.addedNodes.forEach(function (node) {
              if (node.nodeType === 1) convertNode(node);
            });
          });
        });

        observer.observe(document.body, { childList: true, subtree: true });
      }

      if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', bootBridge);
      } else {
        bootBridge();
      }
    })();")),

    # Chargement des polices gouvernementales (Roboto + Open Sans) depuis www/fonts.
    # Chemin relatif servi par Shiny : portable et fonctionne hors-ligne.
    tags$style(HTML("\
      /* Roboto (titres) */
      @font-face {
        font-family: 'Roboto';
        font-style: normal;
        font-weight: 400;
        font-display: swap;
        src: url('fonts/roboto-v48-latin-regular.woff2') format('woff2');
      }
      @font-face {
        font-family: 'Roboto';
        font-style: normal;
        font-weight: 500;
        font-display: swap;
        src: url('fonts/roboto-v48-latin-500.woff2') format('woff2');
      }
      @font-face {
        font-family: 'Roboto';
        font-style: normal;
        font-weight: 600;
        font-display: swap;
        src: url('fonts/roboto-v48-latin-600.woff2') format('woff2');
      }
      @font-face {
        font-family: 'Roboto';
        font-style: normal;
        font-weight: 700;
        font-display: swap;
        src: url('fonts/roboto-v48-latin-700.woff2') format('woff2');
      }
      /* Open Sans (texte courant) */
      @font-face {
        font-family: 'Open Sans';
        font-style: normal;
        font-weight: 400;
        font-display: swap;
        src: url('fonts/open-sans-v43-latin-regular.woff2') format('woff2');
      }
      @font-face {
        font-family: 'Open Sans';
        font-style: normal;
        font-weight: 600;
        font-display: swap;
        src: url('fonts/open-sans-v43-latin-600.woff2') format('woff2');
      }
      @font-face {
        font-family: 'Open Sans';
        font-style: normal;
        font-weight: 700;
        font-display: swap;
        src: url('fonts/open-sans-v43-latin-700.woff2') format('woff2');
      }
      /* Material Symbols Rounded (icônes officielles SDG) */
      @font-face {
        font-family: 'Material Symbols Rounded';
        font-style: normal;
        font-weight: 100 700;
        font-display: block;
        src: url('fonts/material-symbols-rounded-full.woff2') format('woff2');
      }
      .material-symbols-rounded {
        font-family: 'Material Symbols Rounded';
        font-weight: normal;
        font-style: normal;
        font-size: 16px;
        line-height: 1;
        letter-spacing: normal;
        text-transform: none;
        display: inline-block;
        white-space: nowrap;
        word-wrap: normal;
        direction: ltr;
        vertical-align: -0.125em;
        -webkit-font-smoothing: antialiased;
        font-feature-settings: 'liga';
        font-variation-settings: 'opsz' 40, 'FILL' 0, 'GRAD' 0;
      }
      .material-symbols-rounded.fa-3x { font-size: 48px; }
      .material-symbols-rounded.fa-2x { font-size: 32px; }
    ")),

    # Habillage SDG des conteneurs Bootstrap encore présents (migration progressive)
    tags$style(HTML("\
  .sdg-card {
        border: 1px solid var(--qc-color-grey-light);
        background-color: var(--qc-color-white);
        box-shadow: 0 var(--qc-box_shadow-1-offset) var(--qc-box_shadow-1-blur) var(--qc-color-box_shadow);
        border-radius: 0;
      }
  .sdg-card-header {
        font-size: var(--qc-font-size-md);
        line-height: var(--qc-line-height-md);
        font-weight: var(--qc-font-weight-bold);
        color: var(--qc-color-text-primary);
        background-color: var(--qc-color-blue-pale);
        border-bottom: 1px solid var(--qc-color-grey-light);
        padding: var(--qc-spacer-sm);
      }
      .sdg-card-header-muted {
        background-color: var(--qc-color-blue-pale) !important;
      }
  .sdg-card-body {
        padding: var(--qc-spacer-sm);
      }
      .sdg-card-header [data-sdg-toggle='collapse'] {
        color: var(--qc-color-blue-piv);
        text-decoration: none;
      }
      .sdg-card-header [data-sdg-toggle='collapse']:hover {
        color: var(--qc-color-blue-regular);
      }
    ")),

    # Collapse des sections (sans dépendance JS Bootstrap)
    tags$script(HTML(paste(
      "(function(){",
      "  function getTargetElement(trigger) {",
      "    if (!trigger) return null;",
      "    var selector = trigger.getAttribute('data-sdg-target') || trigger.getAttribute('href');",
      "    if (!selector || selector.charAt(0) !== '#') return null;",
      "    return document.querySelector(selector);",
      "  }",
      "",
      "  function syncTriggersForPanel(panel, isOpen) {",
      "    if (!panel || !panel.id) return;",
      "    var selectors = [",
      "      '[data-sdg-toggle=\"collapse\"][href=\"#' + panel.id + '\"]',",
      "      '[data-sdg-toggle=\"collapse\"][data-sdg-target=\"#' + panel.id + '\"]'",
      "    ];",
      "",
      "    document.querySelectorAll(selectors.join(',')).forEach(function(trigger) {",
      "      trigger.setAttribute('aria-expanded', isOpen ? 'true' : 'false');",
      "    });",
      "  }",
      "",
      "  function applyCollapseState(panel, open) {",
      "    if (!panel) return;",
      "    panel.classList.toggle('show', !!open);",
      "    panel.setAttribute('aria-hidden', open ? 'false' : 'true');",
      "    syncTriggersForPanel(panel, !!open);",
      "  }",
      "",
      "  function setCollapseState(id, open) {",
      "    applyCollapseState(document.getElementById(id), open);",
      "  }",
      "",
      "  document.addEventListener('click', function(e) {",
      "    var trigger = e.target.closest('[data-sdg-toggle=\"collapse\"]');",
      "    if (!trigger) return;",
      "",
      "    var panel = getTargetElement(trigger);",
      "    if (!panel) return;",
      "",
      "    e.preventDefault();",
      "    applyCollapseState(panel, !panel.classList.contains('show'));",
      "  });",
      "",
      "  function initCollapseA11y() {",
      "    document.querySelectorAll('.collapse[id]').forEach(function(panel) {",
      "      applyCollapseState(panel, panel.classList.contains('show'));",
      "    });",
      "  }",
      "",
      "  if (document.readyState === 'loading') {",
      "    document.addEventListener('DOMContentLoaded', initCollapseA11y);",
      "  } else {",
      "    initCollapseA11y();",
      "  }",
      "",
      "  Shiny.addCustomMessageHandler('collapse_import_close', function() {",
      "    setCollapseState('collapse_import', false);",
      "  });",
      "",
      "  Shiny.addCustomMessageHandler('collapse_import_open', function() {",
      "    setCollapseState('collapse_import', true);",
      "  });",
      "})();",
      sep = "\n"
    ))),

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

    # Désactivation du module de mortalité si maladie corticale du hêtre = Oui
    tags$script(HTML("
      Shiny.addCustomMessageHandler('toggle_mortalite', function(msg){
      $('#module_mortalite').prop('disabled', msg.disable === true);
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
  width: 100%; }")),

    # Shim utilitaires Bootstrap -> rendu SDG (migration progressive)
    tags$style(HTML("\
  .text-start { text-align: left !important; }
  .text-end { text-align: right !important; }
    .text-center { text-align: center !important; }
    .text-white { color: var(--qc-color-white) !important; }
    .text-dark { color: var(--qc-color-text-primary) !important; }
  .text-body { color: var(--qc-color-text-primary) !important; }
    .text-primary { color: var(--qc-color-blue-piv) !important; }
  .text-warning { color: var(--qc-color-orange-regular) !important; }
  .text-danger { color: var(--qc-color-red-regular) !important; }
    .text-muted { color: var(--qc-color-grey-medium) !important; }
    .small { font-size: var(--qc-font-size-sm) !important; line-height: var(--qc-line-height-sm) !important; }

    .fw-bold { font-weight: var(--qc-font-weight-bold) !important; }
  .fw-semibold { font-weight: var(--qc-font-weight-bold) !important; }
    .fst-italic { font-style: italic !important; }

    .fs-2 { font-size: var(--qc-font-size-lg) !important; line-height: var(--qc-line-height-lg) !important; }
    .fs-3 { font-size: var(--qc-font-size-xl) !important; line-height: var(--qc-line-height-xl) !important; }
    .fs-5 { font-size: var(--qc-font-size-md) !important; line-height: var(--qc-line-height-md) !important; }

    .d-flex { display: flex !important; }
    .align-items-center { align-items: center !important; }
    .justify-content-center { justify-content: center !important; }
    .justify-content-between { justify-content: space-between !important; }
    .justify-content-end { justify-content: flex-end !important; }
    .gap-2 { gap: var(--qc-spacer-sm) !important; }
    .gap-3 { gap: var(--qc-spacer-md) !important; }

    .w-100 { width: 100% !important; }
    .h-100 { height: 100% !important; }
    .overflow-auto { overflow: auto !important; }
  .position-relative { position: relative !important; }

    .mt-0 { margin-top: 0 !important; }
    .mt-1 { margin-top: calc(var(--qc-spacer-xs) / 2) !important; }
    .mt-2 { margin-top: var(--qc-spacer-sm) !important; }
    .mt-3 { margin-top: var(--qc-spacer-md) !important; }
    .mt-4 { margin-top: var(--qc-spacer-lg) !important; }
    .mt-5 { margin-top: var(--qc-spacer-xl) !important; }

    .mb-0 { margin-bottom: 0 !important; }
    .mb-1 { margin-bottom: calc(var(--qc-spacer-xs) / 2) !important; }
    .mb-2 { margin-bottom: var(--qc-spacer-sm) !important; }
    .mb-3 { margin-bottom: var(--qc-spacer-md) !important; }
    .mb-4 { margin-bottom: var(--qc-spacer-lg) !important; }

    .ms-1 { margin-left: calc(var(--qc-spacer-xs) / 2) !important; }
    .ms-2 { margin-left: var(--qc-spacer-sm) !important; }
    .ms-3 { margin-left: var(--qc-spacer-md) !important; }
    .me-2 { margin-right: var(--qc-spacer-sm) !important; }

  .p-1 { padding: calc(var(--qc-spacer-xs) / 2) !important; }
  .p-2 { padding: var(--qc-spacer-sm) !important; }
  .p-4 { padding: var(--qc-spacer-lg) !important; }
  .ps-3 { padding-left: var(--qc-spacer-md) !important; }
  .pe-2 { padding-right: var(--qc-spacer-sm) !important; }
  .pe-3 { padding-right: var(--qc-spacer-md) !important; }
    .pt-2 { padding-top: var(--qc-spacer-sm) !important; }
    .pt-3 { padding-top: var(--qc-spacer-md) !important; }
    .pb-0 { padding-bottom: 0 !important; }
    .py-0 { padding-top: 0 !important; padding-bottom: 0 !important; }
    .py-3 { padding-top: var(--qc-spacer-md) !important; padding-bottom: var(--qc-spacer-md) !important; }
    .px-0 { padding-left: 0 !important; padding-right: 0 !important; }
  .px-2 { padding-left: var(--qc-spacer-sm) !important; padding-right: var(--qc-spacer-sm) !important; }
  .px-3 { padding-left: var(--qc-spacer-md) !important; padding-right: var(--qc-spacer-md) !important; }
    .px-4 { padding-left: var(--qc-spacer-lg) !important; padding-right: var(--qc-spacer-lg) !important; }

  .border { border: 1px solid var(--qc-color-grey-light) !important; }
    .border-top { border-top: 1px solid var(--qc-color-grey-light) !important; }
    .border-bottom { border-bottom: 1px solid var(--qc-color-grey-light) !important; }
    .border-start { border-left: 1px solid var(--qc-color-grey-light) !important; }
    .border-4 { border-width: 4px !important; }
    .border-primary { border-color: var(--qc-color-blue-piv) !important; }

    .bg-light { background-color: var(--qc-color-grey-pale) !important; }
    .rounded { border-radius: 0 !important; }

    .visually-hidden {
      position: absolute !important;
      width: 1px !important;
      height: 1px !important;
      padding: 0 !important;
      margin: -1px !important;
      overflow: hidden !important;
      clip: rect(0, 0, 0, 0) !important;
      white-space: nowrap !important;
      border: 0 !important;
    }

    .sdg-two-cols {
      display: flex;
      gap: var(--qc-spacer-md);
      align-items: stretch;
      flex-wrap: wrap;
      width: 100%;
      max-inline-size: var(--qc-max-content-width);
    }

    .sdg-two-cols > .sdg-two-col {
      flex: 1 1 calc((100% - var(--qc-spacer-md)) / 2);
      max-width: calc((100% - var(--qc-spacer-md)) / 2);
      min-width: 0;
      box-sizing: border-box;
    }

    /* Mise en page 1/3 - 2/3 pour l'onglet Données (sans dépendre de la grille SDG) */
    .sdg-data-layout {
      display: grid;
      grid-template-columns: minmax(280px, 1fr) minmax(0, 2fr);
      gap: var(--qc-spacer-sm);
      align-items: flex-start;
      width: 100%;
    }

    .sdg-data-layout-left {
      box-sizing: border-box;
    }

    .sdg-data-layout-right {
      min-width: 0;
      box-sizing: border-box;
    }

    /* FileInput SDG (import principal) : bouton Parcourir seulement,
       nom du fichier + Supprimer restent affichés en dessous */
    .sdg-fileinput .input-group > input.form-control,
    .sdg-fileinput .input-group > input[type='text'] {
      display: none !important;
    }

    .sdg-fileinput .input-group {
      width: auto;
      display: inline-flex;
    }

     /* Le conteneur a une classe p-2: on neutralise surtout le bas */
    .sdg-fileinput {
      padding-bottom: 0 !important;
      margin-bottom: 0 !important;
    }


    /* Réduire l'espace sous le contrôle fileInput */
    .sdg-fileinput .form-group,
    .sdg-fileinput .shiny-input-container {
      margin-bottom: 0 !important;
    }

    /* Espace vertical entre le libellé et le bouton*/
    .sdg-fileinput .control-label {
      margin-bottom: var(--qc-spacer-xs) !important;
      display: block;
    }

    /* Masquer la barre (incluant la barre grise),
       tout en conservant la liste du fichier + lien Supprimer */
    .sdg-fileinput .shiny-file-input-progress.progress,
    .sdg-fileinput .shiny-file-input-progress .progress,
    .sdg-fileinput .shiny-file-input-progress .progress-bar,
    .sdg-fileinput .shiny-file-input-progress .bar {
      display: none !important;
      height: 0 !important;
      margin: 0 !important;
      padding: 0 !important;
      border: 0 !important;
      box-shadow: none !important;
      background: transparent !important;
    }

    .sdg-fileinput .shiny-file-input-progress .progress-text {
      display: none !important;
    }

    /* Rapprocher la ligne du bouton */
    .sdg-fileinput .shiny-file-input-progress {
      margin-top: 0 !important;
      margin-bottom: 0 !important;
      padding-top: 0 !important;
      padding-bottom: 0 !important;
    }

    /* Ligne du nom de fichier sous le bouton Parcourir */
    .sdg-file-name-row {
      padding: 0 0 12px 0 !important;
      margin: 4px var(--qc-spacer-sm) 2px var(--qc-spacer-sm) !important;
      border-bottom: 1px solid var(--qc-color-grey-light);
      line-height: 1.05;
    }

    .sdg-file-name-row > div {
      margin: 0 !important;
      line-height: 1.1;
    }

    .sdg-fileinput .input-group-btn .btn,
    .sdg-fileinput .input-group .btn,
    .sdg-fileinput .btn-file {
      border-radius: 0 !important;
      border: 1px solid var(--qc-color-blue-piv) !important;
      background-color: var(--qc-color-white) !important;
      color: var(--qc-color-blue-piv) !important;
      font-size: var(--qc-font-size-sm);
      line-height: var(--qc-line-height-sm);
      padding: 6px 12px !important;
      min-height: 32px;
      display: inline-flex;
      align-items: center;
      box-shadow: none !important;
    }

    .sdg-fileinput .input-group-btn .btn:hover,
    .sdg-fileinput .input-group .btn:hover,
    .sdg-fileinput .btn-file:hover {
      background-color: var(--qc-color-blue-pale) !important;
      color: var(--qc-color-blue-regular) !important;
    }

    /* Blocs de validation (base + variantes) */
    .sdg-validation {
      position: relative;
      margin: var(--qc-spacer-sm);
      padding: var(--qc-spacer-sm) var(--qc-spacer-sm) var(--qc-spacer-sm) calc(var(--qc-spacer-sm) + 52px);
      border: 1px solid var(--qc-color-grey-light);
      background-color: var(--qc-color-white);
      display: flex;
      align-items: flex-start;
      gap: var(--qc-spacer-sm);
      overflow: hidden;
    }

    .sdg-validation > * {
      position: relative;
      z-index: 2;
    }

    .sdg-validation::before {
      content: '';
      position: absolute;
      left: 0;
      top: 0;
      bottom: 0;
      width: 48px;
      z-index: 0;
    }

    .sdg-validation-success::before {
      background-color: var(--qc-color-green-pale);
    }

    .sdg-validation-success::after {
      content: 'check_circle';
      position: absolute;
      left: 12px;
      top: 50%;
      transform: translateY(-50%);
      font-family: 'Material Symbols Rounded';
      font-size: 24px;
      line-height: 1;
      color: var(--qc-color-green-dark);
      font-variation-settings: 'opsz' 40, 'FILL' 0, 'GRAD' 0;
      z-index: 1;
      pointer-events: none;
    }

    .sdg-validation-info::before {
      background-color: var(--qc-color-blue-pale);
    }

    .sdg-validation-info::after {
      content: 'info';
      position: absolute;
      left: 12px;
      top: 50%;
      transform: translateY(-50%);
      font-family: 'Material Symbols Rounded';
      font-size: 24px;
      line-height: 1;
      color: var(--qc-color-blue-piv);
      font-variation-settings: 'opsz' 40, 'FILL' 0, 'GRAD' 0;
      z-index: 1;
      pointer-events: none;
    }

    .sdg-validation-error::before {
      background-color: var(--qc-color-red-pale);
    }

    .sdg-validation-error::after {
      content: 'cancel';
      position: absolute;
      left: 12px;
      top: 50%;
      transform: translateY(-50%);
      font-family: 'Material Symbols Rounded';
      font-size: 24px;
      line-height: 1;
      color: var(--qc-color-red-regular);
      font-variation-settings: 'opsz' 40, 'FILL' 0, 'GRAD' 0;
      z-index: 1;
      pointer-events: none;
    }

    .sdg-list-tight,
    .sdg-list-tight.small {
      margin-top: 2px !important;
      margin-bottom: 0 !important;
      padding-top: 0 !important;
      padding-bottom: 0 !important;
      line-height: 1.05 !important;
    }

    .sdg-list-tight li {
      margin: 0 !important;
      padding: 0 !important;
      line-height: 1.3 !important;
    }

    /* Radio options normales (ni primary ni bold) */
    .sdg-radio-normal .shiny-options-group label,
    .sdg-radio-normal .form-check-label,
    .sdg-radio-normal .radio label {
      color: var(--qc-color-text-primary) !important;
      font-weight: var(--qc-font-weight-regular) !important;
      font-size: var(--qc-font-size-sm) !important;
      line-height: var(--qc-line-height-sm) !important;
    }

    .sdg-radio-normal .shiny-options-group .radio {
      margin-top: 0 !important;
      margin-bottom: 2px !important;
    }

    @media (max-width: 768px) {
      .sdg-two-cols {
        flex-direction: column;
      }

      .sdg-two-cols > .sdg-two-col {
        flex: 1 1 100%;
        max-width: 100%;
      }

      .sdg-data-layout {
        grid-template-columns: 1fr;
      }

      .sdg-data-layout-left,
      .sdg-data-layout-right {
        min-width: 0;
      }
    }
    ")),

  ),

  # Solution temporaire pour faire fonctionner les renderUI des inputs (important de laisser ça là)
  div(style = "display:none;",
      selectInput("dummy_hidden", NULL, choices = "")
  ),

# ------------------------ Visuel ----------------------
  # Header gouvernemental
  tags$header(
    div(
  class = "qc-container-fluid piv py-3",
      role = "banner",
      #style = "font-family: var(--qc-font-family-header);",

      div(
        class = "app-container",
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
    )
  ),
  # Menu de navigation
    div(
      class = "headline pt-3",
      div(class = "app-container",

          div(class = "d-flex align-items-center",
              uiOutput("nav_menu")
          )
      )
    )
  ,

  # Contenu de la page
  div(
    class = "app-container mt-5",
    # Gérer coté server
    uiOutput("main_content")
  ),

  # Footer
  div(
    class = "app-container text-center py-3",

    # Info version Artemis- Au dessus de la ligne grise
    div(class = "border-top pt-2 d-flex justify-content-center gap-3 mb-1",
        p(paste("Artemis ©", annee), class = "text-muted small mb-0"),
        p(paste("Version", version_artemis), class = "text-muted small mb-0")
    ),

    # Ligne grise + reste du footer
    div(class = "pt-2",

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
                  class = "qc-color-link-text small"
                )
            )
        )
    )
  )

)



# ------------------------ Serveur ----------------------------------
server <- function(input, output, session) {

  session$onSessionEnded(function() {
    # Nettoyage mémoire
    gc()
    message("Session fermée.")

    # Si une réinitialisation est en cours, la page va se recharger :
    # on ne quitte PAS R, on réarme simplement le drapeau.
    if (.artemis_reset_en_cours) {
      .artemis_reset_en_cours <<- FALSE
      message("Réinitialisation : R reste actif, la page se recharge.")
      return(invisible(NULL))
    }

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
        # Onglet Résultat : toujours visible pour garder des repères stables.
        # Grisé et non cliquable tant qu'aucune simulation n'est faite.
        if (isTRUE(rv$simulation_terminee)) {
          actionLink(
            "tab_resultat", "Résultat",
            class = "text-white px-2 fs-2",
            style = paste0(
              "text-decoration:none; padding-bottom:10px;",
              if (current == "resultat")
                "border-bottom:4px solid lightgray;" else ""
            )
          )
        } else {
          tags$span(
            "Résultat",
            class = "px-2 fs-2 nav-onglet-desactive",
            title = "Lancez une simulation dans l'onglet Données pour accéder aux résultats.",
            `aria-disabled` = "true"
          )
        }
    )
  })

  # Chargement dynamique du contenu des pages
  output$main_content <- renderUI({
    # Page Accueil
    if (current_tab() == "accueil") {
      div(
        class = "texte-courant",
        div(
          h1("Accueil"),
          p("Bienvenue dans l'application Artemis, un Simulateur de croissance à l'échelle de l'arbre pour les forêts du Québec. Cette application vous permet de réaliser des simulations basées sur vos données d'inventaire forestier.")
        ),

        div(
          class = "mt-4",
          h2("Documentation", class = "mb-3"),
            p("Pour vous aider à utiliser efficacement Artemis veuillez consulter la page Wiki de l'application: ",
            tags$a(
              "Aide application R Artémis",
              icon("up-right-from-square", class = "ms-1 small", `aria-hidden` = "true"),
              tags$span("(Ouvre dans un nouvel onglet)", class = "visually-hidden"),
              href = "https://github.com/Modelisation-DRF/Artemis2014_Shiny/wiki",
              target = "_blank",
              rel = "noopener noreferrer",
              class = "fw-bold"
            )),

            p("Cette page contient des instructions détaillées sur la préparation des données et la configuration des simulations.",
              class = "text-muted fst-italic"
            )
          ),

        # Fichiers d'exemple
        tags$div(
          class = "mb-4",
          tags$h2("Fichiers d'exemple", class = "mb-3"),
          tags$p("Pour vous familiariser avec la structure des fichiers d'Artemis, vous pouvez télécharger ces exemples:"),

          tags$div(class = "sdg-two-cols mt-4",

            # Données arbres
            tags$div(
              class = "sdg-two-col",
              tags$div(
                class = "p-4 qc-shading-1",
                tags$div(class = "mb-3", icon("tree",class = "text-primary")),
                tags$h4("Données des arbres", class = "mb-3"),
                tags$p("Exemple de fichier CSV avec les données des arbres nécessaires pour la simulation.", class = ""),
                tags$div(
                  class = " mt-3",
                  downloadButton("download_arbres", "Télécharger", class = "qc-button qc-secondary")
                )
              )
            ),

            # Données climatiques
            tags$div(
              class = "sdg-two-col",
              tags$div(
                class = "p-4 rounded h-100 qc-shading-1",
                tags$div(class = " mb-3", icon("cloud-sun-rain", class = "text-primary")),
                tags$h4("Données climatiques", class = " mb-3"),
                tags$p("Exemples de fichiers CSV contenant les données climatiques pour les simulations.", class = ""),
                tags$div(
                  class = " mt-3",
                  downloadButton("download_climat", "Télécharger", class = "qc-button qc-secondary")
                )
              )
            )
          )
        ),
        # Section contact
        div(
          class = "mt-4",
          h2(
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
      div(
      div(
        h1("Données")),

      div(class = "qc-container-fluid px-0 mt-2",
        div(class = "sdg-data-layout",

            # Colonne de gauche (1/3)
            div(class = "sdg-data-layout-left",

                # Importations des données
                div(class = "sdg-card mb-3",
                    div(
                      class = "sdg-card-header sdg-card-header-muted bg-tertiary d-flex justify-content-between align-items-center",
                      span("Importation de données"),

                      tags$a(
                        href = "#collapse_import",
                        `data-sdg-toggle` = "collapse",
                        role = "button",
                        icon("chevron-down",class="text-dark")
                      )
                    ),

                    div(
                      id = "collapse_import",
                      class = "collapse show",

                      div(
                        class = "p-2 sdg-fileinput",

                        fileInput(
                          "file",
                          "Choisir un fichier CSV",
                          buttonLabel = "Parcourir",
                          placeholder = ""
                        )
                      )
                     ,
                      uiOutput("validation_status"),
                      uiOutput("error_box"),
                     uiOutput("error_box_sim"),
                      uiOutput("Avertissement_box"),
                      uiOutput("Info_box"),
                      uiOutput("extraction_question"),
                      uiOutput("extraction_button")
                    )

                ),

                # Configuration
                div(class = "sdg-card",
                    div(
                      class = "sdg-card-header sdg-card-header-muted bg-tertiary d-flex justify-content-between align-items-center",
                      span("Configuration de la simulation"),
                      tags$a(
                        href = "#collapse_config",
                        `data-sdg-toggle` = "collapse",
                        role = "button",
                        icon("chevron-down",class="text-dark")
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
            div(class = "sdg-data-layout-right",
                # Données importées
                div(class = "sdg-card",
                    div(
                      class = "sdg-card-header sdg-card-header-muted bg-tertiary d-flex justify-content-between align-items-center",
                      span("Données importées"),
                      tags$a(
                        href = "#collapse_data",
                        `data-sdg-toggle` = "collapse",
                        role = "button",
                        icon("chevron-down",class="text-dark")
                      )
                    ),

                    div(
                      id = "collapse_data",
                      class = "collapse show",

                      div(class = "sdg-card-body py-0 fs-5",
                          DTOutput("contents")
                      )
                    )
                )
            )),

            # Bouton de réinitialisation
            div(
              class = "d-flex justify-content-end mt-2",

              actionButton(
                "reset_button",
                "Réinitialiser",
                class = "qc-button qc-danger",
                icon = icon("sync")
              )
            )
        #)
      )
)
    }
    else if (current_tab() == "resultat") {
      div(
        div(
          h1("Résultat")),

    div(class = "qc-container-fluid px-0 mt-2",
      div(class = "qc-row g-2",

      # Colonne de gauche (1/3)
  div(class = "qc-col-md-4",

          # Options de visualisation
          div(class = "sdg-card mb-3",
              div(
                class = "sdg-card-header sdg-card-header-muted bg-tertiary d-flex justify-content-between align-items-center",
                span("Options de visualisation"),

                tags$a(
                  href = "#collapse_opt_vis",
                  `data-sdg-toggle` = "collapse",
                  role = "button",
                  icon("chevron-down",class="text-dark")
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
              div(class = "sdg-card",
                  div(
                    class = "sdg-card-header sdg-card-header-muted bg-tertiary d-flex justify-content-between align-items-center",
                    span("Exportation des résultats"),
                    tags$a(
                      href = "#collapse_exp_res",
                      `data-sdg-toggle` = "collapse",
                      role = "button",
                      icon("chevron-down",class="text-dark")
                    )
                  ),

                  div(
                    id = "collapse_exp_res",
                    class = "collapse show",
                    div(class = "mt-2 ms-2",

                        div(class = "fw-bold text-body mb-1",
                            "Toutes les années de simulation"
                        ),

                        div(class = "pe-2 small",
                            radioButtons(
                              "simplifier",
                              label = NULL,
                              choices = list("Oui" = FALSE, "Non" = TRUE),
                              selected = FALSE,
                              inline = TRUE
                            )
                        ),

                        div(class = "fw-bold text-body mb-1",
                            "Choix de la sortie"
                        ),

                        div(class = "pe-2",
                            selectInput(
                              "Sortie",
                              label = NULL,
                              choices = c(
                                "-- Sélectionner une option --" = "",
                                "Arbre" = "arbre",
                                "Placette" = "placette",
                                "À l'échelle du billon" = "echelle_billon"
                              ),
                              selected = "arbre"
                            )
                        ),
                        # Conditional panel si "À l'échelle du billon"
                        conditionalPanel(
                          condition = "input.Sortie == 'echelle_billon'",

                          # Feuillus durs
                          div(class = "mt-2",
                              div(class = "fw-bold text-body mb-1",
                                  "Billonnage Feuillus durs (Pétro)"
                              ),

                              div(class = "pe-2",
                                  selectInput(
                                    "typeBillonnage",
                                    label = NULL,
                                    choices = list(
                                      "DHP_Régionalisé" = "DHP",
                                      "DHP_Provincial" = "DHP2015"
                                    ))
                              )
                          ),
                          # Résineux
                          div(class = "mt-2",
                              div(class = "fw-bold text-body mb-1",
                                  "Billonnage Résineux"
                              ),
                              div(class = "pe-2",
                                  numericInput(
                                    "dhs_input",
                                    label = "DHS (Diamètre à hauteur de souche) :",
                                    value = 0.15,
                                    min = 0.01,
                                    max = 1.0,
                                    step = 0.01
                                  )
                              )),
                          div(class = "border rounded p-1 bg-light text-start mt-2 me-2 mb-2 ",

                              div(class = "fw-bold text-body mb-0",
                                  "Grade 1"
                              ),

                              textInput(
                                "nom_grade1",
                                "Nom du grade 1:",
                                value = "sciage court"
                              ),

                              selectInput(
                                "long_grade1",
                                "Longueur (pieds):",
                                choices = c("Indéfini", "4", "8", "12"),
                                selected = "8"
                              ),

                              numericInput(
                                "diam_grade1",
                                "Diamètre au fin bout (cm):",
                                value = 20,
                                min = 0,
                                max = 100,
                                step = 0.1
                              )
                          )
                        ,
                        # Section sur les Grades
                        uiOutput("add_grade2_button"),
                        uiOutput("grade2_section"),
                        uiOutput("add_grade3_button"),
                        uiOutput("grade3_section")),

                        div(class = "mt-3 me-2 mb-2",

                            downloadButton(
                              "download_resultats_custom",
                              "Télécharger les résultats",
                              class = "qc-button qc-primary w-100"
                            )
                        ),

                        div(
                          class = "mt-2 mb-2 me-2 small text-muted fst-italic",
                          "Si \"Non\" est sélectionné, seuls les résultats de la première et de la dernière année de la simulation seront exportés."
                        )
                    )
                  )
              )),

              # colonne de droite (2/3)
              div(class = "qc-col-md-8",
                  # Visualisation des résultats
                  div(class = "sdg-card",
                      div(
                        class = "sdg-card-header sdg-card-header-muted bg-tertiary d-flex justify-content-between align-items-center",
                        span("Visualisation des résultats"),
                        tags$a(
                          href = "#collapse_resultat",
                          `data-sdg-toggle` = "collapse",
                          role = "button",
                          icon("chevron-down",class="text-dark")
                        )
                      ),

                      div(
                        id = "collapse_resultat",
                        class = "collapse show",

                        div(class = "sdg-card-body py-0 fs-5",
                            plotOutput("resultat_graphique", height = "600px")
                        )
                      )
                  )
              ) ,

              # Bouton de réinitialisation
              div(
                class = "d-flex justify-content-end mt-2",

                actionButton(
                  "reset_button",
                  "Réinitialiser",
                  class = "qc-button qc-danger",
                  icon = icon("sync")
                )
              )

            )))

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

  output$download_climat<- downloadHandler(
    filename = function() {
      "Donnees_Clim_Exemple_Artemis.csv"
    },
    content = function(file) {
      file.copy("data/ClimTot_Exemple.csv", file)
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
    climat_tot = NULL,
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
    rv$climat_tot <- NULL
    rv$max_annees_simulation <- NA
    rv$simulation_terminee <- FALSE

    # Vider les sorties précédentes
    output$extraction_question <- renderUI({})
    output$extraction_button <- renderUI({})
    output$simulation_message <- renderUI({})
    output$error_box_sim <- renderUI({})

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

    div(class = "",
        div(class = "d-flex align-items-center sdg-file-name-row",
            icon("check-circle", class = "me-2"),
            div(input$file$name)),


        # Si erreur
        if (length(errors) > 0) {
          div(class = "sdg-validation sdg-validation-error",
            div(
              tags$strong("Validation échouée"),
              tags$div(paste(length(errors), "erreur(s) détectée(s)"))
            )
          )
        }

        # Si succès
        else {
          tagList(
            div(
              class = "sdg-validation sdg-validation-success",
              div(
                tags$div(tags$strong("Validation réussie:")),
                tags$div("Les données sont valides")
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
  div( class = "sdg-validation sdg-validation-error",
       div(
           tags$strong("Erreurs détectées"),
        tags$ul( class = "small ps-3 mb-0 sdg-list-tight",
          lapply(errors, function(error) {
            tags$li(style = "margin-bottom: 1px; padding: 0;", error)
          })
        )
      ))
    }
    else {NULL}
  })

  # Afficher les avertissements
  output$Avertissement_box <- renderUI({
      # Avertissement pour âge moyen
      if (!rv$age_moy_valid) {
  div(class = "qc-notice qc-warning alert alert-warning d-flex align-items-center",
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
  div( class = "sdg-validation sdg-validation-info",
      div(
        tags$strong("Champs optionnels absents :"),
        tags$ul(class = "small ps-3 mb-0 sdg-list-tight",
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
            class = "ms-2 me-2 form-section",
            tags$label("Données climatiques", class = "control-label text-secondary")
          ),

          # Section Radio-buttons
          div(
            class = "sdg-card-body sdg-radio-normal pt-1 pb-0 mt-0 pe-3",

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
              actionButton(
                "validate_extraction_choice",
                label = tagList( "Suivant",
                  icon("arrow-right", class = "ms-1")),
                class = "qc-button qc-primary mb-2 mt-2"
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
          class = "p-2",


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

          div(class = "fw-bold mb-1 text-secondary", "Nombre d'années de simulation (multiple de 10)"),

          # Choisir l'horizon
          div(class = "mb-2",
              numericInput(
                "horizon",NULL,
                value = 10,
                min = 10,
                step = 10
              )
          ),

          div(class = "fw-bold mb-1 text-secondary", "Scénario CMIP6"),

          # Choisir le scénario
          div(class = "mb-2 sdg-radio-normal",
              radioButtons(
                "ssp",NULL,
                choices = list("SSP245" = "SSP245", "SSP370" = "SSP370"),
                selected = "SSP245"
              )
          ),

          # Bouton d'extraction
          div(
            class = "mt-2 mb-0",
            uiOutput("extraction_button_final")
          )
        )
      })
    }

    # Fournir les données climatiques
    else if (input$extraction_choice == "upload") {

      output$extraction_button <- renderUI({

        div(
          class = "mt-3",

          # Titre
          div( class = "fw-bold text-primary mb-2 ms-2",
            "Importer des données climatiques" ),

          # Fichier climat annuel
          div(class = "mb-2 ms-2 sdg-fileinput",
              fileInput(
                "climat_tot_file",
                "Fichier climat (CSV)",
                buttonLabel = "Parcourir",
                placeholder = "",
                accept = c( "text/csv", "text/comma-separated-values,text/plain", ".csv")
              )
          ),
          if (!is.null(input$climat_tot_file$name)) {
            div(
                class = "d-flex align-items-center sdg-file-name-row",
                icon("check-circle", class = "me-2"),
                div(input$climat_tot_file$name))},

          # Choisi le scénario
          div(class = "fw-bold mt-2 mb-1 ms-2 text-secondary", "Scénario CMIP6"),
          div(class = "mb-2 ms-2 sdg-radio-normal",
              radioButtons(
                "ssp",NULL,
                choices = list("SSP245" = "SSP245", "SSP370" = "SSP370"),
                selected = "SSP245"
              )
          ),

          # Bouton
          div(
            class = "d-flex ms-2 me-2 mt-2",
            actionButton(
              "validate_climat_file",
              "Valider le fichier climatique",
              class = "qc-button qc-primary",
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

      # Définir les variables climatiques comme NULL pour indiquer qu'elles ne sont pas utilisées
      rv$climat_tot <- NULL
      rv$max_annees_simulation <- NA

      # Mettre à jour l'état indiquant que le processus est terminé
      rv$extraction_completed <- TRUE

      simulation_ui()

    }
  })

  # Ajouter un nouvel observateur pour la validation du fichier climatique importé
  observeEvent(input$validate_climat_file, {
    # Vérifier que les deux fichiers ont été téléversés
    if (is.null(input$climat_tot_file) ) {
      showNotification(
        "Veuillez téléverser le fichier climatique",
        type = "error",
        duration = 5
      )
      return()
    }

    # Lire le fichier climatique téléversé
    tryCatch({
      # Lire le fichier climat
      climat_tot <- read.csv(input$climat_tot_file$datapath,
                                header = TRUE,
                                sep = ",")

      # Vérifier les fichiers avec les fonctions du package Artemis
      erreurs_tot <- verifier_colonnes_ClimTot(climat_tot)
      erreurs_tot <- c(erreurs_tot, validation_total(data(), climat_tot,input$ssp))

      # Vérifier s'il y a des erreurs
      if (length(erreurs_tot) > 0) {
        showModal(modalDialog(
          div(class = "",
             h3("Erreurs dans le fichier climatique")),
          div( class="overflow-auto",
            style = "max-height: 400px;",

            # Section pour l'afficahge des erreurs du fichier climat
            if (length(erreurs_tot) > 0) {
              div(class = "qc-notice qc-error alert alert-danger mb-3",
                h6(class = "mt-0 mb-2",
                  paste0( "Erreurs dans le fichier climat (",
                    input$climat_tot_file$name,"):"
                  )
                ),
                tags$ul(class = "small ps-3 mb-0",
                  lapply(erreurs_tot, tags$li)
                )
              )
            },
          ),
          # Footer
          footer = tagList(

            div(class = "text-center w-100",
              p(class = "fst-italic mb-2",
                "Veuillez corriger les erreurs et réimporter le fichier."
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
        rv$climat_tot <- climat_tot
        rv$max_annees_simulation <- floor(extraire_nb_annee(climat_tot,AnneeDep=as.numeric(format(Sys.Date(), "%Y")))/10)*10

        # Afficher une notification de succès
        showNotification(
          "Fichier climatique validé et importé avec succès !",
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
        paste("Erreur lors de l'importation du fichier climatique:", e$message),
        type = "error",
        duration = 10
      )
    })
  })


# ------------------- Section configuration de la simulation ------------------
  # Rendre le bouton d'extraction final (option simuler les données climatiques)
  output$extraction_button_final <- renderUI({
    req(input$annee_depart, input$horizon, input$ssp)

    # Cas erreur
    if (input$horizon < 10 || input$horizon %% 10 != 0) {

      div(class = "mt-2 small text-danger",
        icon("exclamation-triangle", class = "me-1"),
        "L'horizon doit être un multiple de 10 d'au moins 10 ans."
      )

    } else {
      # Cas valide
      div(
        class = "mt-3 w-100 justify-content-center",

        div(  class = "small text-muted",

          paste0(
            "Période : ",
            input$annee_depart, " - ",
            input$annee_depart + input$horizon,
            " | Scénario : ",
            ifelse(input$ssp == "SSP245", "SSP245", "SSP370")
          )
        ),
        
        div(
          actionButton(
            "extract_climate",
            "Simuler les données climatiques",
            class = "qc-button qc-primary mt-1 mb-0",
            icon = icon("cloud-download-alt")
          )
        )
      )
    }
  })

  # Action pour l'extraction climatique
  observeEvent(input$extract_climate, {
    # Vérifier les paramètres
    req(input$annee_depart, input$horizon, input$ssp)

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
    ssp <- input$ssp

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
            "), Scénario = ", ssp
          )
        )
      ),

      footer = NULL,
      easyClose = FALSE
    ))

    # Appeler la fonction GenereClimat
    result <- tryCatch({
      GenereClimatRaster(Data= data(),AnneeDep = annee_depart,AnneeFin = annee_fin,  SSP = ssp)
    }, error = function(e) {
      showNotification(paste("Erreur lors de la simulation:", e$message), type = "error", duration = 10)
      return(NULL)
    })

        # Stocker les résultats dans les variables réactives
    if (!is.null(result)) {
      rv$climat_tot <- result
      rv$extraction_horizon <- horizon/10  # Stocker l'horizon utilisé pour l'extraction
    }
    #Si donnée à l'exterieur du raster. Erreur de couverture.
    else{
     #showNotification("ExtractMap a retourné des valeurs nulles pour certains points, vérifiez les coordonnées géographiques", type = "error", duration = 15)
      removeModal()
      # Collapse importation de données
      session$sendCustomMessage(
        type = "collapse_import_open",
        message = list()
      )
      #Réinitialiser les données d'entrées
      rv$data_valid <- FALSE
      rv$extraction_choice_made <- FALSE
      rv$extraction_completed <- FALSE
      rv$climat_tot <- NULL
      rv$max_annees_simulation <- NA
      rv$simulation_terminee <- FALSE

      # Vider les sorties précédentes
      output$extraction_question <- renderUI({})
      output$extraction_button <- renderUI({})
      output$simulation_message <- renderUI({})

      # Afficher les erreurs
      output$error_box_sim <- renderUI({
          div( class = "qc-notice qc-error alert alert-danger d-flex align-items-center",
               icon("circle-xmark", class = "me-2"),
               div(
               tags$strong("Vérifiez les coordonnées géographiques: "),
               "Une ou plusieurs placettes situées à l'extérieur de l'étendue des données climatiques.")
          )
      })
      return()
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
          "Les données climatiques ont été simulées avec succès !"
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
            tags$li(style = "margin-bottom: 1px; padding: 0;", paste0("Scénario climatique : ", input$ssp))
          )
        ),

        # Boutons download
        div(class = "d-flex justify-content-center gap-2 mt-3",

          downloadButton(
            "download_tot",
            "Climat",
            class = "qc-button qc-primary"
          ),
        )
      ),

      # Bouton suivant
      div(class = "mt-3",
          actionButton(
            "close_extraction",
            "Suivant",
            class = "qc-button qc-primary",
            style = "width: 100%; display: block;"
          )),
      footer = NULL,


      easyClose = FALSE,
      backdrop = "static"
    ))
  })

  # Télécharger simulation annuelle
  output$download_tot <- downloadHandler(
    filename = function() {
      paste("climat_total_", input$annee_depart, "_", input$annee_depart + input$horizon - 1, "_", input$ssp, ".csv", sep = "")
    },
    content = function(file) {

      write.table(rv$climat_tot, file, sep = ",", row.names = FALSE)
    }
  )

  # Fermer la boîte de dialogue d'extraction
  observeEvent(input$close_extraction, {
    removeModal()

    # Effacer les paramètres et le bouton d'extraction
    output$extraction_button <- renderUI({})

    simulation_ui()

    # Mettre à jour l'état
    rv$extraction_completed <- TRUE
  })


  # Chargement ui de la section Configuration de la simulation
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
          $('#module_accroissement').prop('disabled', true);
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
          $('#module_mortalite').prop('disabled', true);
        });
      ")),

              # Message utilisateur pour module d'accroissement et de mortalité
              div(class = "small fst-italic text-muted mt-1 pe-2",
                icon("info-circle", class = "me-1"),
                "Les modules d'accroissement et de mortalité sensibles au climat  ",
                "sont désactivés car aucune donnée climatique n'est fournie."
              )
            )
          },

          # Message utilisateur lorsque la maladie corticale du hêtre est activée
          # (masqué en mode sans données climatiques, où le grisage prédomine)
          if (!no_climate_data) {
            conditionalPanel(
              condition = "input.mch == 'Oui'",
              div(class = "small fst-italic text-muted mt-1 pe-2",
                icon("info-circle", class = "me-1"),
                "Le module de mortalité est désactivé par la sélection de la maladie corticale du hêtre."
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
                "Option désactivée car aucune donnée climatique n'est fournie."
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

          actionButton(
            "lancer_simulation",
            "Lancer la simulation",
            class = "qc-button qc-primary",
            style = "width: 100%;",
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
        rv$coupe_init <- FALSE
      }
    } else {
      # Réinitialiser les vecteurs quand la case est décochée
      rv$coupe_on_vector <- NULL
      rv$coupe_modif_vector <- NULL
      rv$coupe_init <- FALSE
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

  # Forcer le module de mortalité à "Original" si maladie corticale du hêtre = Oui
  # (ignoré en mode sans données climatiques : le menu est déjà grisé au rendu)
  observeEvent(list(input$mch, rv$extraction_option), {
    no_climate <- !is.null(rv$extraction_option) && rv$extraction_option == "none"
    if (no_climate) {
      return(invisible(NULL))
    }
    if (identical(input$mch, "Oui")) {
      updateSelectInput(session, "module_mortalite", selected = "original")
      session$sendCustomMessage("toggle_mortalite", list(disable = TRUE))
    } else {
      session$sendCustomMessage("toggle_mortalite", list(disable = FALSE))
    }
  }, ignoreInit = FALSE)

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
  actionButton(class = "qc-button qc-danger",
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

  # Section pour modificateur dans traitement de coupe
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
              "Effacer les défoliations TBE",
              class = "qc-button qc-danger",
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

  observeEvent(input$type_coupe, {

    if (!rv$coupe_init) {
      rv$coupe_init <- TRUE
      return()
    }

    if (input$type_coupe == "NA" && rv$coupe_init == TRUE) {
      showNotification("Impossible d'appliquer une configuration avec 'Aucune coupe' sélectionnée.",
                       type = "warning", duration = 4)
      return()
    }

    showNotification(paste("Coupe appliquée à la décennie", input$decennie_coupe),
                     type = "message", duration = 2)
  })

  # Observateurs pour appliquer les modifications aux vecteurs
  observeEvent( list(input$type_coupe, input$modif_coupe, input$modif_excel_file), {
    req(input$decennie_coupe, input$type_coupe)

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
  },
  ignoreInit = TRUE)

  # Observateur pour TBE
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
        (is.null(rv$climat_tot))) {
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

    # Déterminer le CMIP6 à utiliser
    SSP_value <- ifelse(!is.null(input$ssp),
                        input$ssp,
                        "SSP245")  # Valeur par défaut


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
        ClimTot = rv$climat_tot,
        Tendance = Tendance,
        Residuel = Residuel,
        EvolClim = EvolClim,
        AccModif = AccModif,
        MortModif = MortModif,
        SSP = SSP_value,
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
                  tags$li(style = "margin-bottom: 1px; padding: 0;","Évolution du climat : Non")
                } else {
                  tags$li(style = "margin-bottom: 1px; padding: 0;",paste0("Évolution du climat : ", ifelse(input$evolution_climat == "yes", "Oui", "Non")))
                },

                if (!no_climate_data) {
                  tags$li(style = "margin-bottom: 1px; padding: 0;",paste0("Scénario cmip6 : ", SSP_value))
                }
              )
            ),

            # Bouton suivant pour passer au résultat
            div(class = "mt-3",
              actionButton(
                "close_simulation",
                "Suivant",
                class = "qc-button qc-primary",
                style = "width: 100%; display: block;"
              )
            )
          ),

        footer = NULL,

          easyClose = FALSE,
          backdrop = "static"
        )
      )
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
                  tags$li(style = "margin-bottom: 1px; padding: 0;",paste0("Scénario CMIP6 : ", input$ssp))
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

# --------------Exportation des résultats----------------
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

  # Section du Grade 2
  observeEvent(input$add_grade2, {
    rv$show_grade2 <- TRUE
  })
  output$add_grade2_button <- renderUI({

    if (!rv$show_grade2) {

      div(class = "text-center mt-2 mb-2",
          #div(class = "border border-primary rounded p-1 bg-light text-center mt-2 me-2 mb-2 ",

          actionButton(
            "add_grade2",
            "Ajouter Grade 2",
            class = "qc-button qc-primary qc-button-rounded px-4",
            icon = icon("plus-circle"))
      )
      #)
    }
  })
  observeEvent(input$remove_grade2, {
    rv$show_grade2 <- FALSE
    rv$show_grade3 <- FALSE  # Si on supprime Grade 2, supprimer aussi Grade 3

    # Réinitialiser les valeurs du Grade 2 et 3
    updateTextInput(session, "nom_grade2", value = "")
    updateSelectInput(session, "long_grade2", selected = "Indéfini")
    updateNumericInput(session, "diam_grade2", value = NA)

    updateTextInput(session, "nom_grade3", value = "")
    updateSelectInput(session, "long_grade3", selected = "Indéfini")
    updateNumericInput(session, "diam_grade3", value = NA)
  })
  output$grade2_section <- renderUI({

    if (rv$show_grade2) {
        div(class = "grade-card border rounded p-1 bg-light text-start mt-2 me-2 mb-2 position-relative",

          div(class = "position-absolute top-0 end-0 m-2",

            actionButton(
              "remove_grade2",
              NULL,
              class = "qc-button qc-danger qc-compact qc-button-rounded",
              icon = icon("trash"),
              title = "Supprimer le Grade 2 (et Grade 3 si présent)"
            )
          ),

          div(class = "fw-bold text-body mb-0",
              "Grade 2"
          ),

          div(class = "small text-muted mb-2",
              "(Optionnel)"
          ),

          textInput(
            "nom_grade2",
            "Nom du grade 2:",
            value = "pate"
          ),

          selectInput(
            "long_grade2",
            "Longueur (pieds):",
            choices = c("Indéfini", "4", "8", "12"),
            selected = "4"
          ),

          numericInput(
            "diam_grade2",
            "Diamètre au fin bout (cm):",
            value = 8,
            min = 0,
            max = 100,
            step = 0.1
          )
        )
    }
  })


  # Section du grade 3
  observeEvent(input$add_grade3, {
    if (rv$show_grade2) {  # Affiche seulement si grade 2 existe
      rv$show_grade3 <- TRUE
    }
  })
  output$add_grade3_button <- renderUI({

    if (rv$show_grade2 && !rv$show_grade3) {

      div(class = "text-center mt-2 mb-3",
          actionButton(
            "add_grade3",
            "Ajouter Grade 3",
            class = "qc-button qc-primary qc-button-rounded px-4",
            icon = icon("plus-circle")
          )
      )
    }

  })
  observeEvent(input$remove_grade3, {
    rv$show_grade3 <- FALSE

    # Réinitialiser les valeurs du Grade 3
    updateTextInput(session, "nom_grade3", value = "")
    updateSelectInput(session, "long_grade3", selected = "Indéfini")
    updateNumericInput(session, "diam_grade3", value = NA)
  })
  output$grade3_section <- renderUI({

    if (rv$show_grade3) {

      div(class = "grade-card border rounded p-1 bg-light text-start mt-2 me-2 mb-2 position-relative",

          div(class = "position-absolute top-0 end-0 m-2",

              actionButton(
                "remove_grade3",
                NULL,
                class = "qc-button qc-danger qc-compact qc-button-rounded",
                icon = icon("trash"),
                title = "Supprimer le Grade 3"
              )
          ),

          div(class = "fw-bold text-body mb-0",
              "Grade 3"
          ),

          div(class = "small text-muted mb-2",
              "(Optionnel)"
          ),

          textInput(
            "nom_grade3",
            "Nom du grade 3:",
            value = ""
          ),

          selectInput(
            "long_grade3",
            "Longueur (pieds):",
            choices = c( "Indéfini", "4", "8", "12"),
            selected = "Indéfini"
          ),

          numericInput(
            "diam_grade3",
            "Diamètre au fin bout (cm):",
            value = NA,
            min = 0,
            max = 100,
            step = 0.1
          )
      )
    }
  })

  # Download résultat
  output$download_resultats_custom <- downloadHandler(
    # Télécharger les résultats
    filename = function() {
      if(!(input$Sortie == "echelle_billon")){

        paste("resultats_simulation_artemis_sortie_",input$Sortie,"_",Sys.Date(), ".csv", sep = "")

      }
      else{
        paste("resultats_simulation_artemis_sortie_",input$Sortie,"_",input$typeBillonnage,"_",Sys.Date(), ".csv", sep = "")
      }
    },
    content = function(file) {

      if(input$Sortie == "echelle_billon"){
        req(input$dhs_input, input$typeBillonnage, rv$resultats_simulation)

        # Validation
        if (input$nom_grade1 == "" || is.na(input$diam_grade1)) {

          showNotification(
            "Grade 1 : Le nom et le diamètre sont obligatoires",
            type = "error",
            duration = 5
          )
          return()
        }

        if (isTRUE(rv$show_grade2) &&
            (input$nom_grade2 == "" || is.na(input$diam_grade2))) {

          showNotification(
            "Grade 2 : Le nom et le diamètre sont obligatoires",
            type = "error",
            duration = 5
          )
          return()
        }

        if (isTRUE(rv$show_grade3) &&
            (input$nom_grade3 == "" || is.na(input$diam_grade3))) {

          showNotification(
            "Grade 3 : Le nom et le diamètre sont obligatoires",
            type = "error",
            duration = 5
          )
          return()
        }


        withProgress(message = "Calcul du billonnage en cours...", value = 0, {

          incProgress(0.1, detail = "Validation des paramètres...")

          dhs_val <- as.numeric(input$dhs_input)
          simplifier_val <- input$simplifier

          suppressWarnings({

            incProgress(0.2, detail = "Traitement des longueurs...")

            long_grade1_val <- if (is.null(input$long_grade1) || input$long_grade1 == "Indéfini") {
              NA_real_
            } else {
              as.numeric(input$long_grade1)
            }

            long_grade2_val <- NA_real_
            if (isTRUE(rv$show_grade2) && !is.null(input$long_grade2)) {

              long_grade2_val <- if (input$long_grade2 =="Indéfini") {
                NA_real_
              } else {
                as.numeric(input$long_grade2)
              }
            }

            long_grade3_val <- NA_real_
            if (isTRUE(rv$show_grade3) && !is.null(input$long_grade3)) {

              long_grade3_val <- if (input$long_grade3 == "Indéfini") {
                NA_real_
              } else {
                as.numeric(input$long_grade3)
              }
            }

            incProgress(0.3, detail = "Traitement des diamètres...")

            diam_grade1_val <- if (is.null(input$diam_grade1) || is.na(input$diam_grade1)) {
              NA_real_
            } else {
              as.numeric(input$diam_grade1)
            }

            diam_grade2_val <- if (isTRUE(rv$show_grade2) &&
                                   !is.null(input$diam_grade2) &&
                                   !is.na(input$diam_grade2)) {
              as.numeric(input$diam_grade2)
            } else {
              NA_real_
            }

            diam_grade3_val <- if (isTRUE(rv$show_grade3) &&
                                   !is.null(input$diam_grade3) &&
                                   !is.na(input$diam_grade3)) {
              as.numeric(input$diam_grade3)
            } else {
              NA_real_
            }

            incProgress(0.4, detail = "Préparation des noms de grades...")

            nom_grade1_val <- as.character(input$nom_grade1)

            nom_grade2_val <- if (isTRUE(rv$show_grade2) &&
                                  !is.null(input$nom_grade2) &&
                                  input$nom_grade2 != "") {
              as.character(input$nom_grade2)
            } else {
              NA_character_
            }

            nom_grade3_val <- if (isTRUE(rv$show_grade3) &&
                                  !is.null(input$nom_grade3) &&
                                  input$nom_grade3 != "") {
              as.character(input$nom_grade3)
            } else {
              NA_character_
            }
          })

          incProgress(0.5, detail = "Exécution du calcul...")

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
              diam_grade3 = diam_grade3_val,
              Simplifier = simplifier_val
            )

            rv$processed_Simul <- rv$processed_Billonage

            incProgress(1, detail = "Terminé")

            showNotification(
              "Billonnage simulé avec succès!",
              type = "message",
              duration = 3
            )

          }, error = function(e) {

            showNotification(
              paste("Erreur:", e$message),
              type = "error",
              duration = 5
            )

            rv$processed_Billonage <- NULL
            rv$processed_Simul <- NULL
          })
        })
      }

      # Utiliser le dataframe résultant de la simulation
      if (!is.null(rv$processed_Simul)) {
        write.table(rv$processed_Simul, file, sep = ";", row.names = FALSE)
      } else {
        # Créer un fichier vide ou avec un message d'erreur si aucun résultat n'est disponible
        write.csv(data.frame(Erreur = "Aucun résultat de simulation disponible"), file, row.names = FALSE)
      }
    }
  )

# ------------ Bouton reset -------------------
  # Gestion de la réinitialisation
  observeEvent(input$reset_button, {

    showModal(modalDialog(
        title = "Confirmation de réinitialisation",

        div(class = "text-center",

          # Message principal
          p(class = "fw-bold mb-2",
            "Êtes-vous sûr de vouloir réinitialiser l'application?"
          ),

          # Message secondaire
          p(class = "small text-muted mb-2",
            "Toutes les données et simulations actuelles seront perdues."
          ),

          # Avertissement
          p(class = "text-danger fw-semibold",
            "Cette action est irréversible."
          )
        ,


        div(class = "d-flex justify-content-center gap-3",

          tags$button(
            type = "button",
            class = "qc-button qc-tertiary",
            `data-bs-dismiss` = "modal",
            "Annuler"
          ),
          actionButton(
            "confirm_reset",
            "Réinitialiser",
            class = "qc-button qc-danger",
            style = "min-width: 170px; white-space: nowrap;"
          )
        )
    ),

        footer = NULL,
        easyClose = FALSE
      )
    )
  })

  # Réinitialisation
  observeEvent(input$confirm_reset, {
    rv$simulation_terminee <- FALSE
    # Signaler qu'il s'agit d'un reset pour que onSessionEnded ne quitte pas R
    .artemis_reset_en_cours <<- TRUE
    session$reload()
  })




}

# ------------------------ App -----------------------
# Lancer l'application
shinyApp(ui = ui, server = server)
