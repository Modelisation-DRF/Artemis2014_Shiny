@echo off
title Artemis - Demarrage
cd /d "%~dp0"

REM ============================================================================
REM Lanceur Artemis - Recherche automatique de R
REM ============================================================================

set "R_PATH="
set "RSCRIPT="

REM --- Recherche dans les emplacements courants ---

REM 1. C:\Logiciels (MRNF)
for /d %%i in (C:\Logiciels\R-*) do set "R_PATH=%%i"

REM 2. C:\Program Files\R
if "%R_PATH%"=="" (
    for /d %%i in ("C:\Program Files\R\R-*") do set "R_PATH=%%i"
)

REM 3. C:\Program Files (x86)\R (rare, mais possible)
if "%R_PATH%"=="" (
    for /d %%i in ("C:\Program Files (x86)\R\R-*") do set "R_PATH=%%i"
)

REM 4. R_HOME si défini dans l'environnement
if "%R_PATH%"=="" (
    if defined R_HOME (
        set "R_PATH=%R_HOME%"
    )
)

REM --- Vérification ---
if "%R_PATH%"=="" (
    echo.
    echo ============================================================
    echo ERREUR: R n'a pas ete trouve sur ce systeme.
    echo.
    echo Emplacements recherches:
    echo   - C:\Logiciels\R-*
    echo   - C:\Program Files\R\R-*
    echo   - Variable d'environnement R_HOME
    echo.
    echo Veuillez installer R ou verifier son emplacement.
    echo ============================================================
    echo.
    pause
    exit /b 1
)

REM --- Construction du chemin Rscript ---
set "RSCRIPT=%R_PATH%\bin\Rscript.exe"

if not exist "%RSCRIPT%" (
    echo.
    echo ERREUR: Rscript.exe introuvable dans %R_PATH%\bin\
    echo.
    pause
    exit /b 1
)

REM --- Lancement de l'application ---
echo.
echo Demarrage d'Artemis...
echo R trouve: %R_PATH%
echo.

"%RSCRIPT%" "runShinyApp.R"

REM --- Gestion des erreurs de sortie ---
if %ERRORLEVEL% NEQ 0 (
    echo.
    echo L'application s'est terminee avec le code: %ERRORLEVEL%
    echo.
    pause
)
