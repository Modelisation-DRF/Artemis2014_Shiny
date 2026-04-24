' ============================================================================
' Lanceur SaMARE - Application Shiny
' ============================================================================
' Ce script lance l'application SaMARE
' Affiche une fenetre d'information pendant le demarrage
' ============================================================================

Set objShell = CreateObject("WScript.Shell")
Set objFSO = CreateObject("Scripting.FileSystemObject")

' Obtenir le repertoire du script
strCurrentDir = objFSO.GetParentFolderName(WScript.ScriptFullName)
strBatPath = strCurrentDir & "\_launcher.bat"

' Verifier que le fichier batch existe
If Not objFSO.FileExists(strBatPath) Then
    MsgBox "Erreur: Le fichier _launcher.bat est introuvable." & vbCrLf & _
           "Chemin attendu: " & strBatPath, vbCritical, "SaMARE - Erreur"
    WScript.Quit 1
End If

' Se placer dans le bon repertoire
objShell.CurrentDirectory = strCurrentDir

' Lancer le batch (7 = fenetre minimisee)
objShell.Run """" & strBatPath & """", 7, False

' Afficher une fenetre d'information avec timeout de 15 secondes
' La fenetre se ferme automatiquement apres 15 sec OU si l'utilisateur clique OK
strMessage = "Lancement de SaMARE en cours..." & vbCrLf & vbCrLf & _
             "Veuillez patienter quelques instants." & vbCrLf & vbCrLf & _
             "Le systeme effectue les operations suivantes :" & vbCrLf & _
             "   - Verification des packages R requis" & vbCrLf & _
             "   - Chargement des modules de simulation" & vbCrLf & _
             "   - Demarrage du serveur Shiny" & vbCrLf & vbCrLf & _
             "L'application s'ouvrira automatiquement" & vbCrLf & _
             "dans votre navigateur web." & vbCrLf & vbCrLf & _
             "(Cette fenetre se fermera automatiquement dans 15 secondes)"

' Popup(message, secondes, titre, type)
' Type 64 = Icone information
objShell.Popup strMessage, 15, "SaMARE - Demarrage", 64

' Nettoyage
Set objFSO = Nothing
Set objShell = Nothing
