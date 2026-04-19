>>SOURCE FORMAT FREE

*> Definition du programme

IDENTIFICATION DIVISION.
PROGRAM-ID. BANQUE.
AUTHOR. Neo.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    *> Fichier principal contenant les comptes
    SELECT COMPTE ASSIGN TO "Compte.txt"
        FILE STATUS IS FS-COMPTE
        ORGANIZATION IS LINE SEQUENTIAL.

    *> Fichier temporaire utilisé pour réécrire les comptes modifiés
    SELECT TEMP-FILE ASSIGN TO "Temp.txt"
        FILE STATUS IS FS-TEMP
        ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.

*> Description d’un enregistrement du fichier COMPTE
FD COMPTE.
01 ENREGISTREMENT-COMPTE        PIC X(100).

*> Description d’un enregistrement du fichier temporaire
FD TEMP-FILE.
01 TEMP-ENREGISTREMENT          PIC X(100).

WORKING-STORAGE SECTION.

*> Choix du menu principal
01 CHOICE                       PIC X VALUE "0".

*> Indicateur pour quitter le programme
01 QUIT                         PIC X VALUE "N".

*> Variables de saisie utilisateur
01 PRENOM                       PIC X(20).
01 NOM                          PIC X(20).

*> Solde initial par défaut d’un nouveau compte
01 SOLDE                        PIC 9(7)V99 VALUE 1000.00.

*> Montant saisi pour dépôt ou retrait
01 MONTANT                      PIC 9(7)V99 VALUE 0.

*> Ligne lue depuis le fichier
01 LIGNE                        PIC X(100).

*> Indicateur de fin de fichier
01 EOF-FLAG                     PIC X VALUE "N".

*> Indicateur permettant de savoir si le compte recherché a été trouvé
01 TROUVE                       PIC X VALUE "N".

*> Champs extraits après découpage d’une ligne du fichier
01 F-PRENOM                     PIC X(20).
01 F-NOM                        PIC X(20).
01 F-SOLDE-TXT                  PIC X(20).
01 F-SOLDE                      PIC 9(7)V99 VALUE 0.

*> Variable d’affichage formatée du solde
01 SOLDE-AFF                    PIC Z(7)9.99.

*> Ligne reconstruite après modification d’un compte
01 NOUVELLE-LIGNE               PIC X(100).

*> Codes retour des opérations sur fichiers
01 FS-COMPTE                    PIC XX VALUE SPACES.
01 FS-TEMP                      PIC XX VALUE SPACES.

PROCEDURE DIVISION.
    PERFORM INITIALISER-FICHIERS

    *> Boucle principale du programme :
    *> continue tant que l'utilisateur ne choisit pas de quitter
    PERFORM UNTIL QUIT = "Y"

        *> Affichage du menu principal
        DISPLAY " BIENVENUE DANS LE SYSTEME BANCAIRE"
        DISPLAY "1. Afficher les comptes"
        DISPLAY "2. Creer un compte"
        DISPLAY "3. Deposer de l'argent"
        DISPLAY "4. Retirer de l'argent"
        DISPLAY "5. Quitter"
        DISPLAY "Veuillez saisir votre choix : "

        *> Lecture du choix utilisateur
        ACCEPT CHOICE

        *> Traitement du choix
        EVALUATE CHOICE
            WHEN "1"
                PERFORM AFFICHER-COMPTE
            WHEN "2"
                PERFORM CREER-COMPTE
            WHEN "3"
                PERFORM DEPOSER-ARGENT
            WHEN "4"
                PERFORM RETIRER-ARGENT
            WHEN "5"
                MOVE "Y" TO QUIT
                DISPLAY "Au revoir."
            WHEN OTHER
                DISPLAY "CHOIX INVALIDE"
        END-EVALUATE
    END-PERFORM

    STOP RUN.

*> Vérifie que les fichiers nécessaires existent,
*> puis les crée s'ils sont absents

INITIALISER-FICHIERS.
    PERFORM ASSURER-EXISTENCE-COMPTE
    PERFORM ASSURER-EXISTENCE-TEMP.

ASSURER-EXISTENCE-COMPTE.
    MOVE SPACES TO FS-COMPTE
    OPEN INPUT COMPTE

    EVALUATE FS-COMPTE
        WHEN "00"
            CLOSE COMPTE
        WHEN "35"
            OPEN OUTPUT COMPTE
            CLOSE COMPTE
            DISPLAY "Fichier cree."
        WHEN OTHER
            DISPLAY "Erreur: " FS-COMPTE
    END-EVALUATE.

ASSURER-EXISTENCE-TEMP.
    MOVE SPACES TO FS-TEMP
    OPEN INPUT TEMP-FILE

    EVALUATE FS-TEMP
        WHEN "00"
            CLOSE TEMP-FILE
        WHEN "35"
            OPEN OUTPUT TEMP-FILE
            CLOSE TEMP-FILE
            DISPLAY "Fichier cree."
        WHEN OTHER
            DISPLAY "Erreur: " FS-TEMP
    END-EVALUATE.

*> Afficher tous les comptes présents dans le fichier

AFFICHER-COMPTE.
    *> Réinitialisation du flag de fin de fichier
    MOVE "N" TO EOF-FLAG

    *> Ouverture du fichier en lecture
    OPEN INPUT COMPTE

    DISPLAY "LISTE DES COMPTES"

    *> Lecture de toutes les lignes du fichier
    PERFORM UNTIL EOF-FLAG = "Y"
        READ COMPTE
            AT END
                *> Si fin de fichier, on arrête la boucle
                MOVE "Y" TO EOF-FLAG
            NOT AT END
                *> On copie la ligne lue dans une variable de travail
                MOVE ENREGISTREMENT-COMPTE TO LIGNE

                *> On découpe la ligne en prénom, nom et solde
                PERFORM PARSER-LIGNE

                *> Préparation du solde pour affichage
                MOVE F-SOLDE TO SOLDE-AFF

                *> Affichage des informations du compte
                DISPLAY "Prenom : " FUNCTION TRIM(F-PRENOM)
                        " | Nom : " FUNCTION TRIM(F-NOM)
                        " | Solde : " SOLDE-AFF
        END-READ
    END-PERFORM

    *> Fermeture du fichier
    CLOSE COMPTE.

*> Créer un nouveau compte avec un solde initial de 1000.00

CREER-COMPTE.
    *> Saisie du prénom
    DISPLAY "Prenom : "
    ACCEPT PRENOM

    *> Saisie du nom
    DISPLAY "Nom : "
    ACCEPT NOM

    *> Nettoyage de la zone de travail
    MOVE SPACES TO LIGNE

    *> Construction de la ligne au format :
    *> Prenom;Nom;1000.00
    STRING FUNCTION TRIM(PRENOM)
           ";"
           FUNCTION TRIM(NOM)
           ";"
           "1000.00"
    INTO LIGNE
    END-STRING

    *> Ouverture du fichier en ajout
    OPEN EXTEND COMPTE

    *> Écriture du nouveau compte
    WRITE ENREGISTREMENT-COMPTE FROM LIGNE

    *> Fermeture du fichier
    CLOSE COMPTE

    DISPLAY "Compte cree avec succes avec 1000.00 $".

*> Déposer de l'argent sur un compte existant

DEPOSER-ARGENT.
    *> Saisie de l’identité du compte
    DISPLAY "Prenom du compte : "
    ACCEPT PRENOM

    DISPLAY "Nom du compte : "
    ACCEPT NOM

    *> Saisie du montant à déposer
    DISPLAY "Montant a deposer : "
    ACCEPT MONTANT

    *> Réinitialisation des indicateurs
    MOVE "N" TO EOF-FLAG
    MOVE "N" TO TROUVE

    *> Ouverture du fichier principal en lecture
    OPEN INPUT COMPTE

    *> Ouverture du fichier temporaire en écriture
    OPEN OUTPUT TEMP-FILE

    *> Lecture de tous les comptes
    PERFORM UNTIL EOF-FLAG = "Y"
        READ COMPTE
            AT END
                MOVE "Y" TO EOF-FLAG
            NOT AT END
                *> Copie de la ligne lue
                MOVE ENREGISTREMENT-COMPTE TO LIGNE

                *> Extraction des champs
                PERFORM PARSER-LIGNE

                *> Vérifie si c’est le bon compte
                IF FUNCTION TRIM(F-PRENOM) = FUNCTION TRIM(PRENOM)
                   AND FUNCTION TRIM(F-NOM) = FUNCTION TRIM(NOM)

                    *> Ajoute le montant au solde
                    ADD MONTANT TO F-SOLDE

                    *> Marque le compte comme trouvé
                    MOVE "Y" TO TROUVE

                    *> Réécrit la ligne mise à jour dans le fichier temporaire
                    PERFORM ECRIRE-LIGNE-MAJ
                ELSE
                    *> Sinon, copie la ligne telle quelle
                    WRITE TEMP-ENREGISTREMENT FROM ENREGISTREMENT-COMPTE
                END-IF
        END-READ
    END-PERFORM

    *> Fermeture des deux fichiers
    CLOSE COMPTE
    CLOSE TEMP-FILE

    *> Si le compte a été trouvé, on remplace l’ancien fichier
    IF TROUVE = "Y"
        PERFORM REMPLACER-FICHIER
        DISPLAY "Depot effectue."
    ELSE
        DISPLAY "Compte introuvable."
    END-IF.

*> Retirer de l'argent d’un compte existant

RETIRER-ARGENT.
    *> Saisie de l’identité du compte
    DISPLAY "Prenom du compte : "
    ACCEPT PRENOM

    DISPLAY "Nom du compte : "
    ACCEPT NOM

    *> Saisie du montant à retirer
    DISPLAY "Montant a retirer : "
    ACCEPT MONTANT

    *> Réinitialisation des indicateurs
    MOVE "N" TO EOF-FLAG
    MOVE "N" TO TROUVE

    *> Ouverture du fichier principal en lecture
    OPEN INPUT COMPTE

    *> Ouverture du fichier temporaire en écriture
    OPEN OUTPUT TEMP-FILE

    *> Lecture de tous les comptes
    PERFORM UNTIL EOF-FLAG = "Y"
        READ COMPTE
            AT END
                MOVE "Y" TO EOF-FLAG
            NOT AT END
                *> Copie de la ligne en zone de travail
                MOVE ENREGISTREMENT-COMPTE TO LIGNE

                *> Découpage de la ligne
                PERFORM PARSER-LIGNE

                *> Vérifie si c’est le bon compte
                IF FUNCTION TRIM(F-PRENOM) = FUNCTION TRIM(PRENOM)
                   AND FUNCTION TRIM(F-NOM) = FUNCTION TRIM(NOM)

                    *> Vérifie que le solde est suffisant
                    IF MONTANT > F-SOLDE
                        DISPLAY "Retrait impossible : solde insuffisant."

                        *> On considère le compte trouvé même si le retrait échoue
                        MOVE "Y" TO TROUVE

                        *> On recopie la ligne inchangée
                        WRITE TEMP-ENREGISTREMENT FROM ENREGISTREMENT-COMPTE
                    ELSE
                        *> Soustraction du montant demandé
                        SUBTRACT MONTANT FROM F-SOLDE

                        *> Marque le compte comme trouvé
                        MOVE "Y" TO TROUVE

                        *> Réécriture du compte modifié
                        PERFORM ECRIRE-LIGNE-MAJ
                    END-IF
                ELSE
                    *> Si ce n’est pas le bon compte, on recopie sans changer
                    WRITE TEMP-ENREGISTREMENT FROM ENREGISTREMENT-COMPTE
                END-IF
        END-READ
    END-PERFORM

    *> Fermeture des fichiers
    CLOSE COMPTE
    CLOSE TEMP-FILE

    *> Si trouvé, remplacement du fichier principal
    IF TROUVE = "Y"
        PERFORM REMPLACER-FICHIER
        DISPLAY "Retrait effectue."
    ELSE
        DISPLAY "Compte introuvable."
    END-IF.

*> Découper une ligne du fichier en prénom / nom / solde
*> Format attendu : Prenom;Nom;Solde

PARSER-LIGNE.
    *> Réinitialisation des champs
    MOVE SPACES TO F-PRENOM
    MOVE SPACES TO F-NOM
    MOVE SPACES TO F-SOLDE-TXT
    MOVE 0 TO F-SOLDE

    *> Découpage de la ligne selon le séparateur ;
    UNSTRING LIGNE
        DELIMITED BY ";"
        INTO F-PRENOM
             F-NOM
             F-SOLDE-TXT
    END-UNSTRING

    *> Conversion du solde texte en valeur numérique
    MOVE FUNCTION NUMVAL(F-SOLDE-TXT) TO F-SOLDE.

*> Reconstruire une ligne mise à jour puis l’écrire
*> dans le fichier temporaire

ECRIRE-LIGNE-MAJ.
    *> Nettoyage de la ligne de sortie
    MOVE SPACES TO NOUVELLE-LIGNE

    *> Mise en forme du solde pour affichage/écriture
    MOVE F-SOLDE TO SOLDE-AFF

    *> Reconstruction au format :
    *> Prenom;Nom;Solde
    STRING FUNCTION TRIM(F-PRENOM)
           ";"
           FUNCTION TRIM(F-NOM)
           ";"
           FUNCTION TRIM(SOLDE-AFF)
    INTO NOUVELLE-LIGNE
    END-STRING

    *> Écriture dans le fichier temporaire
    WRITE TEMP-ENREGISTREMENT FROM NOUVELLE-LIGNE.

*> Remplacer le fichier principal par le fichier temporaire

REMPLACER-FICHIER.
    *> Commande système pour copier Temp.txt vers Compte.txt
    CALL "SYSTEM" USING "cp Temp.txt Compte.txt".
