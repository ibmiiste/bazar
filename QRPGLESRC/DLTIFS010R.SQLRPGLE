**FREE
//******************************************************************************
// MODULE    : DLTIFS010R                         DATE CREATION :  12/04/2019
// AUTEUR    : O.DRUBIGNY              CC BY SA Olivier DRUBIGNY
// Ce(tte) œuvre est mise à disposition selon le s termes de la Licence
// Creative Commons Attribution - Partage dans les Mêmes Conditions 4.0
// International.
//------------------------------------------------------------------------------
// DESCRIPTION :   Suppression de répertoire/fichier de l'ifs récursif ou non
//                 Grâce à l'utilisation de QSH
//******************************************************************************
// Option de compilation et de programme
/Copy Qcpysrc,Cpyctlstm


// External subprocedures prototypes globales
// prototype sous-procédures externes
/Copy Qcpysrc,Utiproc


// Program status data structure
// Data Structure d'information programme
/copy qcpysrc,psds


// Définition de l'Interface de la procédure
// Paramètres d'appel
// Procedure-Interface definition
// --------------------------------------------------
// Procedure name: Suppression_ifs
// Purpose:        Supprimer un fichier ou un répertoire de l'ifs
//                 de façon récursive ou non
// Returns:
// Parameter:      p_chemin => Contient le chemin de l'objet à supprim...
//                          er dans l'ifs
//                 p_Jours  => Nombre de jours
//                 p_Recursivite => Récursif ou non
//                 p_Filtre => Filtre
//                 p_Type_date => date à comparer par rapport à p_Jours
// --------------------------------------------------

Dcl-proc Suppression_ifs export;


  dcl-pi Suppression_ifs ind;
    p_chemin varchar(2000) const;
    P_Recursivite ind const options(*nopass:*omit);
    p_Filtre varchar(128) const options(*nopass:*omit);
    p_Type_date char(1) const options(*nopass:*omit);
    p_Jours Int(5) const options(*nopass:*omit);
  end-pi;


  // Variables locales / local variables
  dcl-s l_Retour ind;
  dcl-s l_Recursivite ind inz(*off);
  dcl-s l_Filtre char(128);
  dcl-s l_type_date char(1);
  dcl-s l_commande_qsh char(512);
  Dcl-s l_Jours Int(5);

  // constants
  // constantes
  // dcl-c constante1 'CONSTANTE';

  monitor;
    l_Retour = *OFF;
    //---------------------------------------------------------
    // Méchanisme optionnelde protection, autorisation uniquement dans le
    // répertoire /home afin d'éviter de supprimer des objets systèmes
    //---------------------------------------------------------

    if (%subst(p_chemin:1:6) <> '/home/');
      return *ON;
    endif;

    //----------------------------------------------------------
    // Initialisation des variables avec les paramètres si applicable
    //----------------------------------------------------------

    if %parms > 1;
      if %addr(p_Recursivite) <> *NULL;
        l_Recursivite = p_Recursivite;
      endif;
    endif;

    if %parms > 2;
      if %addr(p_Filtre) <> *NULL;
        l_Filtre = %trim(p_Filtre);
      endif;
    endif;

    if %parms > 3;
      if %addr(p_Type_date) <> *NULL;
        l_type_date = p_Type_date;
        l_Jours=p_Jours;
      endif;
    endif;

    //-------------------------------------------------------------
    // Départ de la construction de la chaîne de commande QSH

    l_commande_qsh = 'STRQSH CMD(''find '
                      + %trim(p_chemin)
                      + ' -type f ';

    //-------------------------------------------------------------
    // Voie sécurisée, par défaut non récursif
    // -prune empêche de descendre dans les sous-répertoires
    if not l_Recursivite;
      l_commande_qsh = %trim(l_commande_qsh)
                  + ' -path '''''
                  + %trim(p_chemin)
                  + '/*'''''
                  + ' -prune';
    endif;

    //-------------------------------------------------------------
    // Plus sécurisé, on filtre sur un modèle de nom

    if l_Filtre <> *BLANKS;
      l_commande_qsh = %trim(l_commande_qsh)
                  + ' -name '''''
                  + %trim(l_Filtre)
                  + '''''';
    endif;

    //-------------------------------------------------------------
    // Time: M, C or A = -mtime, -ctime, -atime
    // M : Date de modification
    // C : Date de création
    // A : Date de dernier accès
    if l_type_date<>*Blank;
      l_type_date = %xlate('MCA':'mca':l_type_date);
      l_commande_qsh = %trim(l_commande_qsh) + ' -'
                       + l_type_date + 'time';

      //-------------------------------------------------------------
      // +/- Nombre de jours

      if (l_Jours >= 0);
        l_commande_qsh = %trim(l_commande_qsh) + ' +';
      else;
        l_commande_qsh = %trim(l_commande_qsh) + ' -';
      endif;

      l_commande_qsh = %trim(l_commande_qsh)
                             + %trim(%editc(%abs(p_Jours):'J'));
    Endif;
    //-------------------------------------------------------------
    // -exec rm = Remove en QSH

    l_commande_qsh = %trim(l_commande_qsh)
                           + ' -exec rm {} \;'')';

    //-------------------------------------------------------------
   Cmd_exec(%trim(l_commande_qsh));

  on-error;
    l_Retour = *ON;
    dump(a);
    gest_erreur();
  endmon;

  return l_Retour;

END-PROC ;
